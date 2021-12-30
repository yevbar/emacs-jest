;;; emacs-jest.el --- Jest testing framework in GNU Emacs                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yev Barkalov

;; Author: Yev Barkalov <yev@yev.bar>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a way to use Jest in emacs.
;; Note, this refers to the JavaScript testing framework <https://jestjs.io/>

;;; Code:

;; code goes here
(require 'noflet)
(require 'compile)
(require 'xml)
(require 'dom)
(require 'highlight)
(require 'dash)

;;; Custom vars
(defgroup jest nil
  "Tools for running jest tests"
  :group 'tools)

(defcustom jest-environment-vars nil
  "Environment variables that get applied to all jest calls"
  :type 'string
  :group 'jest)

(defcustom jest-default-args nil
  "Arguments that get applied to all jest calls"
  :type 'string
  :group 'jest)

(defcustom jest-stop-after-first-failure nil
  "Stop any jest call after first failure"
  :type 'boolean
  :group 'jest)

(defcustom jest-stop-coverage-after-first-failure nil
  "Stop a jest coverage call after first failure"
  :type 'boolean
  :group 'jest)

;; TODO - save historical results as custom config
;; TODO - if ^ then make chart to show jest coverage change over time (per branch etc)
;; TODO - take coverage results and highlight lines in file(s)

;;; General utils
(defvar node-error-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\(\\(?:[a-zA-Z]:\\)?[a-zA-Z\.0-9_/\\-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar node-error-regexp-alist
  `((,node-error-regexp 1 2 3)))

;; Takes a buffer name and kills if exists
;; Also takes optional boolean to raise error if buffer exists instead of killing it
(defun check-buffer-does-not-exist (buffer-name &optional error-if-exists)
  (let ((buffer-exists (get-buffer buffer-name)))
    (cond
     ((and buffer-exists error-if-exists)
      (error (concat "Buffer " buffer-name " already exists")))
     (buffer-exists
      (kill-buffer buffer-name)))))

;; Replaces in string
;; Graciously taken from https://stackoverflow.com/a/17325791/16587811
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

;; If whole number, returns as is
;; Otherwise formats up to two decimal places
(defun format-decimal (value)
  (cond
   ((eq 0 value)
    "0%")
   (t
    (concat (format "%.4g" value) "%"))))

(defun get-percentage (portion total)
  (cond
   ((eq 0 portion)
    (format-decimal 0))
   ((eq 0 total)
    (format-decimal 0))
   ((eq portion total)
    (format-decimal 100))
   (t
    (format-decimal (* 100 (/ (float portion) total))))))

(defun last-character (str)
  (substring str -1 nil))

(defun is-percentage (str)
  (string-equal (last-character str) "%"))

(defun extract-percentage (percentage-string)
  (string-to-number (substring percentage-string 0 -1)))

;;; Related to the compilation buffer
(defun jest-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode jest-compilation-mode "Jest"
  "Jest compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist) node-error-regexp-alist)
    (set (make-local-variable 'compilation-finish-functions) 'jest-after-completion-hook)
    (add-hook 'compilation-filter-hook 'jest-compilation-filter nil t)
    ))

;; TODO - change this to a list or smth so people can add their own custom extensions/modifications
;; (append-to-list 'jest-after-completion-hook ...)
(defun jest-after-completion-hook (&rest args))

;;; Related to actually running jest
(defun get-jest-executable ()
  (let ((project-provided-jest-path (concat default-directory "node_modules/.bin/jest")))
    (cond
     ;; Check to see if an executable exists within the `default-directory` value
     ((file-exists-p project-provided-jest-path) project-provided-jest-path)
     ;; Check to see if there's a global jest executable
     ((executable-find "jest") "jest")
     ;; Otherwise we throw an error
     (t (error "Failed to find jest executable")))))

(defun should-bail-after-first-failure (&optional is-coverage)
  (or jest-stop-after-first-failure
   (and is-coverage jest-stop-coverage-after-first-failure)))

(defun with-coverage-args (&optional arguments)
  (let* ((minimum-coverage-args (list "--coverage"))
	 (coverage-args (if (should-bail-after-first-failure t) (append minimum-coverage-args (list "--bail")) minimum-coverage-args)))
    (if arguments
	(append arguments coverage-args)
      coverage-args)))

(defun get-jest-arguments (&optional arguments)
  (if arguments
      (string-join
       ;; TODO - Figure out what to do about duplicates/overwrites
       (flatten-list
	(list
	 jest-environment-vars
	 arguments
	 jest-default-args
	 (when (should-bail-after-first-failure) "--bail"))) " ")
    ""))

;; Takes optional list of tuples and applies them to jest command
(defun generate-jest-command (&optional arguments)
  (let ((jest-executable (get-jest-executable))
	(jest-arguments (get-jest-arguments arguments)))
    (string-join `(,jest-executable ,jest-arguments) " ")))

(defun run-jest-command (&optional arguments)
  ;; Check there are no unsaved buffers
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))

  ;; Kill previous test buffer if exists
  (check-buffer-does-not-exist "*jest tests*")

  ;; Storing `target-directory` since this changes when
  ;; we change window if there's more than one buffer
  (let ((target-directory (projectile-project-root)))
    (unless (eq 1 (length (window-list)))
      (select-window (previous-window)))

    ;; Create new buffer and run command
    (with-current-buffer (get-buffer-create "*jest tests*")
      (switch-to-buffer "*jest tests*")
      (let ((default-directory target-directory) (compilation-scroll-output t))
	(compilation-start
	 (generate-jest-command arguments)
	 'jest-compilation-mode
	 (lambda (m) (buffer-name)))))))

(defun jest-test-file (&optional filename)
  (interactive)
  (cond
   ((not filename)
    (let ((helm-projectile-sources-list
	   '(helm-source-projectile-buffers-list
	     helm-source-projectile-files-list)))
      (noflet ((helm-find-file-or-marked (candidate) (jest-test-file candidate))
	       (helm-buffer-switch-buffers (candidate) (jest-test-file (buffer-file-name candidate))))
	(helm-projectile))))
   ((file-exists-p filename)
    (run-jest-command `(,filename)))
   (t
    (error "Invalid file provided"))))

(defun jest-test-current-file ()
  (interactive)
  (jest-test-file buffer-file-name))

(defun jest-test-directory (&optional directory)
  (interactive)
  (unless directory (setq directory (read-directory-name "Test directory:")))
  (run-jest-command `(,directory)))

(defun jest-test-current-directory ()
  (interactive)
  (jest-test-directory default-directory))

(defun jest-test-coverage ()
  (interactive)
  (run-jest-command (with-coverage-args)))

(defun present-coverage-as-org-table (columns table)
  (insert (concat "|" (string-join columns "|") "|"))
  (newline)

  (insert "|-")
  (newline)

  (mapc
   (lambda (row)
     (progn
       (insert "|")
       (insert (string-join row "|"))
       (insert "|")
       (newline)))
   table)

  ;; Deleting the last (newline) call
  (backward-delete-char-untabify 1)

  (org-mode)
  (org-table-align)
  (add-coverage-table-color-indicators)

  ;; Adding hook so highlights can be re-introduced even after sorting column
  (add-hook 'org-ctrl-c-ctrl-c-hook 'add-coverage-table-color-indicators)

  ;; Moving to start of file
  (beginning-of-buffer))

;; TODO - use table-type for org/ses/etc
(defun present-coverage-as-table (title columns table &optional table-type)
  (when (= (length columns) 0)
    (error "Invalid columns passed in"))

  (let ((desired-buffer-name (concat "coverage: " title)))
    (check-buffer-does-not-exist desired-buffer-name)

    (with-current-buffer (get-buffer-create desired-buffer-name)
      (switch-to-buffer desired-buffer-name)
      (present-coverage-as-org-table columns table))))

;;; FORMAT
;; Two values
;; First: List containing column headers ('File', 'Covered Statements', 'Total Statements', ...)
;; Second: List containing row values based on headers AND (optional) a table that should be rendered when row is selected
;; ('src/app', 10, 15, ..., ())

(defun get-highlight-color-from-percentage (value)
  (cond
   ((>= value 80)
    "green")
   ((>= value 60)
    "yellow")
   (t
    "red")))

;;; TODO - move this into init.el and add as snippet to README?
(defun get-cell-info ()
  (interactive)
  (when (org-table-p)
    (let ((filename (org-table-get nil 1))
	  (column-name (org-table-get 1 nil)))
      (message (string-join (list filename column-name) " - ")))))

(global-set-key (kbd "C-c i") 'get-cell-info)
;;; END

(defun add-coverage-table-color-indicators ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (tables (org-element-map tree 'table 'identity)))
    (org-element-map (car tables) 'table-cell
      (lambda (x)
	(let ((cell-value (car (org-element-contents x)))
	      (cell-start (org-element-property :begin x))
	      (cell-end (- (org-element-property :end x) 1)))
	  (when (is-percentage cell-value)
	    (let* ((percentage (extract-percentage cell-value))
		   (color-to-apply (get-highlight-color-from-percentage percentage))
		   (overlay (make-overlay cell-start cell-end)))
	      (hlt-highlight-region cell-start cell-end `((t (:background ,color-to-apply)))))))))))

;; This takes an lcov-report HTML and returns
;; ("<title>", "X% <category> (M/N)", "X% <category> (M/N)", "X% <category> (M/N)")
(defun jest-parse--lcov-report-meta (lcov-report-html)
  (let* ((title (dom-texts (first (dom-by-tag lcov-report-html 'h1))))
	 (category-tags (dom-by-class lcov-report-html "space-right2"))
	 ;; TODO break up into flatter let block
	 (category-stats (string-join
			  (mapcar
			   (lambda (category-stat)
			     (let* ((info-elements (dom-by-tag category-stat 'span))
				    (info-texts (mapcar 'dom-text info-elements)))
			       (concat (first info-texts) " " (second info-texts) " (" (third info-texts) ")")))
			   category-tags))))
    (append (list title) category-stats)))

(defun jest-parse--lcov-report-row-identifier (lcov-report-row)
  (let* ((a-tag (first (dom-by-tag lcov-report-row 'a)))
	 (a-href (dom-attr a-tag 'href)))
    (if (string-suffix-p "index.html" a-href)
	(substring a-href 0 -10)
      (substring a-href 0 -5))))

(defun jest-parse--lcov-report-row (lcov-report-row)
  (let ((identifier (jest-parse--lcov-report-row-identifier lcov-report-row))
	(data (mapcar 'dom-text (cdr (cdr (dom-by-tag lcov-report-row 'td))))))
    (append (list identifier) data)))

;; Takes the lcov-report HTML and returns the rows to be rendered in a table
;; (("<identifier", "X%", "A/B", "Y%", "C/D"...),
;;  ("<identifier", "X%", "A/B", "Y%", "C/D"...)...)
(defun jest-parse--lcov-report-rows (lcov-report-html)
  (let* ((table-body (first (dom-by-tag lcov-report-html 'tbody)))
	 (table-rows (dom-by-tag table-body 'tr)))
    (mapcar 'jest-parse--lcov-report-row table-rows)))

;; TODO - be able to take any lcov html and generate table
(defun jest-parse--lcov-report (lcov-report-html)
  (let ((meta (jest-parse--lcov-report-meta lcov-report-html))
	(rows (jest-parse--lcov-report-rows lcov-report-html)))
    (let ((desired-buffer-name (concat "coverage: " (first meta))))
      (check-buffer-does-not-exist desired-buffer-name)

      (with-current-buffer (get-buffer-create desired-buffer-name)
	(switch-to-buffer desired-buffer-name)
	(present-coverage-as-org-table
	 (list "File" "Statements Covered" "Statements" "Branches Covered" "Branches" "Functions Covered" "Functions" "Lines Covered" "Lines")
	 rows)))))

(defun jest-parse--lcov-report-target (&optional target)
  (let* ((target-file (cond
		      ((null target)
		       "index.html")
		      ((string-suffix-p "/" target)
		       (concat target "index.html"))
		      (t
		       (concat target ".html"))))
	 (target-filepath (concat (projectile-project-root) "coverage/lcov-report/" target-file))
	 (xml-dom-tree (with-temp-buffer
			   (insert-file-contents target-filepath)
			   (libxml-parse-html-region (point-min) (point-max)))))
    (jest-parse--lcov-report xml-dom-tree)))

(defun jest-get-coverage ()
  (interactive)
  (jest-parse--lcov-report-target))

(global-unset-key (kbd "C-:"))
(global-set-key (kbd "C-:") 'jest-get-coverage)

(defun get-target-coverage ()
  (interactive)
  (when (org-table-p)
    (let ((identifier (org-table-get nil 1)))
      (jest-parse--lcov-report-target identifier))))

(global-unset-key (kbd "C-c c"))
(global-set-key (kbd "C-c c") 'get-target-coverage)

(provide 'emacs-jest)
;;; emacs-jest.el ends here
