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

;;; Coverage related
;; Parse a coverage table page
(defun jest-parse-lcov-report-coverage-table ()
  )

;; Parse an individual file coverage page
(defun jest-parse-lcov-report-coverage-file ()
  )

(defun jest-parse-lcov-report ()
  
  )

;; Takes a <package> node obtained from clover.xml and returns the attributes related to coverage
(defun jest-parse-clover-xml-package-coverage (package-node)
  (let ((metrics-node (first (dom-by-tag package-node 'metrics))))
    (jest-parse-clover-xml-metrics metrics-node)))

;; Takes either a node or string to format
(defun jest-parse-clover-xml-package-name (package-name)
  (unless (stringp package-name)
    (setq package-name (name-attr-value (dom-attr package-node 'name))))

  (replace-in-string "\." "\/" package-name))

(defun jest-parse-clover-xml-package (package-node)
  (list
   (jest-parse-clover-xml-package-name package-node)
   (jest-parse-clover-xml-package-coverage package-node)))

(defun jest-parse-clover-xml-project-coverage (project-node)
  (let ((project-metrics (first (dom-by-tag project-node 'metrics))))
    (jest-parse-clover-xml-metrics project-metrics)))

(defun jest-parse-clover-xml-project-name (project-node)
  (dom-attr project-node 'name))

(global-unset-key (kbd "C-:"))
(global-set-key (kbd "C-:") 'jest-parse-clover-xml)

(defun jest-parse--clover-xml-metrics-single-node (node)
  (let* ((node-name (jest-parse-clover-xml-package-name (dom-attr node 'name)))
	 (metrics-node (first (dom-by-tag node 'metrics)))
	 (covered-statements (string-to-number (dom-attr metrics-node 'coveredstatements)))
	 (total-statements (string-to-number (dom-attr metrics-node 'statements)))
	 (covered-conditionals (string-to-number (dom-attr metrics-node 'coveredconditionals)))
	 (total-conditionals (string-to-number (dom-attr metrics-node 'conditionals)))
	 (covered-methods (string-to-number (dom-attr metrics-node 'coveredmethods)))
	 (total-methods (string-to-number (dom-attr metrics-node 'methods))))
    (list
     node-name
     (get-percentage covered-statements total-statements)
     (concat (number-to-string covered-statements) "/" (number-to-string total-statements))
     (get-percentage covered-conditionals total-conditionals)
     (concat (number-to-string covered-conditionals) "/" (number-to-string total-conditionals))
     (get-percentage covered-methods total-methods)
     (concat (number-to-string covered-methods) "/" (number-to-string total-methods)))))

(defun jest-parse--clover-xml-single-package (package-node)
  (let* ((package-coverage (jest-parse--clover-xml-metrics-single-node package-node))
	 (file-nodes (dom-by-tag package-node 'file))
	 (files-coverage (mapcar 'jest-parse--clover-xml-metrics-single-node file-nodes)))
    (append
     package-coverage
     (list files-coverage))))

;; Takes a list of package nodes and returns their rows as well as their corresponding file tables data
(defun jest-parse--clover-xml-package-metrics (package-nodes)
  (mapcar 'jest-parse--clover-xml-single-package package-nodes))

;; Returns a spreadsheet presentable version of the clover.xml report
;; if package is provided then generates based on that package
(defun jest-parse--clover-xml-result ()
  ;; TODO - check that clover.xml actually exists here
  ;;(unless )

  (let* ((clover-xml-filepath (concat (projectile-project-root) "coverage/clover.xml"))
	 (xml-dom-tree (with-temp-buffer
			 (insert-file-contents clover-xml-filepath)
			 (libxml-parse-xml-region (point-min) (point-max))))
	 (project-node (first (dom-by-tag xml-dom-tree 'project)))
	 (package-nodes (dom-by-tag project-node 'package)))
    (list
     (list "File" "Covered Statements" "Total Statements" "Covered Conditionals" "Total Conditionals" "Covered Methods" "Total Methods")
     (append
      (list (jest-parse--clover-xml-metrics-single-node project-node))
      (jest-parse--clover-xml-package-metrics package-nodes)))))

(defun present-coverage-as-org-table (columns table)
  (insert (concat "|" (string-join columns "|") "|"))
  (newline)

  (insert "|-")
  (newline)

  (mapc
   (lambda (row)
     (progn
       (insert "|")
       (mapc
	(lambda (item)
	  (when (not (proper-list-p item))
	    (insert item)
	    (insert "|")))
	row)
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
  (beginning-of-buffer)

  (local-set-key (kbd "C-c c") (lambda ()
				 (interactive)
				 (when (org-table-p)
				   (let* ((row-identifier (org-table-get nil 1))
					  (row-index (-find-index (lambda (row) (string-equal row-identifier (first row))) table))
					  (selected-row (unless (or (not row-index) (eq row-index 0)) (nth row-index table))))
				     (cond
				      ((eq row-index 0)
				       (message "Already viewing"))
				      (row-index
				       (present-coverage-as-table
					row-identifier
					columns
					(append (list selected-row) (first (last selected-row)))))))))))

;; TODO - use table-type for org/ses/etc
(defun present-coverage-as-table (title columns table &optional table-type)
  (when (= (length columns) 0)
    (error "Invalid columns passed in"))

  (let ((desired-buffer-name (concat "coverage: " title)))
    (check-buffer-does-not-exist desired-buffer-name)

    ;; TODO - change to be formatted with overall-coverage name so we can have multiple folders open
    (with-current-buffer (get-buffer-create desired-buffer-name)
      (switch-to-buffer desired-buffer-name)
      (present-coverage-as-org-table columns table))))

;;; FORMAT
;; Two values
;; First: List containing column headers ('File', 'Covered Statements', 'Total Statements', ...)
;; Second: List containing row values based on headers AND (optional) a table that should be rendered when row is selected
;; ('src/app', 10, 15, ..., ())

(defun jest-parse-clover-xml ()
  (interactive)
  (let* ((parse-result (jest-parse--clover-xml-result))
	 (columns (first parse-result))
	 (table (second parse-result)))
    (present-coverage-as-table "All files" columns table)))

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

(provide 'emacs-jest)
;;; emacs-jest.el ends here
