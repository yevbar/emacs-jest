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

;; TODO - config to save output from compilation call to file
;; TODO - either generate SES on the fly or create SES file (latter is likely better)

;;; General utils
(defvar node-error-regexp
  "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\(\\(?:[a-zA-Z]:\\)?[a-zA-Z\.0-9_/\\-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?"
  "Regular expression to match NodeJS errors.
From http://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/")

(defvar node-error-regexp-alist
  `((,node-error-regexp 1 2 3)))

;;; Related to the compilation buffer
(defun jest-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(define-compilation-mode jest-compilation-mode "Jest"
  "Jest compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist) node-error-regexp-alist)
    (add-hook 'compilation-filter-hook 'jest-compilation-filter nil t)
    ))

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

;; TODO - account for custom variables (ie maxWorkers)
(defun get-jest-arguments (&optional arguments)
  (if arguments
      (string-join
       ;; TODO - Figure out what to do about duplicates/overwrites
       (flatten-list (list jest-environment-vars arguments jest-default-args)) " ")
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
  (when (get-buffer "*jest tests*")
    (kill-buffer "*jest tests*"))

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

(provide 'emacs-jest)
;;; emacs-jest.el ends here
