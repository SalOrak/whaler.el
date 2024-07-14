;;; whaler-functions.el --- Common functions for whaler -*- lexical-binding: t -*-

;; Author: Hector "Salorak" Alarcon <salorack@protonmail.com>
;; Maintainer: Hector "Salorak" Alarcon <salorack@protonmail.com>
;; URL: https://github.com/salorak/whaler.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") ( f "0.20.0"))
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains common functions for the
;; minimalistic project manager `whaler'

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Requires
(require 'f)
(require 'whaler)
(require 'counsel-fzf)
(require 'counsel-rg)

;; Example of custom functions to extend whaler
(cl-defun whaler-functions-prompt (&optional (post " >> ") (dir default-directory))
  "Custom Whaler prompt"
  (concat "[" (f-filename dir) "]" post))


(defun whaler-functions-async-shell()
  "Custom async shell function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory 
   (lambda ()(interactive)
     (call-interactively #'async-shell-command))
   nil))


(defun whaler-functions-find-files ()
  "Custom find files function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (dir)(interactive)
     (counsel-fzf
      ""
      dir
      (whaler-functions-prompt " -- Find files >> " dir)))))

(defun whaler-functions-rg()
  "Execute `counsel-rg' function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (dir)(interactive)
     (counsel-rg
      ""
      dir 
      nil
      (whaler-functions-prompt " -- Search String >> ")))
   t))


(defun whaler-functions-dired-root ()
  "Open root project in dired for `whaler.el'"
  (interactive)
  (whaler-execute-function-on-current-working-directory 'dired ))

(defun whaler-functions-counsel-find-files (dir)
  "Wrapper for finding files in another directory"
  (interactive)
  (counsel-fzf
   ""
   dir
   (whaler-functions-prompt " -- Find files >> " dir)))

(defun whaler-functions-counsel-search-strings (dir)
  "Wrapper for searching strings in another directory"
  (interactive)
  (counsel-rg
   ""
   dir
   nil
   (whaler-functions-prompt " -- Search String >> " dir)))

(defun whaler-functions-compile()
  "Execute `compile' function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (dir)
     (interactive)
     (let (
	   (compilation-command
	    (read-string (whaler-functions-prompt " -- Compile commmand >> " dir)
			 )
	    ))
       (compile compilation-command)))))

(defun whaler-functions-compile-other ()
  "Wrapper for executing `compile' in another directory."
  (interactive)
  (let (
	(compilation-command
	 (read-string (whaler-functions-prompt " -- Compile commmand >> "))))
    (compile compilation-command)))
