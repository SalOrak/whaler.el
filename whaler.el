;;; whaler.el --- Move between directories blazingly fast -*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

;; Author: Hector Alarcon <salorack@protonmail.com>
;; Maintainer: Hector Alarcon <salorack@protonmail.com>
;; URL: https://github.com/salorack/whaler.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.5"))
;; Keywords: matching

;; This file is part of GNU Emacs.

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

;; This package provides `whaler' as a way to move between
;; directories blazingly fast while maintaining some sort of
;; current working directory

;; It is minimalistic project manager aiming to help move between
;; directories and find files as fast as possible.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'f)
(require 'ivy)


;;* Customization 
(defgroup whaler nil
  "Whaler.el"
  :link '(info-link :tag "Info Manual" "(whaler)")
  :link '(url-link :tag "Homepage" "https://github.com/salorak/whaler.el")
  :link '(emacs-library-link :tag "Library Source" "whaler.el")
  :group 'tools
  :group 'convenience
  :prefix "whaler-")

(defcustom whaler-include-hidden-directories nil
  "Whether whaler should include hidden directories or not."
  :type 'boolean
  )

(defcustom whaler-directories-alist '("~/personal/" "~/work/")
  "List of directories where projects will be retrieved and added to the projects list. For example, if the list is ('~/personal' '~/work') it will search, with depth 1, and add all directories that have as parent directories '~/personal/' or '~/work'. The resulting list would be something similar to ('~/personal/project-emacs' '~/personal/helloworld' '~/work/secret' '~/work/backend-server'). These are just an example."
  :type 'alist
  )

(defcustom whaler-oneoff-directories-alist '("~/.config/emacs/")
  "List contaning path to projects that will be added in the projects list directly. If you want a single project to appear directly you can add it to this list. Imagine you want to add '~/.config/emacs/' but don't want to add ALL the '~/.config/' directories, you can add it here."
  :type 'alist
  )

(defcustom whaler-default-working-directory (f-full "~")
  "Default directory to use when `whaler-current-working-dir' is nil or no projects have been found. It acts as a fallback."
  :type 'string
  )

(defvar whaler-current-working-directory nil
  "Current working directory selected through `whaler-whaler'."
  ) 

(defvar whaler-project-directories '()
  "List of directory paths used as candidates to search. This list represents the available candidates when executing `whaler-whaler'. DO NOT MODIFY IT MANUALLY, instead modify the `whaler-directories-alist' and `whaler-oneoff-directories-alist' lists and then run `whaler-populate-projects-directories' to automatically update this list."
  )

;; Functions

(cl-defun whaler--execute-function-on-current-working-directory
    (action)
  "Generic function to execute in the current working directory.
The `action' parameter represent the function to execute. It should accept a string parameter, specifically it will receive the `whaler-current-working-directory' or `whaler-default-working-directory' as argument."
  (interactive)
  (cond
   ((null action)
      (user-error "`action' function cannot be nil")
      )
   ((fboundp action)
      (user-error "`action' should have a SYMBOL defined.")
      )
   )
  (cond
   ((null whaler-current-working-dir) 
    (funcall action whaler-default-working-dir) 
    )
   (f-dir-p whaler-current-working-dir) 
   (funcall action whaler-current-working-dir)
   )
  )
  
(defun --whaler-default-find-files-function (directory)
  "Default function used when executing `whaler-find-files-in-current-working-dir'."
  (counsel-find-file "" directory)
  )

(cl-defun whaler-find-files-in-current-working-directory
    (&key (action #'--whaler-default-find-files-function))
  "Find files in the `whaler-current-working-directory'. If there are no `whaler-project-directories' it will use the 'whaler-default-working-directory' as fallback to search in.

`ACTION' is a function that accepts one argument, a string representing a directory path. By default it uses `counsel-find-file' to search for files. 
"
  (interactive)
  (whaler--execute-function-on-current-working-directory #'action)
  )


(cl-defun whaler--generate-subdirectories (list &key (hidden whaler-include-hidden-directories))
  "Generates all subdirectories using the provided `list' argument. It search inside eachdirectory in `list' argument and appends every subdirectory in the `whaler-project-directories'. 
`HIDDEN' is used to indicate whether to append hidden directories or not." 
  (let (value)
    (dolist (value list)
      (let (el)
	(dolist (el (f-directories (f-long value) (lambda (x) (or (not (f-hidden-p x 'last)) hidden)) nil))
	  (add-to-list 'whaler-project-directories el)
	  )
	)
      )
    )
  )

(defun whaler--add-oneoff-directories ()
  "Appends the oneoff directories directly to the projects list."
  (mapcar (lambda (x) (add-to-list 'whaler-gen-dirs x)) whaler-oneoff-directories-alist)
  )

(defun whaler-populate-projects-directories ()
  "Populate the projects list `whaler-project-directories' and delete duplicates."
  (interactive)
  (whaler--generate-subdirectories whaler-directories-alist) ;; Add 
  (whaler--add-oneoff-directories)
  (delete-dups whaler-project-directories) 
  )

(cl-defun whaler-whaler ( &key (action 'dired) (change-cwd-auto t))
  "Change or move between directories blazingly fast and apply an `action' in the selected directory. By default it will open the directory with `dired'.

`ACTION' is a function that accepts a parameter, string, and will be used upon the selected directory. By default is dired.

`CHANGE-CWD-AUTO' is a boolean indicating whether whaler should set the selected candidate as its current working directory or not. By defaul is t.
"
  (interactive)
  (ivy-read "[ Whaler ] >> " whaler-project-directories
	    :require-match t ;; Only one candidate
	    :action (lambda (x)
		      (progn
			(when change-cwd-auto 
			  (setq whaler-current-working-dir (f-slash x))
			  )
			(funcall action whaler-current-working-dir)
			)
		      )
	    )
  )

(provide 'whaler)

;; whaler end here
