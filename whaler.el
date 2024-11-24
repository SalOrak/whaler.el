;;; whaler.el --- Minimalistic and customizable project manager-*- lexical-binding: t -*-

;; Author: Hector "Salorak" Alarcon <salorack@protonmail.com>
;; Maintainer: Hector "Salorak" Alarcon <salorack@protonmail.com>
;; URL: https://github.com/salorak/whaler.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") ( f "0.20.0") (dash "2.19.1") )
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

;; This package provides `Whaler', a minimalistic and highly customizable
;; project manager focused on tailored experiences.

;; It is a minimalistic project manager aiming to provide an easy API to
;; work with repositories / directories / projects.

;; `Whaler' provides functions to easily create a pool of projects as
;; well as interact with these projects.  For example, with `Whaler' you
;; can call `compile' in a project chosen from the minibuffer without
;; changing the current project.  But not only you can call `compile', but
;; any function or custom function you want, interacting either in your
;; current working project or another project selected from the minibuffer.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'f)
(require 'dash)

;;* Customization
(defgroup whaler nil
  "Whaler.el."
  :link '(info-link :tag "Info Manual" "(whaler)")
  :link '(url-link :tag "Homepage" "https://github.com/salorak/whaler.el")
  :link '(emacs-library-link :tag "Library Source" "whaler.el")
  :group 'tools
  :group 'convenience
  :prefix "whaler-")

(defcustom whaler-include-hidden-directories nil
  "Whether whaler should include hidden directories or not."
  :type 'boolean)

(defcustom whaler-directories-alist '("~/personal/" "~/work/")
  "List of dirs where projects will be retrieved and added to projects list.
For example, if the list is (\'~/personal\' \'~/work\') it will search,
with depth 1, and add all directories that have as parent directories
\'~/personal/\' or \'~/work\'.
The resulting list would be something similar to
\(\'~/personal/project-emacs\' \'~/personal/helloworld\' \'~/work/secret\'
\'~/work/backend-server\').
These are just an example."
  :type 'alist)

(defcustom whaler-oneoff-directories-alist `(,user-emacs-directory)
  "List project's paths that will be added in the projects list directly.
If you want a single project to appear directly you can add it to this list.
Imagine you want to add the `user-emacs-directory' but don't want to add ALL
the \'~/.config/\' directories, you can add it here.
By default is the `user-emacs-directory'."
  :type 'alist)

(defcustom whaler-default-working-directory (f-full (getenv "HOME"))
  "Default directory to use when no projects found or the cwd is not set.
It acts as a fallback."
  :type 'string)

(defvar whaler-current-working-directory nil
  "Current working directory selected through `whaler'.")

(defvar whaler--project-directories '()
  "List of directory paths used as candidates to search.
This list represents the available candidates when executing `whaler'.
DO NOT MODIFY IT MANUALLY, instead modify the `whaler-directories-alist'
and `whaler-oneoff-directories-alist' lists and then
run `whaler-populate-projects-directories' to automatically update this list.")

(defvar whaler--hash-project-aliases (make-hash-table :test 'equal)
  "Hash table containing the directory paths and its aliases.
DO NOT MODIFY IT MANUALLY."
  )
(defvar whaler--project-keys '()
  "List of KEYS for the `whaler--hash-project-aliases'.
List used to be display in the `completing-read' function instead of the
absolute PATH.  It improves readability."
  )

;; Functions
(defun whaler-current-working-directory ()
  "Return the current working directory if set, otherwise the default directory.
Fallbacks to the $HOME directory if neither is a valid path."
  (cond
   ((and (stringp whaler-current-working-directory)
         (f-dir-p whaler-current-working-directory)
         (not (string-empty-p whaler-current-working-directory)))
    whaler-current-working-directory)
   ((and (stringp whaler-default-working-directory)
         (f-dir-p whaler-default-working-directory)
         (not (string-empty-p whaler-default-working-directory)))
    whaler-default-working-directory)
   ((f-dir-p (getenv "HOME"))
    (getenv "HOME"))))

(cl-defun whaler-execute-function-on-current-working-directory (action &optional (action-arg t))
  "Generic function to execute in the current working directory.

The ACTION parameter represent the function to execute.

The ACTION-ARG parameter determines whether the current working directory
should be passed as an argument to the ACTION function.
By default is t.

It accepts a string parameter, specifically the current whaler directory."
  (interactive)
  (let ((default-directory (whaler-current-working-directory)))
    (if (null action-arg)
	    (funcall action)
	  (funcall action default-directory))))


(defun whaler--directory-exists (dir)
  "Check whether the directory path exists or not.
If DIR does not exist, prints it as an error message but doesn't raise an error."
  (if (not (f-dir-p dir))
      (progn
        (message (concat "[Whaler] Error: Directory " dir " not found."))
        nil)
    dir))

(cl-defun whaler--generate-subdirectories (list &key (hidden whaler-include-hidden-directories))
  "Generate all subdirectories using the provided LIST argument.
It search inside each directory in LIST argument and appends every subdirectory
 in the `whaler--project-directories'.
LIST corresponds to the list of directories to search in.
HIDDEN is used to indicate whether to append hidden directories or not."
  (dolist (value list)
    (when (whaler--directory-exists value)
      (dolist (el (f-directories (f-long value) (lambda (x) (or (not (f-hidden-p x 'last)) hidden)) nil))
        (add-to-list 'whaler--project-directories el)))))

(defun whaler--add-oneoff-directories ()
  "Append the oneoff directories directly to the projects list."
  (mapcar (lambda (x)
            (when (whaler--directory-exists x)
              (add-to-list 'whaler--project-directories x)))
          whaler-oneoff-directories-alist))


(defun whaler--format-path (path)
  "Return the PATH string formatted to be improve readability in the UI.
The format is as follows: [ PARENT-DIRECTORY  ] FINAL-DIRECTORY where
PARENT-DIRECTORY is the parent directory of the PATH.
FINAL-DIRECTORY is the directory pointing to.

Example:
Given the PATH '/home/user/programming/whaler'
The PARENT-DIRECTORY is 'programming' and
the FINAL-DIRECTORY is 'whaler'.
The functions then returns '[ programming ] whaler' string."
  (cond
   (( not (f-root-p path))
    (let (
          (parent (car (last (f-split (f-parent path)))))
          (final (cadr (last (f-split path) 2))))
      (format "[ %s ] %s" parent final)))
   (t
    (format "[ root ] /")
    )))

(defun whaler--update-hash-table ()
  "Update the projects hash table.
The hash table contains the ALIAS as KEY and PATH as VALUE"
  (setq whaler--hash-project-aliases (make-hash-table :test 'equal))
  (setq whaler--project-keys '())
  (mapcar #'(lambda (path)
              (progn
                (let ((fp (whaler--format-path path)))
                  (puthash fp path whaler--hash-project-aliases)
                  (add-to-list 'whaler--project-keys fp)
                  )
                ))
          whaler--project-directories)
  )

(defun whaler-populate-projects-directories ()
  "Populate the projects list `whaler--project-directories' and delete duplicates."
  (interactive)
  (setq whaler--project-directories '())
  (whaler--generate-subdirectories whaler-directories-alist)
  (whaler--add-oneoff-directories)
  (delete-dups whaler--project-directories)
  (whaler--update-hash-table)
  (message "[Whaler] Projects have been repopulated."))


(cl-defun whaler ( &key (action 'dired) (action-arg t)(change-cwd-auto t))
  "Change or move between directories blazingly fast.
Apply an ACTION in the selected directory.
By default it will open the directory with `dired'.

ACTION is a function that accepts a parameter, string, and will be used upon
the selected directory.  By default is `dired'.

ACTION-ARG determines whether the ACTION function should receive the
selected directory or not.  By default is t.

CHANGE-CWD-AUTO is a boolean indicating whether whaler should
set the selected candidate as its current working directory or not.  Default t"

  (interactive)
  (let ((chosen-directory ""))
    (setq chosen-key (completing-read "[ Whaler ] >> " whaler--project-keys nil t))
    (setq chosen-directory (gethash chosen-key whaler--hash-project-aliases))
    (when change-cwd-auto (setq whaler-current-working-directory (f-slash chosen-directory)))
    (let ((default-directory chosen-directory))
      (if (null action-arg)
	      (funcall action)
	    (funcall action chosen-directory)))))


(provide 'whaler)

;;; whaler.el ends here
