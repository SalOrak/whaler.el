# Whaler

![Whaler Logo Image](doc/whaler-logo.png)

## What is Whaler.el?

**Whaler** is `completing-read` function extension to move ~blazingly~ fast between directories as well as have a minimalistic sense of working directory.

**Whaler** offers a fast experience to move between directories and act upon them.

## Why use Whaler instead of X?

Whaler is not better than other solutions, it is just another solution. The one that works for me.

I've ported and added some things from my original [`Whaler.nvim`](https://github.com/salorak/whaler.nvim) plugin from NeoVim here to Emacs. Why? For the same reason I developed it in the first place:
> **I wanted a simple and minimalistic way to move between directories and files.**

**Whaler.el** is works specially well with the great [Ivy](https://github.com/abo-abo/swiper) extension that adds a thin layer to be able to interact with directories and projects.

So what does this package do exactly? 

## Features

As I already mentioned, `Whaler.el` is a package to move between directories while maintaining a sense of working directory. This means the following:

1. Ability to find directories based on parent paths. For example, keeping all programming directories under `~/programming`, personal projects under `~/personal` and work related projects under `~/work`. I want all of the directories whose parent directory I added.
2. Ability to switch from directories as fast as possible and with a completion system.
3. If I go deep into a many sublevels of a project I want to be able to go to the root directory, find files in the whole project or just open a shell on root directory. 
4. Ability to find an specific file from another project without losing my current working directory.
5. Ability to cherry pick specific projects without adding a whole bunch of projects. A common case is adding the `~/.config/emacs/` or maybe `/usr/share/fonts`. 
6. Being able to customize what I would like to do with the directory I selected. It could be executing `magit`, finding files with `counsel`, opening `dired` on another window. 

`Whaler.el` does exactly that. Nothing less, nothing more, just that.


## Usage

This package does not come with any mappings by default. Here is an example on how to set it up (using `use-package`:

```elisp
(use-package whaler
	:after '(ivy) ;; Optional
	:ensure t
	:bind ( 
	;;; Completly up to you keybindings.
	("M-p w w" . whaler) ;; Change between directories changing working dir.
	("M-p w p" . whaler :change-cwd-auto nil) ;; Change between directories WITHOUT changing working dir.
	("M-p w f" . whaler-find-files-in-current-working-directory) ;; Find files in the working directory
	)
	:custom
	;; RECOMMENDED! Add at least one of the following or whaler does nothing
	(setq whaler-directories-alist '("~/personal" "~/work")) ;; List of parent directories to search for projects and add them.
	(setq whaler-oneoff-directories-alist '("~/.config/emacs")) ;; List of projects to add directly.
	;; (OPTIONAL)
	(setq whaler-include-hidden-directories nil) ;; Whether whaler should include hidden directories
	(setq whaler-default-working-directory "~") ;; Fallback working directory to search for files in case no project has been selected
	:config
	;; --- THIS IS IMPORTANT --- In order to avoid cpu usage finding for directories every time, the projects are populated once. To regenerate projects run this.
	(whaler-populate-projects-directories)
)
```


Once installed and configured its time to use it:

- Pressing `M-p w w` launches the completion miniframe. Selecting any project will open `dired` in that directory and set it as your current working directory.
- Pressing `M-p w p` does the same but it does NOT changes your current working directory. This is a life saver when the only thing you want to do is look for an specific file or read something from another project.
- Pressing `M-p w f` finds files in the current working directory. By default uses `counsel-fzf` but you can change it as you wish.

##  Customization

Here comes the fun part: customizing whaler to suit your specifics needs.

For example, lets say you want to bind `M-p w g` to open `magit` in the selected project without changing your current working directory. Add this to your config.
```elisp
(use-package whaler 
;; (-- Previous config --)
:bind(
	("M-p w g" . whaler :action 'magit :change-cwd-auto nil) ;; Change between directories changing working dir.
)
;; (-- Moreconfig --)
)
```

That's it.

Now you want to open a `shell` in the selected directory and in the current working directory respectively.
```elisp
(use-package whaler 
;; (-- Previous config --)
:bind(
	("M-p w s" . whaler :action '(lambda (d) (shell-cd d)(shell)) :change-cwd-auto nil) ;; Run eshell on selected directory.
	("M-p w e" . whaler-execute-function-on-current-working-directory  (lambda (d) (shell-cd d)(eshell))) ;; Run eshell on current working directory or default directory.
)
;; (-- Moreconfig --)

```


From here you can do whatever you want. Compiling, magit, dired, finding files, executing complex elisp functions, etc. Endless possibilities :)

And yes, the function `whaler-find-files-in-current-working-directory` internally calls `whaler--execute-function-on-current-working-directory`.

If you want to know more I strongly recommend reading the function descriptions.

Here are the most interesting ones:

- **`whaler-find-files-in-current-working-directory (action)`**: "Find files in the `whaler-current-working-directory`. If there are no `whaler-project-directories` it will use the `whaler-default-working-directory` as fallback to search in. `ACTION` is a function that accepts one argument, a string representing a directory path. By default it uses `counsel-fzf` to search for files. "

- **`whaler-execute-function-on-current-working-directory (action)`**: "Generic function to execute in the current working directory. The `action` parameter represent the function to execute. It should accept a string parameter, specifically it will receive the `whaler-current-working-directory` or `whaler-default-working-directory` as argument."

- **`whaler ( &key (action 'dired) (change-cwd-auto t))`**: "Change or move between directories blazingly fast and apply an `action` in the selected directory. By default it will open the directory with `dired`. `ACTION` is a function that accepts a parameter, string, and will be used upon the selected directory. By default is dired. `CHANGE-CWD-AUTO` is a boolean indicating whether whaler should set the selected candidate as its current working directory or not. By defaul is `t`. "

##  Whaler as a project manager
`Whaler.el` can do most of what other project managers do in Emacs. `Whaler` gives users an API to work with directories / repositories.
In my personal [Emacs config](https://github.com/SalOrak/dotfiles/blob/main/files/emacs/plugins/whaler.el) you can see a bunch of functions to do the basic stuff.

Here are the **most** used functions for me:
- **Find files**: The ability to find files from the root of the current working directory.
- **Search strings**: The ability to search strings from all files in the current working directory.
- **Compile**: The ability to compile (call `compile`) in the root of the current working directory.
  - **Cd back**: The ability go back to the root directory.

The following snippet is the `elisp` code to make all of these functions possible using `whaler.el`.
```elisp
;; Custom functions to extend whaler
(cl-defun salorak/whaler-prompt (&optional (post " >> "))
  "Whaler prompt"
  (concat "[" (f-filename whaler-current-working-directory) "]" post)
  )

(defun salorak/whaler-find-files ()
  "Custom find files function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (x)(interactive)
     (counsel-fzf
      ""
      x
      (salorak/whaler-prompt " -- Find files >> ")
      ))))

(defun salorak/whaler-rg()
  "Execute `counsel-rg' function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (x)(interactive)
     (counsel-rg
      ""
      x
      nil
      (salorak/whaler-prompt " -- Search String >> ")
      ))))

(defun salorak/whaler-compile()
  "Execute `compile' function for `whaler.el' in the cwd."
  (interactive)

  (let (
	(compilation-command
	 (read-string (salorak/whaler-prompt " -- Compile commmand >> ")
		      )
	 ))
	(let (
	      (default-directory whaler-current-working-directory)
	      )
	  (compile compilation-command)
	  )))

(defun salorak/whaler-dired-root ()
  "Open root project in dired for `whaler.el'"
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (x)(interactive)
     (dired x)
     )))
```

If you have any other code or functionality that want to share don't forget to open an issue! 
 

## Alternatives

There are tons of great projects out there that perform some sort of navigation between projects. Here is a list of them! Check them out:
- [projectile](https://github.com/bbatsov/projectile): Projectile is a project integration library for Emacs. Its goal is to provide a nice set of features operating on a project level without introducing external dependencies.

Note: If there you use another alternative not listed here, please submit an issue an I gladly add it here ;)
