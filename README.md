# Whaler

![Whaler Logo Image](doc/whaler-logo.png)

## What is Whaler.el?

**Whaler** is `completing-read` function extension to move ~blazingly~ fast between directories as well as have a minimalistic sense of working directory whilst providing an easy to use API to work with directories.

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
	;; RECOMMENDED! Add at least one of the following or whaler won't do nothing
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
	("M-p w g" . whaler :action 'magit :change-cwd-auto nil) ;; Open magit in a selected directory without changing the current working directory.
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

- **`whaler-execute-function-on-current-working-directory (action &optional ())`**: "Generic function to execute any function in the current working directory. The `ACTION` parameter represent the function to execute. The `ACTION-ARG` parameter determines whether the current working directory should be passed as an argument to the `ACTION` function. By default is `t`. It should accept a string parameter, specifically it will receive the `whaler-current-working-directory` or `whaler-default-working-directory` as argument."

- **`whaler ( &key (action 'dired) (action-arg t)(change-cwd-auto t))`**: "Change or move between directories blazingly fast and apply an `action` in the selected directory. By default it will open the directory with `dired`. `ACTION` is a function that accepts a parameter, string, and will be used upon the selected directory. By default is dired. `ACTION-ARG` determines whether the `ACTION` function should receive the selected directory as argument or not. By default is `t`. `CHANGE-CWD-AUTO` is a boolean indicating whether whaler should set the selected candidate as its current working directory or not. By default is `t`. "

### Understanding `whaler` main functions

`whaler()` is the primarly function to move between directories. But not only move, but also to ACT upon directories.
The `whaler` function has the following arguments:
- `action`: The function to execute once a candidate ( a project) has been selected. By default is `dired`, which means that once you select a project it will execute `dired` on that project. 
- `action-arg`: Sometimes the `action` function requires the directory argument to be passed around. `action-arg` determines whether the selected directory should be passed to the function. By default is `t` (true).
- `change-cwd-auto`: This parameter determines whether whaler (the package) should set the current working directory (the project) to the selected candidate. By default is `t` (true).

On the other hand, `whaler-execute-function-on-current-working-directory` is used to work on the current working directory, i.e. the project selected through `whaler`.
The `whaler-execute-function-on-current-working-directory` function has the following arguments:
- `action`: The function to execute once a candidate ( a project) has been selected. By default is `dired`, which means that once you select a project it will execute `dired` on that project. 
- `action-arg`: Sometimes the `action` function requires the directory argument to be passed around. `action-arg` determines whether the selected directory should be passed to the function. By default is `t` (true).

Familar right? That is because it works the same way as `whaler` but on the current working directory!

Okay, I kinda understand but why all these arguments? What can I do?
The best way to summarize it is to show you a real example:

Let's say you are working on a new project called `react-test`. First you move to that project using `whaler` and selecting `react-test`. Perfect. But you suddenly realize there is a file you commonly place on your react projects which is under the `common-react` project. For that you call `whaler :change-cwd-auto nil`, select `common-react`, find the corresponding file and copy it over the `react-test`. 

But, wait, because now you can move back to the root directory of the `react-test` by executing `whaler-execute-function-on-current-working-directory 'dired`. Awesome.

But now, you want to search a string in another repository using `counsel-rg`. You can create or directly call to `whaler-execute-function-on-current-working-directory :action 'counsel-rg`. That's it. 




##  Whaler as a project manager
`Whaler.el` can do most of what other project managers do in Emacs. `Whaler` gives users an API to work with directories / repositories.
In my personal [Emacs config](https://github.com/SalOrak/dotfiles/blob/main/files/emacs/plugins/whaler.el) you can see a bunch of functions to do the basic stuff.

Here are the **most** used functions for me:
- **Find files**: The ability to find files from the root of the current working directory.
- **Search strings**: The ability to search strings from all files in the current working directory.
- **Compile**: The ability to compile (call `compile`) in the root of the current working directory.
- **Change directory to the root of the project**: The ability go back to the root directory.

The following snippet is the `elisp` code to make all of these functions possible using `whaler.el`.
```elisp
;; Custom functions to extend whaler
(cl-defun salorak/whaler-prompt (&optional (post " >> ") (dir default-directory))
  "Custom Whaler prompt"
  (concat "[" (f-filename dir) "]" post))

(defun salorak/whaler-find-files ()
  "Custom find files function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda ()(interactive)
     (counsel-fzf
      ""
      default-directory
      (salorak/whaler-prompt " -- Find files >> ")))))

(defun salorak/whaler-rg()
  "Execute `counsel-rg' function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda (dir)(interactive)
     (counsel-rg
      ""
      dir 
      nil
      (salorak/whaler-prompt " -- Search String >> ")
      ))
	  t))

(defun salorak/whaler-compile()
  "Execute `compile' function for `whaler.el' in the cwd."
  (interactive)
  (whaler-execute-function-on-current-working-directory
   (lambda ()
     (interactive)
     (let (
	   (compilation-command
	    (read-string (salorak/whaler-prompt " -- Compile commmand >> ")
			 )
	    ))
       (compile compilation-command)))))

(defun salorak/whaler-dired-root ()
  "Open root project in dired for `whaler.el'"
  (interactive)
  (whaler-execute-function-on-current-working-directory 'dired t))

(defun salorak/whaler-counsel-find-files (dir)
  "Wrapper for finding files in another directory in whaler.el"
  (interactive)
  (counsel-fzf
   ""
   dir
   (salorak/whaler-prompt " -- Find files >> " dir)))

(defun salorak/whaler-counsel-search-strings (dir)
  "Wrapper for searching strings in another directory in whaler.el."
  (interactive)
  (counsel-rg
   ""
   dir
   nil
   (salorak/whaler-prompt " -- Search String >> " dir)))

(defun salorak/whaler-compile-other ()
  "Wrapper for executing `compile' in another directory in whaler.el."
  (interactive)
  (let (
	(compilation-command
	 (read-string (salorak/whaler-prompt " -- Compile commmand >> ")
		      )
	 ))
    (compile compilation-command)))
```

If you have any other code or functionality that want to share don't forget to open an issue! 

And the following is a snippet of how to map keys to functions using [General.el](https://github.com/noctuid/general.el):

```elisp 
;; Whaler keymaps
(leader-spc 'normal 'override
  "ff" 'salorak/whaler-find-files
  "ss" 'salorak/whaler-rg
  "sh" 'whaler
  "su" (lambda () (interactive)(whaler :change-cwd-auto nil))
  "fo" (lambda () (interactive)(whaler :change-cwd-auto nil :action 'salorak/whaler-counsel-find-files))
  "so" (lambda () (interactive)(whaler :change-cwd-auto nil :action 'salorak/whaler-counsel-search-strings ))
  "pv" 'salorak/whaler-dired-root
  "po" (lambda () (interactive)(whaler :change-cwd-auto nil :action 'dired :action-arg t))
  "cc" 'salorak/whaler-compile
  "co" 'salorak/whaler-compile-other
  "wr" 'whaler-populate-projects-directories
  )
```

## Alternatives

There are tons of great projects out there that perform some sort of navigation between projects. Here is a list of them! Check them out:
- [projectile](https://github.com/bbatsov/projectile): Projectile is a project integration library for Emacs. Its goal is to provide a nice set of features operating on a project level without introducing external dependencies.

Note: If there you use another alternative not listed here, please submit an issue an I gladly add it here ;)
