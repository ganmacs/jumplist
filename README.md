# jumplist.el ![melpa badge][melpa-badge]

jumplist.el is Emacs port of jumplist(`Ctrl-O`) in vim.
And, as an additional feature, This package provides an another behavior mode(ex-mode) that move like the browser history.
We will describe how to use ex-mode in the Customize Chapter.

## Requirements

Emacs 24.3 or higher.

## Installation

You can install jumplist.el from [MELPA](https://melpa.org/#/jumplist) with package.el.

install directly:

```sh
$ cd load-path-dir
$ wget https://raw.githubusercontent.com/ganmacs/jumplist/master/jumplist.el
```

After Installation add following to your configuration file(`~/.emacs.d/init.el`, `~/.emacs` etc)

```lisp
(require 'jumplist)
```

## Customize

### Simple Customize

```lisp
(require 'jumplist)
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)
```

### Hooks Customize

If you want to customize hooks. you can customize it by using `jumplist-hook-commands`


```lisp
(custom-set-variables
 '(jumplist-hook-commands
   '(helm-swoop dired-jump helm-for-files
     isearch-forward end-of-buffer beginning-of-buffer
     find-file)))
```

### Ex-mode Customize

If you want to use ex-mode, add this code.

```lisp
(custom-set-variables
 '(jumplist-ex-mode t))
 ```

### Sample Customize

This is my customize.

```lisp
(require 'jumplist)
(global-set-key (kbd "C-<") 'jumplist-previous)
(global-set-key (kbd "C->") 'jumplist-next)
(custom-set-variables
 '(jumplist-hook-commands
   '(helm-swoop dired-jump helm-for-files
     isearch-forward end-of-buffer beginning-of-buffer
     find-file))
 '(jumplist-ex-mode t))
```


[melpa-badge]: https://melpa.org/packages/jumplist-badge.svg
