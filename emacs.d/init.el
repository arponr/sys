;; Load lisp files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/lisp"))


;; Load packages
(load "packages.el")


;; Put autosave files and backup files in ~/.emacs.d/
;; -- snarfed.org/gnu_emacs_backup_files
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))


;; Use shell PATH
;; -- http://stackoverflow.com/questions/6411121
(defun set-path-from-shell ()
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-path-from-shell)


;; Edit settings
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq-default indent-tabs-mode nil)
(column-number-mode t)


;; Follow symlinks
(setq vc-follow-symlinks t)


;; Hide successful compilations
;; -- github.com/antonj/.emacs.d/blob/master/aj/aj-compilation.el
(require 'aj-compilation)

(load "funcs.el")
(load "keys.el")
(load "modes.el")


(load-theme 'arp t)
(server-start)
