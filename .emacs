;; Load elpa packages now
(setq package-enable-at-startup nil)
(package-initialize)

;; Put autosave files and backup files in ~/.emacs.d/
;; -- snarfed.org/gnu_emacs_backup_files
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; Extension files directory
(add-to-list 'load-path (expand-file-name "~/.emacs.d/extensions"))

;; Use shell PATH
;; -- http://stackoverflow.com/questions/6411121
(defun set-path-from-shell ()
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-path-from-shell)

;; Edit settings
(set-cursor-color "#000000")
(set-face-attribute  'default nil :family "Menlo" :height 120
                     :foreground "#222222" :background "ffffff")
(set-face-foreground 'font-lock-type-face          "#333333")
(set-face-foreground 'font-lock-function-name-face "#333333")
(set-face-foreground 'font-lock-variable-name-face "#333333")
(set-face-foreground 'font-lock-constant-face      "#333333")
(set-face-foreground 'font-lock-comment-face       "#102090")
(set-face-foreground 'font-lock-string-face        "#105010")
(set-face-foreground 'font-lock-keyword-face       "#909090")
(set-face-foreground 'font-lock-builtin-face       "#909090")
(set-face-foreground 'button                       "#900090")
(set-face-foreground 'minibuffer-prompt            "#900090")
(set-face-background 'region                       "#dddddd")

(setq-default indent-tabs-mode nil)

;; Body centering
(defun center-body ()
  (let ((margin (max 0 (/ (- (window-width) (+ fill-column 2)) 2))))
    (set-window-margins (get-buffer-window) margin margin)))

(defun uncenter-body ()
  (set-window-margins (get-buffer-window) 0 0))

(defun body-center-mode ()
  (interactive)
  (if (not (car (window-margins)))
      (progn
        (center-body)
        (add-hook 'window-configuration-change-hook 'center-body nil 1))
    (uncenter-body)
    (remove-hook 'window-configuration-change-hook 'center-body 1)))

;; Copy/paste
(defun pt-pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pt-pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
    (shell-command-on-region
     (point) (mark) "pbcopy")
    (kill-buffer "*Shell Command Output*")))

(global-set-key "\C-x\C-y" 'pt-pbpaste)
(global-set-key "\C-x\M-w" 'pt-pbcopy)

;; Key bindings
(global-set-key "\C-cf" 'auto-fill-mode)
(global-set-key "\C-cc" 'body-center-mode)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\C-c\C-a" 'move-beginning-of-line)

;; GUI is evil
;; -- github.com/benskuhn/dot-emacs
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(column-number-mode 1)

;; Hide successful compilations
;; -- github.com/antonj/.emacs.d/blob/master/aj-compilation.el
(require 'aj-compilation)

;; mu4e
;; (load "mu4e.el")

;; c-mode
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; coffee-mode
(require 'coffee-mode)
(custom-set-variables '(coffee-tab-width 4))
(add-hook 'coffee-mode-hook (lambda () (coffee-cos-mode 1)))
(add-hook 'coffee-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; go-mode
(require 'go-mode-load)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; javascript-mode
(add-hook 'javascript-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; latex-mode
(load "latex-mode.el")

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.td\\'" . markdown-mode))
(setq markdown-enable-math t)

;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-hook 'scss-mode-hook (lambda () (setq show-trailing-whitespace t)))
