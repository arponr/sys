;; Load packages
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Edit settings
(set-cursor-color "#000000")
(set-face-attribute 'fringe nil :background "#ffffff" :foreground "#ffffff")
(set-face-attribute  'default nil :family "Input" :height 120
                     :foreground "#333333" :background "ffffff")
(set-face-foreground 'font-lock-type-face          "#333333")
(set-face-foreground 'font-lock-function-name-face "#333333")
(set-face-foreground 'font-lock-variable-name-face "#333333")
(set-face-foreground 'font-lock-constant-face      "#333333")
(set-face-foreground 'font-lock-comment-face       "#3333bb")
(set-face-foreground 'font-lock-string-face        "#337733")
(set-face-foreground 'font-lock-keyword-face       "#999999")
(set-face-foreground 'font-lock-builtin-face       "#999999")
(set-face-foreground 'button                       "#900090")
(set-face-foreground 'minibuffer-prompt            "#900090")
(set-face-background 'region                       "#eeeeee")

(global-visual-line-mode t)
(setq-default indent-tabs-mode nil)

(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\C-x\C-a" 'move-beginning-of-line)

;; Text edit settings hook
(defun text-buffer-hook ()
  (interactive)
  (setq buffer-face-mode-face '(:height 140))
  (buffer-face-mode)
  (setq line-spacing 4)
  (local-set-key "\C-a" 'move-beginning-of-line)
  (local-set-key "\C-x\C-a" 'back-to-indentation))

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

(global-set-key "\C-cc" 'body-center-mode)

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

;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; javascript-mode
(add-hook 'javascript-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; latex-mode
(load "latex-mode.el")

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t)
(add-hook 'markdown-mode-hook 'text-buffer-hook)

;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
