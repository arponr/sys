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
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (setq show-trailing-whitespace t)))


;; haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;; javascript-mode
(add-hook 'javascript-mode-hook (lambda () (setq show-trailing-whitespace t)))


;; latex-mode
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))


;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(setq markdown-enable-math t)


;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; text-mode
(add-hook 'text-mode-hook 'my-text-mode-hook)
