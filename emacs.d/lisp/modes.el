;; c-mode
(setq c-default-style "bsd"
      c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (setq show-trailing-whitespace t)))


;; coffee-mode
(require 'coffee-mode)
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook (lambda () (coffee-cos-mode 1)))
(add-hook 'coffee-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; css-mode
(setq css-indent-offset 2)

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
(add-to-list 'auto-mode-alist '("\\.td\\'" . markdown-mode))
(setq markdown-enable-math t)
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c e") 'texdown-environment)
            (local-set-key (kbd "C-c i") 'texdown-enumerate)
            (local-set-key (kbd "C-c q") 'texdown-equation)))


;; scss-mode
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save t)


;; text-mode
(add-hook 'text-mode-hook 'my-text-mode-hook)
