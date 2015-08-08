(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-refresh-contents)
(package-initialize)

(setq my-packages '(adaptive-wrap
                    auctex
                    coffee-mode
                    go-mode
                    haskell-mode
                    markdown-mode
                    scss-mode))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))
