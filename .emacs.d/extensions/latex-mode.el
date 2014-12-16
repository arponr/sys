(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(require 'tex)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (set-face-foreground 'font-latex-string-face "#222222")
            (set-face-foreground 'font-latex-italic-face "#222222")
            (set-face-foreground 'font-latex-sedate-face "#909090")
            (set-face-foreground 'font-latex-math-face   "#105010")
            (set-face-attribute  'font-latex-warning-face nil
                                 :foreground "#900090" :weight 'normal)
            (set-face-attribute  'font-latex-sectioning-2-face nil
                                 :foreground "#888888" :family "Menlo" :height 110 :weight 'normal)
            (set-face-attribute  'font-latex-sectioning-3-face nil
                                 :foreground "#888888" :family "Menlo" :height 110 :weight 'normal)))
