(deftheme arp
  "created 2015-08-08")

(defconst arp-white "#ffffff")
(defconst arp-grey-white "#eeeeee")
(defconst arp-grey-light "#999999")
(defconst arp-grey-dark "#333333")
(defconst arp-blue "#3333bb")
(defconst arp-green "#337733")
(defconst arp-red "#993333")

(custom-theme-set-faces
 'arp

 ;; General
 `(default ((t (:family "Input" :height 120 :background ,arp-white :foreground ,arp-grey-dark))))
 `(fringe ((t (:background ,arp-white :foreground ,arp-grey-light))))
 `(font-lock-type-face ((t (:foreground ,arp-grey-dark))))
 `(font-lock-function-name-face ((t (:foreground ,arp-grey-dark))))
 `(font-lock-variable-name-face ((t (:foreground ,arp-grey-dark))))
 `(font-lock-constant-face ((t (:foreground ,arp-grey-dark))))
 `(font-lock-keyword-face ((t (:foreground ,arp-grey-light))))
 `(font-lock-builtin-face ((t (:foreground ,arp-grey-light))))
 `(minibuffer-prompt ((t (:foreground ,arp-grey-light))))
 `(font-lock-comment-face ((t (:foreground ,arp-blue))))
 `(font-lock-string-face ((t (:foreground ,arp-green))))
 `(region ((t (:background ,arp-grey-white))))

 ;; LaTeX
 `(font-latex-string-face ((t (:foreground ,arp-grey-dark))))
 `(font-latex-string-face ((t (:foreground ,arp-grey-dark))))
 `(font-latex-italic-face ((t (:foreground ,arp-grey-dark))))
 `(font-latex-sedate-face ((t (:foreground ,arp-grey-light))))
 `(font-latex-math-face ((t (:foreground ,arp-green))))
 `(font-latex-warning-face ((t (:foreground ,arp-red :weight normal))))
 `(font-latex-sectioning-1-face ((t (:family "Input" :height 120 :weight bold :background ,arp-white :foreground ,arp-grey-dark))))
 `(font-latex-sectioning-2-face ((t (:family "Input" :height 120 :weight bold :background ,arp-white :foreground ,arp-grey-dark))))
 `(font-latex-sectioning-3-face ((t (:family "Input" :height 120 :weight bold :background ,arp-white :foreground ,arp-grey-dark))))
 `(font-latex-sectioning-4-face ((t (:family "Input" :height 120 :weight bold :background ,arp-white :foreground ,arp-grey-dark)))))


(provide-theme 'arp)
