;; Toggle visual-line-mode and adaptive-wrap-prefix-mode simultaneously
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))


;; Text editing settings
(defun my-text-mode-hook ()
  (interactive)
  (visual-line-mode t)
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
