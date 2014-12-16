(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir       "~/Maildir"
      mu4e-drafts-folder "/drafts"
      mu4e-sent-folder   "/sent"
      mu4e-trash-folder  "/trash")

(setq mu4e-maildir-shortcuts
      '(("/archive" . ?a)
        ("/drafts"  . ?d)
        ("/inbox"   . ?i)
        ("/sent"    . ?s)
        ("/trash"   . ?t)))

(setq mu4e-sent-messages-behavior 'delete
      mu4e-attachment-dir  "~/Downloads"
      user-mail-address "arpon.raksit@gmail.com"
      user-full-name "Arpon Raksit"
      message-signature nil
      mu4e-get-mail-command "offlineimap"
      message-kill-buffer-on-exit t
      ;; mu4e-headers-include-related nil
      ;; mu4e-headers-skip-duplicates t
      mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
      mu4e-split-view 'vertical)

(set-face-foreground 'mu4e-view-link-face "#bbbb88")
(set-face-foreground 'message-header-name "#505050")
(set-face-foreground 'message-header-other "#90aa90")
(set-face-foreground 'message-header-subject "#90aa90")
(set-face-foreground 'message-cited-text "#bbbb88")
(set-face-attribute  'message-header-to nil :foreground "#90aa90" :weight 'normal)
(set-face-attribute  'message-header-cc nil :foreground "#90aa90" :weight 'normal)
(set-face-attribute  'mu4e-view-header-key-face nil :foreground "#90aa90" :weight 'normal)
(set-face-attribute  'mu4e-unread-face nil :foreground "#90aa90" :weight 'normal)
(set-face-attribute  'mu4e-header-highlight-face nil :weight 'normal)

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)


;; check for proper from: address when composing
;; -- github.com/benskuhn/dot-emacs/blob/master/lisp/modes/mu4e.el
(setq my-from-addresses
      '(("gmail"   . "arpon.raksit@gmail.com")
        ("harvard" . "araksit@college.harvard.edu")))
(setq mu4e-user-mail-address-list (mapcar 'cdr my-from-addresses))

(defun choose-from-address ()
  (mu4e-read-option "Choose a From address: " my-from-addresses))
(defun parent-addressed-to (addr)
  (flet ((matches (sym) (mu4e-message-contact-field-matches mu4e-compose-parent-message sym addr)))
    (or (matches :to) (matches :cc) (matches :bcc) (matches :from))))

(defun find-in-list (lst fun default)
  (let ((elt (car lst)))
    (cond
      ((eq lst nil) (funcall default))
      ((funcall fun elt) elt)
      (t (find-in-list (cdr lst) fun default)))))

(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (setq user-mail-address
          (if (eq nil mu4e-compose-parent-message)
              (choose-from-address)
            (find-in-list
             (mapcar 'cdr my-from-addresses)
             'parent-addressed-to
             'choose-from-address)))))
