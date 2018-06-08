;; disable evil mode in mu4e
(evil-set-initial-state 'mu4e-main-mode 'emacs)
(evil-set-initial-state 'mu4e-headers-mode 'emacs)
(evil-set-initial-state 'mu4e-view-mode 'emacs)

(setq mu4e-view-show-images t)
(setq mu4e-view-show-addresses t)

;; path to our Maildir directory
(setq mu4e-maildir "/home/chris/Maildir")

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("chris@langhans-coding.de" "chris@chris-maier.com" "info@chris-maier.com" "chris.maier@acurana.de"))

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-attachment-dir  "~/Downloads")
(setq mu4e-sent-folder   "/LocalChrisLanghansCoding/INBOX/Sent"
      mu4e-trash-folder  "/LocalChrisLanghansCoding/INBOX/Trash"
      mu4e-refile-folder "/LocalChrisLanghansCoding/INBOX/Archives")

(setq mu4e-contexts
      ;; backtick is necessary. It disables evaluation for every subexpression not preceded by a comma
      `(
         ,(make-mu4e-context
           :name "chris@langhans-coding.de"
           :enter-func (lambda () (mu4e-message "chris@langhans-coding.de"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris@langhans-coding.de")))
           :vars '( (user-mail-address       . "chris@langhans-coding.de" )
                    (user-full-name          . "Chris Langhans" )
                    (mu4e-reply-to-address "chris@langhans-coding.de")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalChrisLanghansCoding/INBOX/Sent")
                    (mu4e-drafts-folder . "/LocalChrisLanghansCoding/INBOX/Drafts")
                    (mu4e-trash-folder . "/LocalChrisLanghansCoding/INBOX/Trash")
                    (mu4e-refile-folder . "/LocalChrisLanghansCoding/INBOX/Archives")
                    ))

        ,(make-mu4e-context
           :name "chris@chris-maier.com"
           ;; :enter-func (lambda () (mu4e-message "chris@chris-maier.com"))
           ;; :leave-func (lambda () (mu4e-message "Leaving Private context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris@chris-maier.com"
                                                               )))
           :vars '( (user-mail-address      . "chris@chris-maier.com"  )
                    (user-full-name         . "Chris Langhans" )
                    (mu4e-reply-to-address "chris@langhans-coding.de")
                    (mu4e-compose-signature . nil)
                    (mu4e-sent-folder . "/LocalChris/INBOX/Sent")
                    (mu4e-drafts-folder . "/LocalChris/INBOX/Drafts")
                    (mu4e-trash-folder . "/LocalChris/INBOX/Trash")
                    (mu4e-refile-folder . "/LocalChris/INBOX/Archives")
                    ))

         ,(make-mu4e-context
           :name "info@chris-maier.com"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "info@chris-maier.com"
                                                               )))
           :vars '( (user-mail-address       . "info@chris-maier.com")
                    (user-full-name          . "Chris Langhans")
                    (mu4e-reply-to-address "chris@langhans-coding.de")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalInfo/INBOX/Sent")
                    (mu4e-drafts-folder . "/LocalInfo/INBOX/Drafts")
                    (mu4e-trash-folder . "/LocalInfo/INBOX/Trash")
                    (mu4e-refile-folder . "/LocalInfo/INBOX/Archives")
                    ))

         ;; ,(make-mu4e-context
         ;;   :name "chris@langhans-coding.de"
         ;;   :enter-func (lambda () (mu4e-message "chris@langhans-coding.de"))
         ;;   :match-func (lambda (msg)
         ;;                 (when msg
         ;;                   (mu4e-message-contact-field-matches msg
         ;;                                                       (list :to :from :bcc :cc) "chris@langhans-coding.de")))
         ;;   :vars '( (user-mail-address       . "chris@langhans-coding.de" )
         ;;            (user-full-name          . "Chris Langhans" )
         ;;            (mu4e-compose-signature  . nil)
         ;;            (mu4e-sent-folder . "/LocalChrisLanghansCoding/Sent")
         ;;            (mu4e-drafts-folder . "/LocalChrisLanghansCoding/Drafts")
         ;;            (mu4e-trash-folder . "/LocalChrisLanghansCoding/Trash")
         ;;            (mu4e-refile-folder . "/LocalChrisLanghansCoding/Archives")
         ;;            ))

         ;; ,(make-mu4e-context
         ;;   :name "Web.de"
         ;;   :enter-func (lambda () (mu4e-message "chris.maier85@web.de"))
         ;;   :match-func (lambda (msg)
         ;;                 (when msg
         ;;                   (mu4e-message-contact-field-matches msg
         ;;                                                       (list :to :from :bcc :cc) "chris.maier85@web.de")))
         ;;   :vars '( (user-mail-address       . "chris.maier85@web.de" )
         ;;            (user-full-name          . "Chris Langhans" )
         ;;            (mu4e-compose-signature  . nil)
         ;;            ;; (mu4e-sent-folder . "/LocalWeb.de/Sent")
         ;;            ;; (mu4e-drafts-folder . "/LocalWeb.de/Drafts")
         ;;            ;; (mu4e-trash-folder . "/LocalWeb.de/Trash")
         ;;            ;; (mu4e-refile-folder . "/LocalWeb.de/Archives")
         ;;            ))

         ;; ,(make-mu4e-context
         ;;   :name "Gmail"
         ;;   :enter-func (lambda () (mu4e-message "otix85@googlemail.com"))
         ;;   :match-func (lambda (msg)
         ;;                 (when msg
         ;;                   (mu4e-message-contact-field-matches msg
         ;;                                                       (list :to :from :bcc :cc) "otix85@googlemail.com")
         ;;                   ))
         ;;   :vars '( (user-mail-address       . "otix85@googlemail.com")
         ;;            (user-full-name          . "Chris Langhans")
         ;;            (mu4e-compose-signature  . nil)
         ;;            ;; (mu4e-sent-folder . "/LocalGmail/Sent")
         ;;            ;; (mu4e-drafts-folder . "/LocalGmail/Drafts")
         ;;            ;; (mu4e-trash-folder . "/LocalGmail/Trash")
         ;;            ;; (mu4e-refile-folder . "/LocalGmail/Archives")
         ;;            ))

         ,(make-mu4e-context
           :name "chris.maier@acurana.de"
           :enter-func (lambda () (mu4e-message "chris.maier@acurana.de"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris.maier@acurana.de")
                           ))
           :vars '( (user-mail-address       . "chris.maier@acurana.de")
                    (user-full-name          . "Chris Langhans")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalAcurana/Sent")
                    (mu4e-drafts-folder . "/LocalAcurana/Drafts")
                    (mu4e-trash-folder . "/LocalAcurana/Trash")
                    (mu4e-refile-folder . "/LocalAcurana/Archives")
                    ))
         ))

;; Modify Header field in mu4e-header
(setq mu4e-headers-fields
      '((:human-date . 12)
        (:flags . 6)
        (:mailing-list . 10)
        (:from . 22)
        (:to . 22)
        (:subject))
      )

;; Bookmark for listing all Inboxes
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name  "Inboxes"
              :query "maildir:/LocalChris/INBOX or maildir:/LocalInfo/INBOX or maildir:/LocalAcurana/INBOX or maildir:/LocalChrisLanghansCoding/INBOX"
              :key ?i))

;; Bookmark for all sent messages
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "Sent"
              :query "maildir:/LocalChris/INBOX/sent or maildir:/LocalInfo/INBOX/sent or maildir:/LocalAcurana/INBOX/sent or maildir:/LocalChrisLanghansCoding/INBOX/sent"
              :key ?s))

;; enable reflow on messages
(setq mu4e-compose-format-flowed t)

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'pick-first)

;; Don't ask to quit... ?
(setq mu4e-confirm-quit nil)

;; we substitute sendmail with msmtp
(setq sendmail-program "/usr/bin/msmtp")
;; send mail with sendmail
(setq message-send-mail-function 'message-send-mail-with-sendmail)
;; This is needed to allow msmtp to do its magic:
(setq message-sendmail-f-is-evil 't)
;; need to tell msmtp which account we're using
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-envelope-from 'header)
;; after a message is sent, kill the buffer
(setq message-kill-buffer-on-exit t)

(setq mail-specify-envelope-from t)
(setq mail-envelope-from 'header)

(provide 'mu4e-local)
