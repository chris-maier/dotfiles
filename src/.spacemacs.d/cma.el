;; -*- mode: emacs-lisp -*-
;; This is my personal configuration file
;;
(global-company-mode t)
(yas-global-mode t)

;; Scrolling
(setq scroll-margin 10)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; BACKUP
(defvar backup-dir "~/.spacemacs.d/backups/")
(unless (file-name-directory backup-dir)
  (make-directory backup-dir t))
(setq backup-by-copying t
      backup-directory-alist (list (cons "." backup-dir))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; HISTORY
(setq savehist-file "~/.spacemacs.d/savehist"
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history
                                      ring
                                      grep-history)
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-autosave-interval 60)
(setq-default history-length 1000)
(savehist-mode t)

;; evil escape configuration
(setq-default evil-escape-key-sequence "jk")
(setq evil-escape-unordered-key-sequence t)
(setq-default evil-escape-delay 0.4)

;; move over line endigs
(setq evil-cross-lines t)

;; moving over wrapped lines as they were real once
(evil-define-key 'normal global-map (kbd "j") 'evil-next-visual-line)
(evil-define-key 'normal global-map (kbd "k") 'evil-previous-visual-line)

(evil-define-motion cma/evil-prev-visual-line (count)
  "Move the cursor COUNT screen lines down, or -5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count -5))))

(evil-define-motion cma/evil-next-visual-line (count)
  "Move the cursor COUNT screen lines down, or 5."
  :type exclusive
  (let ((line-move-visual t))
    (evil-line-move (or count 5))))

;; it overloads 'spacemacs/evil-smart-doc-lookup so remap it to "C-k"
(evil-define-key 'normal global-map (kbd "K") 'cma/evil-prev-visual-line)
(evil-define-key 'visual global-map (kbd "K") 'cma/evil-prev-visual-line)

(evil-define-key 'normal global-map (kbd "C-k") 'spacemacs/evil-smart-doc-lookup)

;; it overloads 'evil-join on "J"
(evil-define-key 'normal global-map (kbd "J") 'cma/evil-next-visual-line)
(evil-define-key 'visual global-map (kbd "J") 'cma/evil-next-visual-line)

;; move wordwise with H and L
(evil-define-key 'normal global-map (kbd "H") 'evil-backward-word-begin)
(evil-define-key 'visual global-map (kbd "H") 'evil-backward-word-begin)
(evil-define-key 'normal global-map (kbd "L") 'evil-forward-word-begin)
(evil-define-key 'visual global-map (kbd "L") 'evil-forward-word-begin)

;; do not kill the server on exit
(evil-leader/set-key "q q" 'spacemacs/frame-killer)

;; C/C++ settings
(setq-default c-default-style "bsd"
              c-basic-offset 4
              tab-width 4)

;; hooks
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)

;; example configuration for mu4e
(require 'mu4e)

(setq mu4e-view-show-images t)
;; path to our Maildir directory
(setq mu4e-maildir "/home/chris/.offlineimap")

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("chris#chris-maier.com" "info@chris-maier.com" "otix85@googlemail.com" "chris.maier@acurana.de" "chris.maier85@web.de"))

;; program to get mail; alternatives are 'fetchmail', 'getmail'
;; isync or your own shellscript. called when 'U' is pressed in
;; main view.

;; If you get your mail without an explicit command,
;; use "true" for the command (this is the default)
(setq mu4e-get-mail-command "offlineimap")

(setq mu4e-attachment-dir  "~/Downloads")
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Chris"
           ;; :enter-func (lambda () (mu4e-message "chris@chris-maier.com"))
           ;; :leave-func (lambda () (mu4e-message "Leaving Private context"))
           ;; we match based on the contact-fields of the message
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris@chris-maier.com"
                                                               )))
           :vars '( (user-mail-address      . "chris@chris-maier.com"  )
                    (user-full-name         . "Chris Maier" )
                    (mu4e-compose-signature . nil)
                    ;; ( mu4e-compose-signature .
                    ;;   (concat
                    ;;     "Alice Derleth\n"
                    ;;     "Lauttasaari, Finland\n"))
                    (mu4e-sent-folder . "/LocalChris/INBOX/Sent")
                    (mu4e-drafts-folder . "/LocalChris/INBOX/Drafts")
                    (mu4e-trash-folder . "/LocalChris/INBOX/Trash")
                    (mu4e-refile-folder . "/LocalChris/INBOX/Archives")
                    ))

         ,(make-mu4e-context
           :name "Info"
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "info@chris-maier.com"
                                                               )))
           :vars '( (user-mail-address       . "info@chris-maier.com")
                    (user-full-name          . "Chris Maier")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalInfo/INBOX/Sent")
                    (mu4e-drafts-folder . "/LocalInfo/INBOX/Drafts")
                    (mu4e-trash-folder . "/LocalInfo/INBOX/Trash")
                    (mu4e-refile-folder . "/LocalInfo/INBOX/Archives")
                    ))

         ,(make-mu4e-context
           :name "Web.de"
           :enter-func (lambda () (mu4e-message "chris.maier85@web.de"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris.maier85@web.de")))
           :vars '( (user-mail-address       . "chris.maier85@web.de" )
                    (user-full-name          . "Chris Maier" )
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalWeb.de/Sent")
                    (mu4e-drafts-folder . "/LocalWeb.de/Drafts")
                    (mu4e-trash-folder . "/LocalWeb.de/Trash")
                    (mu4e-refile-folder . "/LocalWeb.de/Archives")
                    ))

         ,(make-mu4e-context
           :name "Gmail"
           :enter-func (lambda () (mu4e-message "otix85@googlemail.com"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "otix85@googlemail.com")
                           ))
           :vars '( (user-mail-address       . "otix85@googlemail.com")
                    (user-full-name          . "Chris Maier")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalGmail/Sent")
                    (mu4e-drafts-folder . "/LocalGmail/Drafts")
                    (mu4e-trash-folder . "/LocalGmail/Trash")
                    (mu4e-refile-folder . "/LocalGmai/Archives")
                    ))

         ,(make-mu4e-context
           :name "Acurana"
           :enter-func (lambda () (mu4e-message "chris.maier@acurana.de"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg
                                                               (list :to :from :bcc :cc) "chris.maier@acurana.de")
                           ))
           :vars '( (user-mail-address       . "chris.maier@acurana.de")
                    (user-full-name          . "Chris Maier")
                    (mu4e-compose-signature  . nil)
                    (mu4e-sent-folder . "/LocalAcurana/Sent")
                    (mu4e-drafts-folder . "/LocalAcurana/Drafts")
                    (mu4e-trash-folder . "/LocalAcurana/Trash")
                    (mu4e-refile-folder . "/LocalAcurana/Archives")
                    ))
         ))

(setq mu4e-context-policy 'pick-first)

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

(setq browse-url-browser-function 'browse-url-generic
       browse-url-generic-program "google-chrome")
(provide 'cma)
