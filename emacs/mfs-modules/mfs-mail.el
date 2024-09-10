(setq message-send-mail-function 'smtpmail-send-it)

;; auto-complete emacs address using bbdb command, optional
(add-hook 'message-mode-hook
	  (lambda ()
	    (flyspell-mode t)
	    (company-mode t)))

;;;
;;; mu4e

(defvar mfs-mu4e-contexts
  `(,(make-mu4e-context
      :name "HVGC"
      :enter-func (lambda () (mu4e-message "Entering Happy Valley Go Club context"))
      :leave-func (lambda () (mu4e-message "Leaving Happy Valley Go Club context"))
      :match-func (lambda (msg)
		    (when msg
		      (or
		       (mu4e-message-contact-field-matches msg
			'(:to :from :bcc :cc) "mitchell@happyvalleygo.org"))))
      :vars '( ( user-mail-address . "mitchell@happyvalleygo.org")
	       ( user-full-name . "Mitchell Schmeisser")
	       ( message-user-organization . "Happy Valley Go Club")
	       ( message-signature . t)
	       ( message-signature-file . "~/.hvgc-signature")
	       ( smtpmail-default-smtp-server "smtp.titan.email")
	       ( smtpmail-servers-requiring-authorization "^.happvalleygo\.org")
	       ( smtpmail-smtp-service 465)))
    ,(make-mu4e-context
      :name "Personal"
      :enter-func (lambda () (mu4e-message "Entering Personal context"))
      :leave-func (lambda () (mu4e-message "Leaving Personal context"))
      :match-func (lambda (msg)
		    (when msg
		      (or
		       (mu4e-message-contact-field-matches msg
			'(:to :from :cc :bcc) "mitchellschmeisser@librem.one"))))
      :vars '( ( user-mail-address . "mitchellschmeisser@librem.one")
	       ( user-full-name . "Mitchell Schmeisser")
	       ( message-user-organization . "")
	       ( message-signature . t)
	       ( message-signature-file . "~/.personal-signature")
	       ( smtpmail-default-smtp-server "smtp.librem.one")
	       ( smtpmail-servers-requiring-authorization "^.librem\.one")
	       ( smtpmail-smtp-service 567)))))


(use-package mu4e
  :bind
  (("C-c m" . mu4e)
   ("C-x m" . mu4e-compose-new))
  :custom
  (mu4e-attachment-dir (mfs-home-path "/Mail/attachments"))
  (mu4e-bookmarks
   '(( :name  "Unread messages"
       :query "flag:unread AND NOT flag:trashed AND NOT unsubscribe"
       :key ?u)
     ( :name "Today's messages"
       :query "date:today..now"
       :key ?t)
     ( :name "AYD"
       :key ?a
       :query "ayd")
     ( :name "librem"
       :key ?l
       :query "librem.one")
     ( :name "psu"
       :key ?p
       :query "psu.edu")
     ( :name "hvgc"
       :key ?h
       :query "mitchell@happyvalleygo.org")
     ( :name "junk"
       :hide-unread t
       :key ?j
       :query "unsubscribe AND NOT flag:trashed")))
  :config
  (require 'mu4e-org)
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-get-mail-command "offlineimap -c ~/projects/dot_files/mail/offlineimaprc")
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq mu4e-contexts mfs-mu4e-contexts))

;;;
;;; Elfeed

(use-package elfeed
  :init
  (setq elfeed-feeds
	'("https://guix.gnu.org/feeds/blog.atom"
	  "https://zipcpu.com/feed.xml"
	  "https://protesilaos.com/books.xml"
	  "https://protesilaos.com/codelog.xml"
	  "https://hnrss.org/best.atom"))
  :custom
  (elfeed-curl-extra-arguments .  '("-k"))
  :bind
  (("C-x w" . elfeed)))

(provide 'mfs-mail)
