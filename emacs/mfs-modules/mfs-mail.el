(defun homep ()
  (string-match-p user-login-name "mitchell"))

(setq user-mail-address "mitchellschmeisser@librem.one")
(setq smtpmail-smtp-server "smtp.librem.one")
(setq smtpmail-servers-requiring-authorization "^.librem\.one")

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.librem.one"
      smtpmail-smtp-service 587
      smtpmail-local-domain "homepc")

;; auto-complete emacs address using bbdb command, optional
(add-hook 'message-mode-hook
          (lambda ()
            (flyspell-mode t)))

;;;
;;; GNUS

(setq gnus-select-method '(nntp "news.gwene.org")) ;; Read feeds/atom through gwene
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "librem.one"
                      (nnimap-address "imap.librem.one")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl)
                      (nnir-search-engine imap)
                      (nnmail-expiry-wait 90)))

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

; NO 'passive
(setq gnus-use-cache t)

;;;
;;; mu4e

(use-package mu4e
  :bind
  (("C-c m" . mu4e))
  :custom
  (mu4e-attachment-dir (mfs-home-path "/Mail/attachments"))
  (mu4e-bookmarks
   '(( :name  "Unread messages"
       :query "flag:unread AND NOT flag:trashed"
       :key ?u)
     ( :name "Today's messages"
       :query "date:today..now"
       :key ?t)
     ( :name "AYD"
       :key ?a
       :query "ayd")
     ( :name "librem"
       :key ?l
       :query "librem.one"))))

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
