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


(provide 'mfs-mail)
