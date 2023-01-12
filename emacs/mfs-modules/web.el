(use-package browse-url
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-secondary-browser-function 'browse-url-default-browser))

(use-package shr
  :config
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.6)
  (setq shr-width fill-column)o
  (setq shr-max-width fill-column)

  (setq shr-discard-aria-hidden t)
  (setq shr-cookie-policy nil))

(use-package url-cookie
  :config
  (setq url-cookie-trusted-urls '".*"))

(use-package eww
  :init
  (mkdir "~/Downloads/eww-downloads")
  :bind
  (:map
   dired-mode-map
   ("C-c E" . #'eww-open-file) ; to render local HTML file
   )
  :config
  (setq browse-url-browser-function #'eww-browse-url)
  (setq eww-restore-desktop t)
  (setq eww-desktop-remove-duplicates t)
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
	'(eww-links-at-point
	  thing-at-point-url-at-point))
  (setq eww-history-limit 150)
  (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-use-external-browser-for-content-type
	"\\`\\(video/\\|audio\\)")
  (setq eww-browse-url-new-window-is-tab nil)
  (setq eww-form-checkbox-selected-symbol "[X]")
  (setq eww-form-checkbox-symbol "[ ]"))

(define-prefix-command 'mfs-eww-map)
(define-key global-map (kbd "C-c w") 'mfs-eww-map)
