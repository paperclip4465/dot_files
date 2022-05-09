(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(eldoc-mode 1)
(column-number-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode
	    (lambda () (display-line-numbers-mode 0))))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq gc-cons-threshold (* 50 1000 1000))
(setq vc-follow-symlinks t)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq comp-async-report-warnings-errors nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

(show-paren-mode t)

(setq calendar-date-style 'iso)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(require 'use-package)

(defun mfs/evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  repl-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (evil-mode 1)
  :hook (evil-mode . mfs/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  (evil-global-set-key 'motion "j" 'evil-next-line)
  (evil-global-set-key 'motion "k" 'evil-previous-line)

  (evil-global-set-key 'motion (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'motion (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'motion (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'motion (kbd "C-k") 'evil-window-up)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
	(remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package company
  :hook ((prog-mode) . company-mode))

(use-package org
  :mode ("\\.org$" . org-mode))

(use-package org-roam
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
