;;; Init.el --- emacs start up file
;;; Commentary:
;;; Code:

(server-start)

(load-theme 'modus-operandi)

(defvar mfs-home (getenv "HOME")
  "Home environment variable which is not always the same on every computer.")

(defun mfs-home-path (file)
  "Return the absolute filepath of FILE, a file located in $HOME."
  (concat mfs-home "/" file))

(defun mfs-emacs-path (file)
  "Return absolute path of FILE in 'user-emacs-directory'."
  (concat user-emacs-directory "/" file))

;; Personal Information
(setq user-full-name "Mitchell Schmeisser")

(setq lexical-binding t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

;; Use yyyy/mm/dd
(setq calendar-date-style 'iso)

;; Enable those
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region))
  (put c 'disabled nil))

(put 'overwrite-mode 'disabled t)

(dolist (path '("mfs-lisp" "mfs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))
(setq custom-file (make-temp-file "emacs-custom-"))

(add-hook 'prog-mode-hook
	  (lambda () (interactive)
	    (display-line-numbers-mode 1)
	    (setq show-trailing-whitespace 1)
	    (setq display-line-numbers 'relative)))


(show-paren-mode t)
(column-number-mode t)
(eldoc-mode 1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-x C-c"))

(add-hook 'before-save-hook 'whitespace-cleanup)

(load-theme 'modus-operandi)

(setq backup-directory-alist '(("." . (locate-user-emacs-file "emacs-saves"))))

(require 'mfs-util)

(require 'use-package)

(require 'mfs-wm)

(defun mfs/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
  :hook ((compilation-filter) . mfs/ansi-colorize-buffer))

(defun mfs/evil-hook ()
  "Enable evil mode in these additional modes."
  (dolist (mode '(custom-shell
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
  :after company
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :hook
  ((evil-mode) . 'mfs/evil-hook)
  :config
  (evil-mode t)
  (evil-global-set-key 'motion (kbd "s-l") 'evil-window-right)
  (evil-global-set-key 'motion (kbd "s-k") 'evil-window-up)
  (evil-global-set-key 'motion (kbd "s-j") 'evil-window-down)
  (evil-global-set-key 'motion (kbd "s-h") 'evil-window-left)
  (evil-global-set-key 'motion "j" 'evil-next-line)
  (evil-global-set-key 'motion "k" 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package org
  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-cite-global-bibliography
	`(,(mfs-emacs-path "org-cite/cite.bib")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (scheme . t)))
  (setq org-todo-keywords
	'((sequence "TODO" "NEXT" "IN-PROGRESS" "|" "DONE")))
  (setq org-agenda-files '("~/org/")))

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-tree
  :custom
  (undo-tree-history-directory-alist
   '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t))


(use-package rainbow-delimiters
  :hook ((prog-mode lisp-mode emacs-lisp-mode) . 'rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d ")
  (ivy-mode))

(use-package counsel
  :after ivy
  :config
  (put 'counsel-find-symbol 'no-counsel-M-x t)
  :bind
  (("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-h" . counsel-up-directory-level)
   ("C-l" . ivy-done)
   ;; exit ivy selection with current text ignoring canidates
   ("C-<return>" . (lambda () (interactive) (ivy-alt-done t)))))

(use-package geiser-guile)

(use-package geiser
  :hook ((geiser-repl-mode) . 'company-mode)
  :config
  (setq-default geiser-guile-load-path '("~/guix"
					 "~/mfs-guix-channel/")))

(use-package guix
  :hook
  ((shell-mode) . 'guix-build-log-minor-mode)
  :bind
  (("C-c g" . guix-popup))
  :config
  (require 'emacs-guix)
  (guix-eval-in-repl "(use-modules (emacs-guix packages))"))

(use-package company
  :hook ((prog-mode) . 'company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up arguments list by tabs, not spaces"
  (let* ((anchor (c-lengelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(use-package cc-mode
  :hook ((c-mode . (lambda ()
		     (setq indent-tabs-mode t
			   show-trailing-whitespace t)
		     (c-set-style "linux-tabs-only")))
	 (c-mode-common . (lambda ()
			    ;;; Add kernel style
			    (c-add-style "linux-tabs-only"
					 '("linux" (c-offsets-alist
						    (arglist-cont-nonempty
						     c-lineup-gcc-asm-reg
						     c-lineup-arglist-tabs-only)))))))
  :bind
  (("M-/" . company-complete)))

(use-package evil-org
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (setq org-agenda-start-on-weekday 0))

(use-package org-roam
  :init (setq org-roam-v2-ack t)
  :custom (org-roam-directory "~/RoamNotes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((projects . 10)
			  (recents . 5)
			  (agenda . 5))
	dashboard-week-agenda t))

(use-package dts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.dtsi*$" . dts-mode)))

(add-hook 'dired-load-hook
	  (lambda () (load "dired-x")))

(use-package erc
  :bind
  (("C-c e l" . (lambda () (interactive)
		  (erc))))
  :config
  (setq erc-compute-server "irc.libera.chat"
	erc-compute-nick "mitchell"
	erc-compute-port "6667"))

(set-mouse-color "white")

(use-package kconfig)

(use-package magit)

(require 'mfs-lisp)
(require 'mfs-web)
(require 'mfs-completion)
(require 'mfs-mail)

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package octave
  :init
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  :custom
  (inferior-octave-startup-args '("-i"
				  "--line-editing"))
  :bind
  (:map octave-mode-map
	("C-c C-c" . octave-send-block)
	("C-c C-k" . octave-send-buffer)
	("C-c C-r" . octave-send-region)))

;;; init.el ends here
