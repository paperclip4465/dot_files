(server-start)

;; Personal Information
(setq user-full-name "Mitchell Schmeisser"
      user-mail-address (if (string-match-p user-login-name "mitchell")
			    "mitchellschmeisser@librem.one"
			  "mfs5173@arl.psu.edu"))

(setq lexical-binding t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Some basic settings
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)

;; Use yyyy/mm/dd
(setq calendar-date-style 'iso)

;; Enable those
(dolist (c '( narrow-to-region narrow-to-page upcase-region downcase-region))
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
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))


(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-saves")))

(defvar ensured-package-list '())

;; Get info windows to open in a nice way...
(add-to-list 'display-buffer-alist
	     '("\\*info\\*"
	       (display-buffer-in-side-window)
	       (side . right)
	       (slot . 0)
	       (window-width . 80)
	       (window-parameters
		(no-delete-other-windows . t))))

;; Reuse windows for these things
(add-to-list 'display-buffer-alist
	  `(,(rx (| "*xref*"
		    "*grep*"
		    "*Occur*"))
	    display-buffer-reuse-window
	    (inhibit-same-window . nil)))

(require 'windmove)

(defun mfs/wm-integration (command)
  (pcase command
    ((rx bos "focus")
     (windmove-do-window-select
      (intern (elt (split-string command) 1))))
    (- (error command))))

(require 'use-package)

(defun mfs/evil-hook ()
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
  (evil-global-set-key 'motion "j" 'evil-next-line)
  (evil-global-set-key 'motion "k" 'evil-previous-line)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection

  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :ensure nil
  :config
  (evil-collection-init))

(use-package which-key
  )

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

(use-package sly

  :init
  (setq inferior-lisp-program "~/.guix-profile/bin/sbcl")
  :bind
  (:map sly-mode-map
	("C-c C-k" . sly-compile-file)
	("C-c C-c" . sly-eval-defun)
	("M-." . sly-edit-definition))
  :config
  (setq sly-autodoc-use-multiline-p t))


(use-package rainbow-delimiters

  :hook ((prog-mode lisp-mode emacs-lisp-mode) . 'rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode 1))

(use-package paredit

  :hook
  (((lisp-mode scheme-mode emacs-lisp-mode) . 'enable-paredit-mode))
  :bind
  (:map paredit-mode-map
	("C-l" . paredit-forward-slurp-sexp)
	("C-h" . paredit-forward-barf-sexp)
	("M-l" . paredit-backward-barf-sexp)
	("M-h" . paredit-backward-slurp-sexp)))

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

(use-package geiser-guile
  )

(use-package geiser

  :hook ((geiser-repl-mode) . 'company-mode))

(use-package guix

  :hook
  ((shell-mode) . 'guix-build-log-minor-mode)
  :bind
  (("C-c g" . guix-popup)))

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
  :config
  (define-key c-mode-map [(tab)] 'company-complete)
  (define-key c++-mode-map [(tab)] 'company-complete))

(use-package org

  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-cite-global-bibliography '("/home/mitchell/.emacs.d/org-cite/cite.bib"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (scheme . t)))
  (setq org-todo-keywords
	'((sequence "TODO" "NEXT" "IN-PROGRESS" "|" "DONE")))
  (setq org-agenda-files '("~/org/")))

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


(use-package embark

    :config
    (define-key global-map (kbd "C-,") #'embark-act)
    (define-key embark-collect-mode-map (kbd "C-,") #'embark-act)
    (let ((map minibuffer-local-completion-map))
      (define-key map (kbd "C-,") #'embark-act)
      (define-key map (kbd "C->") #'embark-become))
    (setq embark-quit-after-action t
	  embark-indicators '(embark-mixed-indicator
			      embark-highlight-indicator)
	  embark-verbose-indicator-excluded-actions
	  '("\\`customize-" "\\(local\\|global\\)-set-key"
	    set-variable embark-cycle embark-keymap-help embark-isearch)
	  embark-verbose-indicator-display-action nil))

(use-package magit
  )
