(server-start)

(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use yyyy/mm/dd
(setq calendar-date-style 'iso)

(add-hook 'prog-mode-hook
	  (lambda () (interactive)
	    (display-line-numbers-mode 1)
	    (setq show-trailing-whitespace 1)
	    (setq display-line-numbers 'relative)))

(show-paren-mode t)
(column-number-mode t)
(eldoc-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-h"))
(global-unset-key (kbd "C-x C-c"))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))


(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(require 'use-package)

(use-package gruvbox
  :config (load-theme 'gruvbox-dark-soft t))


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
  (evil-global-set-key 'motion (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'motion (kbd "C-k") 'evil-window-up)
  (evil-global-set-key 'motion (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'motion (kbd "C-h") 'evil-window-left)
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

(use-package which-key)

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
  (("C-c C-k" . sly-compile-file)
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
  (("C-l" . paredit-forward-slurp-sexp)
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
   ("C-x b" . ivy-switch-buffer)))

(use-package geiser)
(use-package geiser-guile)
(use-package guix
  :hook
  ((shell-mode) . 'guix-build-log-minor-mode))

(use-package company
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
  :bind
  (("C-c a" . org-agenda)
   ("C-c l" . org-store-link))
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

(use-package gruvbox
  :config
  (load-theme 'gruvbox t))

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


(defvar kconfig-mode-font-lock-keywords
  '(("^[\t, ]*\\_<bool\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<int\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<boolean\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<tristate\\_>" . font-lock-type-face)
    ("^[\t, ]*\\_<depends on\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<select\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<help\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<---help---\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<default\\_>" . font-lock-variable-name-face)
    ("^[\t, ]*\\_<range\\_>" . font-lock-variable-name-face)
    ("^\\_<config\\_>" . font-lock-constant-face)
    ("^\\_<comment\\_>" . font-lock-constant-face)
    ("^\\_<menu\\_>" . font-lock-constant-face)
    ("^\\_<endmenu\\_>" . font-lock-constant-face)
    ("^\\_<if\\_>" . font-lock-constant-face)
    ("^\\_<endif\\_>" . font-lock-constant-face)
    ("^\\_<menuconfig\\_>" . font-lock-constant-face)
    ("^\\_<source\\_>" . font-lock-keyword-face)
    ("\#.*" . font-lock-comment-face)
    ("\".*\"$" . font-lock-string-face)))

(defvar kconfig-headings
  '("bool" "int" "boolean" "tristate" "depends on" "select"
    "help" "---help---" "default" "range" "config" "comment"
    "menu" "endmenu" "if" "endif" "menuconfig" "source"))

(defun kconfig-outline-level ()
  (looking-at "[\t ]*")
  (let ((prefix (match-string 0))
	(result 0))
    (dotimes (i (length prefix) result)
      (setq result (+ result
		      (if (equal (elt prefix i) ?\s)
			  1 tab-width))))))

(define-derived-mode kconfig-mode text-mode
  "kconfig"
  (set (make-local-variable 'font-lock-defaults)
       '(kconfig-mode-font-lock-keywords t))
  (set (make-local-variable 'outline-regexp)
       (concat "^[\t ]*" (regexp-opt kconfig-headings)))
  (set (make-local-variable 'outline-level)
       'kconfig-outline-level))

(add-to-list 'auto-mode-alist '("Kconfig" . kconfig-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(safe-local-variable-values
   '((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (eval let
	   ((root-dir-unexpanded
	     (locate-dominating-file default-directory ".dir-locals.el")))
	   (when root-dir-unexpanded
	     (let*
		 ((root-dir
		   (expand-file-name root-dir-unexpanded))
		  (root-dir*
		   (directory-file-name root-dir)))
	       (unless
		   (boundp 'geiser-guile-load-path)
		 (defvar geiser-guile-load-path 'nil))
	       (make-local-variable 'geiser-guile-load-path)
	       (require 'cl-lib)
	       (cl-pushnew root-dir* geiser-guile-load-path :test #'string-equal))))
     (eval setq-local guix-directory
	   (locate-dominating-file default-directory ".dir-locals.el"))))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'windmove)

(defun mfs/wm-integration (command)
  (pcase command
    ((rx bos "focus")
     (windmove-do-window-select
      (intern (elt (split-string command) 1))))
    (- (error command))))
