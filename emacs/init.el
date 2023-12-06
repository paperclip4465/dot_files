;; -*- lexical-binding: t; -*-
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

(setq custom-file (locate-user-emacs-file "emacs-custom"))
(setq enable-recursive-minibuffers t)

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

(setq backup-directory-alist '(("." .  "~/.emacs.d/emacs-saves")))

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
  (setq evil-collection-mode-list
	(filter (lambda (x)
		  (not (member x
			       '(mu4e mu4e-conversation))))
		evil-collection-mode-list))
  (evil-collection-init))

(use-package org
  :init
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-cite-global-bibliography
	`(,(locate-user-emacs-file "org-cite/cite.bib")))

  (defun mfs/org-insert-src-block ()
    (interactive)
    (insert "#+begin_src\n\n#+end_src\n")
    (forward-line -1))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c C" . org-capture)
   :map org-mode-map
   ("M-l" . org-metaright)
   ("M-h" . org-metaleft)
   ("M-k" . org-metaup)
   ("M-j" . org-metadown)
   ("C-c s" . mfs/org-insert-src-block))
  :custom
  (org-format-latex-options
   '( :foreground default
      :background default
      :scale 1.7
      :html-foreground "Black"
      :html-background "Transparent"
      :html-scale 1.0
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (scheme . t)))
  (setq org-todo-keywords
	'((sequence "ON-HOLD" "TODO" "IN-PROGRESS" "|" "DONE")
	  (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")))

  (setq org-default-notes-file (concat org-directory "/notes.org")))

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
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
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
		     (ggtags-mode 1)
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
  :custom (org-roam-directory "~/roam-notes")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n I" . org-roam-insert-immediate)
   ("C-c n p" . org-roam-find-project)
   ("C-c n P" . org-roam-capture-project)
   ("C-c n d" . org-roam-dailies-capture-today)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)

  (defun org-roam-insert-immediate (arg &rest args)
    "Insert a new org-roam-node link but do not open the note for editing."
    (interactive "P")
    (let ((args (cons arg args))
	  (org-roam-capture-templates (list (append (car org-roam-capture-templates)
						    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (defun org-roam-tag-filter (tag)
    "Create a lambda which returns true when node contains TAG."
    (lambda (node)
      (member tag (org-roam-node-tags node))))

  (defun org-roam-list-notes-by-tag (tag)
    "Return a list of org roam notes containing TAG"
    (mapcar #'org-roam-node-file
	    (seq-filter (org-roam-tag-filter tag)
			(org-roam-node-list))))

  (defun org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (org-roam-list-notes-by-tag "project")))

  (defun org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
    capture was not aborted."
    ;; Remove hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'org-roam-project-finalize-hook)
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
	(add-to-list 'org-agenda-files (buffer-file-name)))))

  (defvar project-head
    ":PROPERTIES:
:PROJECT-NUMBER: %^{PROJECT-NUMBER}
:SPONSOR: %^{SPONSOR}
:END:
#+title: ${title}
#+category: ${title}
#+filetags: :project:")

  (defvar mfs-project-template
    `("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks.\n\n* Dates\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" ,project-head)
      :unnarrowed t
      :immediate-finish t))

  (defun org-roam-find-project ()
    (interactive)
    ;; add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'org-roam-project-finalize-hook)

    ;; select project file to open
    (let ((tag  "project"))
      (org-roam-node-find
       nil nil
       (org-roam-tag-filter tag))))

  (defun org-roam-capture-project ()
    (interactive)
    ;; add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'org-roam-project-finalize-hook)
    (let ((tag "project"))
      (org-roam-capture-
       :node (org-roam-node-read
	      nil
	      (org-roam-tag-filter tag))
       :templates `(("m" "meeting" entry "** %?\nSCHEDULED: %^{Date}T\n\n"
		     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
					    ,project-head
					    ("Meetings")))
		    ("n" "note" entry "\n** %?"
		     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
					    ,project-head
					    ("Notes")))
		    ("t" "todo" entry "** TODO %?"
		     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
					    ,project-head
					    ("Tasks")))))))

  (setq org-roam-capture-templates
	`(("d" "default" plain
	   "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t
	   :immediate-finish t)
	  ("h" "hardware" plain
	   "* Overview\n\n%?\n\n* Notes\n\n* References\n\n* Projects\n\n"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags: :hardware:")
	   :unnarrowed t
	   :immediate-finish t)
	  ,mfs-project-template))
  (require 'org-roam-dailies)
  (org-roam-refresh-agenda-list))

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

(use-package epa
  :bind
  (:map epa-key-list-mode-map
	("<return>" . #'epa-show-key)))

(use-package pinentry
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(use-package direnv
  :custom
  (direnv-mode t))

(use-package xdc
  :config
  (add-to-list 'auto-mode-alist '("\\.xdc$" . xdc-mode)))

(use-package eglot)

;;; init.el ends here
