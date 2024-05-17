(require 'use-package)


(use-package paredit
  :hook
  (((lisp-mode scheme-mode emacs-lisp-mode) . 'enable-paredit-mode))
  :bind
  (:map paredit-mode-map
        ("C-l" . paredit-forward-slurp-sexp)
        ("C-h" . paredit-forward-barf-sexp)
        ("M-l" . paredit-backward-barf-sexp)
        ("M-h" . paredit-backward-slurp-sexp))
  :config
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t))


;;; Elisp
(let ((map emacs-lisp-mode-map))
  (bind-key (kbd "C-c C-c") 'eval-defun map)
  (bind-key (kbd "C-c C-k") 'eval-buffer map))


;;; Common lisp

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



;;; Scheme

(defvar project-geiser-proc nil)

(use-package geiser
  :init
  :hook ((geiser-repl-mode) . 'company-mode)
  :bind
  (("C-C C-Z" . (lambda ()
                  (interactive)
                  (when (process-live-p project-geiser-proc)
                    (stop-process project-geiser-proc))
                  (setf project-geiser-proc
                        (projectile-run-async-shell-command-in-root
                         "./pre-inst-env guile --listen"))
                  (geiser-connect
                   'guile
                   "localhost"
                   37146))))
  :config
  (setq geiser-guile-load-path '("/home/mfs5173/guix"
                                 "/home/mfs5173/nonguix")))

(use-package geiser-guile)

(provide 'mfs-lisp)
