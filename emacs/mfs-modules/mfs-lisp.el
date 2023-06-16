(require 'use-package)

(add-hook 'before-save-hook
	  (lambda ()
	    (when (member major-mode '(list scheme-mode lisp-mode emacs-lisp-mode))
	      (untabify (point-min) (point-max)))))


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

(use-package geiser
  :hook ((geiser-repl-mode) . 'company-mode))

(use-package geiser-guile)

(provide 'mfs-lisp)
