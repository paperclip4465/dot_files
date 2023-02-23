(require 'use-package)

(setq resize-mini-windows t)
(setq read-answer-short t)
(setq echo-keystrokes 0.25)
(setq kill-ring-max 60)               ; Keep it small
(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
(minibuffer-electric-default-mode 1)


(use-package marginalia
  :config
  (setq marginalia-max-relative-age 0)
  (marginalia-mode 1))

(use-package savehist
  :hook
  ((after-init) . #'savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

(use-package vertico
  :config
  (setq vertico-scroll-margin 0)
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t))

(provide 'mfs-completion)
