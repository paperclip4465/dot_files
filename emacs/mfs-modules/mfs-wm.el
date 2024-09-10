(setq display-buffer-alist '())

;; Get info/man windows to open in a nice way...
(add-to-list 'display-buffer-alist
	     `(,(rx (or "*info*"
			"*Help*"
			"WoMan"))
	       (display-buffer-in-side-window)
	       (side . right)
	       (slot . 0)
	       (window-width . 100)
	       (window-parameters
		(no-delete-other-windows . t))))

;; get compilation buffers to pop up on bottom
(add-to-list 'display-buffer-alist
	     `(,(rx (| "compilation"
		       "xref"))
	       (display-buffer-reuse-mode-window
		display-buffer-in-side-window)
	       (reusable-frames . t)
	       (side . bottom)
	       (slot . 0)
	       (window-height . 20)
	       (window-parameters
		(no-delete-other-windows . t))))

;; Reuse windows for these things
(add-to-list 'display-buffer-alist
	     `(,(rx (| "*xref*"
		       "*grep*"
		       "*Occur*"))
	       display-buffer-reuse-window
	       (inhibit-same-window . nil)))

;; Make magit-revision windows split horizontally and open below
(add-to-list 'display-buffer-alist
	     `(,(rx (| "magit-revision"))
	       display-buffer-below-selected
	       (window-height . 100)))

;; Move point to end of buffer in compilation mode
;; by default
(add-hook 'compilation-start-hook
	  (lambda ()
	    (setq compilation-scroll-output t)))

(require 'windmove)

;;;###autoload
(defun mfs/wm-integration (command)
  "Window manager integration function.
The window manager calls this via emacsclient
to move by Emacs window instead of by window manager window.
COMMAND is a 'windmove' command."
  (pcase command
    ((rx bos "focus")
     (let )
     (funcall
      (intern (concat "evil-window-"
		      (elt (split-string command) 1)))
      1))
    (- (error command))))

(provide 'mfs-wm)
