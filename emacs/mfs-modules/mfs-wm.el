(setq display-buffer-alist '())

;; Get info/man windows to open in a nice way...
(setq display-buffer-alist
      `((,(rx (or "*info*"
		  "*Help*"
		  "WoMan"))
	 (display-buffer-in-side-window)
	 (side . right)
	 (slot . 0)
	 (window-width . 100)
	 (dedicated . t)
	 (window-parameters
	  (mode-line-format . none)
	  (no-delete-other-windows . t)))

	(,(rx (| "compilation"
		 "xref"))
	 (display-buffer-reuse-mode-window
	  display-buffer-in-side-window)
	 (dedicated . t)
	 (reusable-frames . t)
	 (side . bottom)
	 (slot . 0)
	 (window-height . 20)
	 (window-parameters
	  (no-delete-other-windows . t)))

	(,(rx (| "*xref*"
		 "*grep*"
		 "*Occur*"))
	 display-buffer-reuse-window
	 (inhibit-same-window . nil))

	(,(rx (| "magit-revision"))
	 (display-buffer-reuse-mode-window
	  display-buffer-below-selected)
	 (window-height . 100)
	 (dedicated . t))))

(require 'windmove)

;;;###autoload
(defun mfs/wm-integration (command)
  "Window manager integration function.
The window manager calls this via emacsclient
to move by Emacs window instead of by window manager window.
COMMAND is a 'windmove' command."
  (pcase command
    ((rx bos "focus")
     (windmove-do-window-select
      (intern (elt (split-string command) 1))))
    (- (error command))))

(provide 'mfs-wm)
