(require 'shr)
(require 'eww)

(defgroup mfs-eww ()
  "Tweaks for EWW."
  :group 'eww)

;;;; Basic setup

(defun mfs-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
		  (plist-get eww-data :url)
		(plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(add-hook 'eww-after-render-hook #'mfs-eww--rename-buffer)
(advice-add 'eww-back-url :after #'mfs-eww--rename-buffer)
(advice-add 'eww-forward-url :after #'mfs-eww--rename-buffer)

(defvar mfs-eww-visited-history '()
  "History of visited URLs.")

(defcustom mfs-eww-save-history-file
  (locate-user-emacs-file "mfs-eww-visited-history")
  "File to save the value of `mfs-eww-visited-history'."
  :type 'file
  :group 'mfs-eww)

(defcustom mfs-eww-save-visited-history nil
  "Whether to save `mfs-eww-visited-history'.
If non-nil, save the value of `mfs-eww-visited-history' in
`mfs-eww-save-history-file'."
  :type 'boolean
  :group 'mfs-eww)

(defcustom mfs-eww-list-history-buffer "*mfs-eww-history*"
  "Name of buffer for `mfs-eww-list-history'."
  :type 'string
  :group 'mfs-eww)


;; These history related functions are adapted from eww.
(defun mfs-eww--save-visited-history ()
  "Save the value of `mfs-eww-visited-history' in a file.
The file is determined by the variable `mfs-eww-save-history-file'."
  (when mfs-eww-save-visited-history
    (with-temp-file mfs-eww-save-history-file
      (insert (concat ";; Auto-generated file;"
		      " don't edit -*- mode: lisp-data -*-\n"))
      (pp mfs-eww-visited-history (current-buffer)))))

(defun mfs-eww--read-visited-history (&optional error-out)
  "Read history from `mfs-eww-save-history-file'.
If ERROR-OUT, signal `user-error' if there is no history."
  (when mfs-eww-save-visited-history
    (let ((file mfs-eww-save-history-file))
      (setq mfs-eww-visited-history
	    (unless (zerop
		     (or (file-attribute-size (file-attributes file))
			 0))
	      (with-temp-buffer
		(insert-file-contents file)
		(read (current-buffer)))))
      (when (and error-out (not mfs-eww-visited-history))
	(user-error "No history is defined")))))

(unless mfs-eww-visited-history
  (mfs-eww--read-visited-history t))

(defun mfs-eww--history-prepare ()
  "Prepare dedicated buffer for browsing history."
  (set-buffer (get-buffer-create mfs-eww-list-history-buffer))
  (mfs-eww-history-mode)
  (let ((inhibit-read-only t)
	start)
    (erase-buffer)
    (setq-local header-line-format
		"Unified EWW and Elpher Browsing History (mfs-eww)")
    (dolist (history mfs-eww-visited-history)
      (setq start (point))
      (insert (format "%s" history) "\n")
      (put-text-property start (1+ start) 'mfs-eww-history history))
    (goto-char (point-min))))

;;;###autoload
(defun mfs-eww-list-history ()
  "Display `mfs-eww-visited-history' in a dedicated buffer.
This is a replacement for `eww-list-histories' (or equivalent),
as it can combine URLs in the Gopher or Gemini mfsocols."
  (interactive)
  (when mfs-eww-visited-history
    (mfs-eww--save-visited-history))
  (mfs-eww--read-visited-history t)
  (pop-to-buffer mfs-eww-list-history-buffer)
  (mfs-eww--history-prepare))


(defvar mfs-eww-history-kill-ring nil
  "Store the killed history element.")

(defun mfs-eww-history-kill ()
  "Kill the current history."
  (interactive)
  (let* ((start (line-beginning-position))
	 (history (get-text-property start 'mfs-eww-history))
	 (inhibit-read-only t))
    (unless history
      (user-error "No history on the current line"))
    (forward-line 1)
    (push (buffer-substring start (point))
	  mfs-eww-history-kill-ring)
    (delete-region start (point))
    (setq mfs-eww-visited-history (delq history
					 mfs-eww-visited-history))
    (mfs-eww--save-visited-history)))

(defun mfs-eww-history-yank ()
  "Yank a previously killed history to the current line."
  (interactive)
  (unless mfs-eww-history-kill-ring
    (user-error "No previously killed history"))
  (beginning-of-line)
  (let ((inhibit-read-only t)
	(start (point))
	history)
    (insert (pop mfs-eww-history-kill-ring))
    (setq history (get-text-property start 'mfs-eww-history))
    (if (= start (point-min))
	(push history mfs-eww-visited-history)
      (let ((line (count-lines start (point))))
	(setcdr (nthcdr (1- line) mfs-eww-visited-history)
		(cons history (nthcdr line
				      mfs-eww-visited-history)))))
    (mfs-eww--save-visited-history)))

(defun mfs-eww-history-browse ()
  "Browse the history under point."
  (interactive)
  (let ((history (get-text-property (line-beginning-position)
				     'mfs-eww-history)))
    (unless history
      (user-error "No history on the current line"))
    (quit-window)
    (mfs-eww history)))

(defvar mfs-eww-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") 'mfs-eww-history-kill)
    (define-key map (kbd "C-y") 'mfs-eww-history-yank)
    (define-key map (kbd "<RET>") 'mfs-eww-history-browse)

    (easy-menu-define nil map
      "Menu for `mfs-eww-history-mode-map'."
      '("mfs-eww history"
	["Exit" quit-window t]
	["Browse" mfs-eww-history-browse
	 :active (get-text-property (line-beginning-position)
				    'mfs-eww-history)]
	["Kill" mfs-eww-history-kill
	 :active (get-text-property (line-beginning-position)
				    'mfs-eww-history)]
	["Yank" mfs-eww-history-yank
	 :active mfs-eww-history-kill-ring]))
    map))

(define-derived-mode mfs-eww-history-mode
  special-mode
  "mfs-eww-history"
  "Mode for listing history.

\\{mfs-eww-history-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

(defun mfs-eww--record-history ()
  "Store URL in `mfs-eww-visited-history'.
To be used by `eww-after-render-hook'."
  (let ((url (plist-get eww-data :url)))
    (add-to-history 'mfs-eww-visited-history url)))

(defun mfs-eww--get-current-url ()
  "Return the current-page's URL."
  (cond ((eq major-mode 'eww-mode)
	 (plist-get eww-data :url))))
