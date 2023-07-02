;; Some basic extensions to emacs-elf-mode
;; since I use it surprisingly often.
(defvar-local elf-mode nil)

(defvar elf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-e y") #'elf-symbols)
    (define-key map (kbd "C-e S") #'elf-strings)
    (define-key map (kbd "C-e x") #'elf-hex-dump)
    map)
  "Key map for `elf-mode`.")

(defvar elf-mode-command "readelf --syms -W %s"
  "The shell command to use for `elf-mode'.")

(defun readelf (&optional args file)
  "Return results of readelf command which operates on the current
buffer file."
  (shell-command-to-string
   (format "readelf %s %s" args (or file (buffer-file-name)))))

(defun objdump (&optional args file)
  "Return results of readelf command which operates on the current
buffer file."
  (shell-command-to-string
   (format "objdump %s %s" args (or file (buffer-file-name)))))

(defun insert-readelf (args)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (readelf args))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(defun elf-sections ()
  (interactive)
  (insert-readelf "--sections -W"))

(defvar test-regex (rx ;; index
		    "["
		    (group (one-or-more
			    (or whitespace
				digit)))
		    "]"
		    (one-or-more whitespace)
		    ;; name
		    (group
		     (zero-or-more
		      (or "."
			  alphabetic
			  "_")))
		    (one-or-more whitespace)
		    ;; type
		    (group
		     (one-or-more alphabetic))))

(defun test-section-match ()
  (interactive)
  (goto-char (line-beginning-position))
  (re-search-forward test-regex (line-end-position) t))


(defun elf-section-index (section)
  "Return name element from SECTION as parsed by elf-section-list"
  (elt section 0))

(defun elf-section-name (section)
  "Return name element from SECTION as parsed by elf-section-list"
  (elt section 1))

(defun elf-section-type (section)
  "Return name element from SECTION as parsed by elf-section-list"
  (elt section 2))

(defun elf-read-sections (file)
  "Parse the output of readelf --sections FILE and return a list of
sections"
  (interactive)
  (let* ((regex (rx ;; index
		 "["
		 (group (one-or-more
			    (or whitespace
				digit)))
		 "]"
		 (one-or-more whitespace)
		 ;; name
		 (group
		  (zero-or-more
		   (or "."
		       alphabetic
		       "_")))
		 (one-or-more whitespace)
		 ;; type
		 (group
		  (one-or-more alphabetic))))
	 (sections (readelf "--sections -W" file))
	 acc)
    (with-temp-buffer
      (insert sections)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(when (re-search-forward regex (line-end-position) t)
	  (push (list (string-to-number (match-string 1))
		      (match-string 2)
		      (match-string 3))
		acc))
	(forward-line)))
    acc))

(defun elf-section-minibuffer-complete (file)
  (let* ((names (mapcar #'elf-section-name (elf-read-sections file)))
	 (section-at-point (member (word-at-point) names)))
    (list file (completing-read "Section: " names nil t
				(when section-at-point
				  (word-at-point))))))



(defun elf-disassemble (file section)
  (interactive (list
		(buffer-file-name)
		(elf-section-minibuffer-complete (buffer-file-name))))
  (let ((dis (objdump (concat "--disassemble=" section) file))
	(inhibit-read-only t))
    (erase-buffer)
    (insert dis)
    (set-buffer-modified-p nil)
    (read-only-mode 1)))


(defun elf-file-header()
  (interactive)
  (insert-readelf "--file-header -W"))


;;;###autoload
(defun elf-mode ()
  (interactive)
  (let ((inhibit-read-only t))
    (if elf-mode
	(progn
	  (erase-buffer)
	  (insert-file-contents (buffer-file-name))
	  (setq elf-mode nil))
      (setq elf-mode t)
      (erase-buffer)
      (insert (shell-command-to-string
	       (format elf-mode-command (buffer-file-name)))))
    (set-buffer-modified-p nil)
    (read-only-mode 1)))

(provide 'elf-mode)
