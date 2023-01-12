(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
    To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
		  (plist-get eww-data :url)
		(plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))
