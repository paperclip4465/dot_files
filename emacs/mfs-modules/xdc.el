;;; xdc-mode -- Summary
;;;; Dumb minor mode for editing xdc constraints files
;;; Commentary:
;;; Code:
(defvar xdc-mode-font-lock-keywords
  '(("\#.*" . font-lock-comment-face)
    ("\".*\"" . font-lock-string-face)
    ("^set_property" . font-lock-type-face)
    ("get_ports" . font-lock-type-face)
    ("PACKAGE_PIN" . font-lock-constant-face)
    ("IOSTANDARD" . font-lock-constant-face)))


(define-derived-mode xdc-mode text-mode
  "xdc"
  (set (make-local-variable 'font-lock-defaults)
       '(xdc-mode-font-lock-keywords t)))

;;;###autoload
(defun xdc-make-pin-array (name pins)
  "Create a new pin array named NAME which is mapped to PINS."
  (interactive "sArray Name: \nsPins: ")
  (let* ((pin-list (split-string pins))
	 (i 0))
    (dolist (pin pin-list)
      (make-pin nil (string-join (list name "_" (number-to-string i))) pin)
      (setq i (+ i 1)))))

;;;###autoload
(defun xdc-make-pin (prefix name pin)
  "Create a new pin named NAME which is mapped to PIN.
If PREFIX is given make NAME an array which contains PIN(s)."
  (interactive "P\nsName: \nsPin: ")
  (if prefix
      (make-pin-array name pin)
    (insert  (format "set_property PACKAGE_PIN %s [get_ports %s]\n" pin name))))

(add-to-list 'auto-mode-alist '("\\.xdc$" . xdc-mode))

(provide 'xdc)
