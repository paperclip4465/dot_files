;;;###autoload
(defun iota (count &optional start step)
  "Return a list containing COUNT numbers, starting from START
and adding STEP each time.  The default START is 0, the default
STEP is 1"
  (let* ((start (if start start 0))
	 (step (if step step 1))
	 (last (+ start count))
	 (counter 0)
	 (list '())
	 (elt start))
    (while (< counter count)
      (push elt list)
      (setq elt (+ elt step))
      (setq counter (1+ counter)))
    (reverse list)))

(provide 'iota)
