;;;###autoload
(defun filter (pred lst &optional acc)
  "Return a new list containing only the elements of LST
which satisfy PRED."
  (if (null lst)
      (reverse acc)
    (filter pred
	    (cdr lst)
	    (if (funcall pred (car lst))
		(cons (car lst) acc)
	      acc))))

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

;;;###autoload
(defun bits (x n-bits &optional acc)
  (if (= 0 n-bits)
      acc
    (bits (ash x -1) (1- n-bits) (cons (logand x 1) acc))))

(defun bits->number (bits &optional acc)
  (or acc (setq acc 0))
  (if bits
      (bits->number (cdr bits) (logior (ash acc 1) (car bits)))
    acc))

(defun normalize-bits (bits)
  (mapcar (lambda (x) (= x 1)) bits))

;;;###autoload
(defun make-truth-table (func n-bits)
  (mapcar (lambda (x)
            (let* ((vals (bits x n-bits '()))
                   (res (apply func (normalize-bits vals))))
              `(,@vals ,@(if (consp res)
                             res
                           (list res)))))
          (iota (expt 2 n-bits) 0)))

(provide 'mfs-util)
