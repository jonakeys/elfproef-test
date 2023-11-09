(defparameter *multiplies9* '(-1 2 3 4 5 6 7 8 9))
(defparameter *multiplies8* '(-1 2 3 4 5 6 7 8))

(defun is-bsn-valid (bsn)
  "Test if bsn is valid (with elfproef)"
  (let ((use-list *multiplies9*)
		(sum 0))
	(when (and (>= bsn 10000000) (< bsn 1000000000))
	  (if (< bsn 100000000)
		  (setf use-list *multiplies8*))
	  (dolist (i use-list sum)
		(setf sum (+ sum (* i (mod bsn 10))))
		(setf bsn (truncate bsn 10))))
	(if (zerop sum)
		nil
		(eql 0 (mod sum 11)))))

(defun find-valid-pairs ()
  "Find a valid bsn pair. I.e. a valid nine number and eight number
where the last digit is dropped."
  (let ((counter 0) (sum-valid 0) (total-iterations 0))
	(do ((i 100000000 (incf i)))
		((= i 1000000000))
	  (when (is-bsn-valid i)
		(incf sum-valid)
		(if (is-bsn-valid (truncate i 10))
			(incf counter)))
	  (incf total-iterations))
	(format t "~d pairs (~,3f% of ~d)~%"
			counter
			(* 100 (/ counter sum-valid))
			sum-valid)))
