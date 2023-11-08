(defun is-bsn-valid (bsn)
  "Test if bsn is valid (with elfproef)"
  (let* ((multiplies9 '(-1 2 3 4 5 6 7 8 9))
		 (multiplies8 '(-1 2 3 4 5 6 7 8))
		 (use-list multiplies9)
		 (sum 0))
	(when (and (>= bsn 10000000) (<= bsn 999999999))
	  (if (< bsn 100000000)
		  (setf use-list multiplies8))
	  (dolist (i use-list sum)
		(setf sum (+ sum (* i (mod bsn 10))))
		(setf bsn (truncate bsn 10))))
	(if (zerop sum)
		nil
		(eql 0 (mod sum 11)))))

(defun find-valid-pairs ()
  "Find a valid bsn pair. I.e. a valid nine number and eight number
where the last digit is dropped."
  (let ((counter 0))
	(do ((i 100000000 (incf i)))
		((= i 1000000000))
	  (if (is-bsn-valid i)
		  (if (is-bsn-valid (truncate i 10))
			  (incf counter))))
	(format nil "~d pairs (~1f%)" counter (* 100 (/ counter 899999999)))))
