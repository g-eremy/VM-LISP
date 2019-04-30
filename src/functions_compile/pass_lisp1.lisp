(defun lisp1_get_code(file)
	(labels (
		(f (r value stream)
			(cond
				((null value) r)
				(T (f (append r (list value)) (read stream nil nil) stream))
			))
		(reading (stream r)
			(f r (read stream nil nil) stream)
		))
	(reading (open file) '())
))

(defun lisp1(file)
	(lisp1_get_code file))
