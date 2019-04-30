(defun test1(r ls)
	(cond
		((null ls)
			r
		)
		((= (car ls) 0)
			(cond
				((null (cdr ls))
					(print 'ok2)
					r
				)
				(T
					(print 'ok3)
					(test1 0 (cdr ls))
				)))
		(T
			(print 'ok4)
			(test1 (+ r (car ls)) (cdr ls))
)))

(print (test1 0 '(1 2 3 0 1 2 3 0)))
