(defun test_bq(b c)
	`(a b ,c d (a (a a) a a) ,(+ 2 3) ,(list `(a ,b ,(+ 1 1)))))

(defun test_cond(all_ls)
	(labels (
		(f (r ls)
			(cond
				((null ls) 
					r)
				((numberp (car ls))
					(cond
						((oddp (car ls)) (f (* r (car ls)) (cdr ls)))
						(T (f (+ r (car ls)) (cdr ls)))))
				(T (f (+ r 1) (cdr ls))))))
	(f 0 all_ls)))

(defun ec(code)
	code)

(defun test(code)
	`(car (car (car ,(ec code))))
)

(defun test2(first_param second_param r)
	`(setf ,first_param ,second_param (quote ,r)))

(print (test 'ok))

(print (test_cond '(1 2 3 4 5)))

(print (test_bq 5 2))
