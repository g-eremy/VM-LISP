(defun fact(n)
	(cond
		((= n 0) 1)
		(T (* n (fact (- n 1))))))

(defun fact_terminal(acc n)
	(cond
		((= n 1) acc)
		(T (fact_terminal (* acc n) (- n 1)))))

(print (fact 10))

(print (fact_terminal 1 10))
