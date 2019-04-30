(defun get_arr(o)
	(car o))

(defun modif_first(ls)
	(setf (car ls) 1))
	
(defun test(ls)
	(print ls)
	(modif_first ls)
	(print ls)
)

(defun test2(a)
	(print a)
	(setf a 5)
	(print a)
)

(defun test3(o pos ls)
	(print (get_arr o))
	(setf (aref (get_arr o) pos) (car ls))
	(print (get_arr o))
)

(test '(5 2 3 4))

(test2 10)

(test3 (list (make-array (list 10))) 5 '(a b c d))
