(print (and (listp 'a) (car 'a)))

(print (and (= 2 2) (= 1 2)))

(print (and (= 2 2) (= 1 1)))

(print (and (= 2 2) (= 1 1) (= 3 2)))

(print (and (= 2 2) (= 1 1) (= 3 3)))

(print (or (= 2 1) (= 1 1)))

(print (or (= 2 1) (= 1 2)))

(print (<= 5 3))

(print (<= 5 1))

(print (<= 5 5))

(cond 
	((and (= 2 2) (= 1 2)) (print "pasok"))
	((or (= 2 1) (= 1 2)) (print "pasok2"))
	((and (= 2 2) (= 1 1) (= 3 3)) (print "ok") (print "double_ok"))
	(T (print "pasok3")))

