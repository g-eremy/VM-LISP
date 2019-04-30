(defun test1(pref suf)
	(apply 'list (list pref 1 2 3 suf)))

(defun test2()
	(apply 'test1 '((4 5 6) (7 8 9))))

(defun test3(ls)
	(apply 'equal ls))

(defun test4(a b c)
	(+ a b c))

(print (test2))

(print (test3 '(a b)))

(print (test3 '(a a)))

(print (funcall 'test4 1 2 3))
