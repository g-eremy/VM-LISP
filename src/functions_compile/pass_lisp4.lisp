(defun automate_lisp4()
	'( 
		(( 
			(quote 2)
			(cond 1)
		) 0 0)
		
		(() 0 1 lisp4_toif)

		(() 2 1)
	))

(defun lisp4_cond(code)
	(caar code))

(defun lisp4_eval(code)
	(cdar code))

(defun lisp4_toif(code)
	(labels (
		(continue_eval(ls)
			(append
				(list 'progn)
				(lisp4_eval_automate (lisp4_eval ls))
			))
		(f (ls)
			(cond
				((null ls) '())
				((equal (lisp4_cond ls) 'T) (continue_eval ls))
				(T (list 'if (lisp4_cond ls) (continue_eval ls) (f (cdr ls)))))))
	(f (cdr code))))

(defun lisp4_eval_automate(code)
	(eval_automate_rc (automate_lisp4) code))

(defun lisp4(compile)
	(create_compile
		(get_all_gfunctions_compile compile)
		(get_all_lfunctions_compile compile)
		(get_all_variables_compile compile)
		(lisp4_eval_automate (get_code_compile compile))
	))
