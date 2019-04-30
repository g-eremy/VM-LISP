(defun automate_lisp2_backquote()
	'( 
		((
			(SYSTEM::BACKQUOTE 1)
			(quote 2)
		) 0 0)
		
		(() 2 1 lisp2_back_to_quotify)

		(() 2 1)
	))

(defun automate_lisp2_unquote()
	'(
		((
			(SYSTEM::UNQUOTE 1)
			(QUOTE 2)
		) 0 1 lisp2_quotify)

		(() 0 1 lisp2_unqotify)

		(() 0 1 lisp2_quotify_inception)
	))

(defun lisp2_eval_automate_backquote(code)
	(eval_automate (automate_lisp2_backquote) code))

(defun lisp2_eval_automate_unquote(code)
	(eval_automate_rc (automate_lisp2_unquote) code))

(defun lisp2_back_to_quotify (code)
	(lisp2_quotify (cadr code)))

(defun lisp2_quotify (code)
	(labels (
		(f(r ls)
			(cond
				((null ls)
					(cons 'list r))
				((not (consp ls))
					(f r (list ls)))
				((and (consp (car ls)))
					(f
						(append r (lisp2_eval_automate_unquote (list (car ls))))
						(cdr ls)
				))
				(T
					(f
						(append r (list `(quote ,(car ls))))
						(cdr ls)
				))
		)))
	(f '() code)
))

(defun lisp2_unqotify (code)
	(car (lisp2_eval_automate_backquote (cdr code))))

(defun lisp2_quotify_inception (code)
	(list 'list (quote 'quote) (car (lisp2_eval_automate_unquote (cdr code)))))

(defun lisp2(compile)
	(create_compile
		(get_all_gfunctions_compile compile)
		(get_all_lfunctions_compile compile)
		(get_all_variables_compile compile)
		(lisp2_eval_automate_backquote (get_code_compile compile))
	))
