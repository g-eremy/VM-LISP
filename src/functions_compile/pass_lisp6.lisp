(defun lisp6_callback(code var_pair)
	(get_pair_table var_pair code))

(defun lisp6_get_vars(compile)
	(labels (
		(create_pair_var (r pos ls)
			(cond
				((null ls) r)
				(T
					(create_pair_var
						(cons (list (car ls) pos) r)
						(- pos 1)
						(cdr ls)
					)
				)
			))
		(f (r code)
			(cond
				((null code)
					r)
				((and (consp (car code)) (equal (caar code) 'defun))
					(f
						(append (create_pair_var '() (+ (list-length (caddar code)) 1) (caddar code)) r)
						(cdr code)
					))
				(T (f r (cdr code))))))
	(create_compile
		(get_all_gfunctions_compile compile)
		(get_all_lfunctions_compile compile)
		(f '() (get_code_compile compile))
		(get_code_compile compile)
)))

(defun lisp6_rename(compile)
	(labels (
		(parse_params(r ls)
			(cond
				((null ls)
					r)
				(T
					(parse_params
						(cons (list (car ls) (gensym "VAR")) r)
						(cdr ls)
				))
			))
		(create_nodes(r ls)
			(cond
				((null ls) (cons (list 'quote 2) r))
				(T (create_nodes
					(cons (list (car ls) 1) r) 
					(cdr ls) 
				))
			))
		(create_automate(ls)
			(list
				(list
					(create_nodes '() ls)
					0
					0
				)
				(list
					(create_nodes '() ls)
					0
					0
					'lisp6_callback
					(parse_params '() ls)
				)
				(list
					'()
					2
					1
				)
			))
		(parse_fun (c)
			(eval_automate (create_automate (caddr c)) c))
		(f (r c)
			(cond
				((null c)
					r)
				((and (consp (car c)) (equal (caar c) 'defun))
					(f 
						(append r (list (parse_fun (car c))))
						(cdr c) 
					))
				(T
					(f 
						(append r (list (car c)))
						(cdr c) 
					))
		)))
	(f '() (get_code_compile compile))))

(defun lisp6(compile)
	(lisp6_get_vars
		(create_compile
			(get_all_gfunctions_compile compile)
			(get_all_lfunctions_compile compile)
			(get_all_variables_compile compile)
			(lisp6_rename compile)
		)
))
