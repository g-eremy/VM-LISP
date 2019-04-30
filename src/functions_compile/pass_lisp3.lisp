(defun lisp3_rc_f (code fun_list params)
	(append
		(list (get_pair_table fun_list (car code)))
		params
		(lisp3_rename_call (cdr code) fun_list params)
	))


(defun lisp3_rc_automate_callback(function_name fun_list params)
	(lisp3_rc_f function_name fun_list params))

(defun lisp3_rename_call(code fun_list params)
	(labels (
		(generate_node(r ls)
			(cond
				((null ls) 
					(append r (list (list 'QUOTE 2))))
				(T
				 	(generate_node 
						(append r (list (list (caar ls) 1)))
						(cdr ls) 
					))))
		(generate_automate (nodes)
			(list 
				(list nodes 0 0) 
				(list nodes 0 1 'lisp3_rc_automate_callback fun_list params)
				(list '() 2 1))
		))
	(eval_automate (generate_automate (generate_node '() fun_list)) code)))

(defun lisp3_labels (code)
	(labels (
		(is_function (c)
			(equal (car c) 'defun))
		(get_body (c)
			(cdddr c))
		(has_label (c)
			(and (consp c) (equal (car c) 'labels)))
		(contain_label (c)
			(cond 
				((null c)
					nil)
				((has_label (car c))
					T)
				(T 
					(contain_label (cdr c)))))
		(get_label (c)
			(cond 
				((null c) nil)
				((has_label (car c))
					(cadar c))
				(T
					(get_label (cdr c)))))
		(create_pair (fun_list c)
			(cond
				((null c)
					fun_list)
				(T
					(create_pair
						(append fun_list (list (list (caar c) (gensym "LOCAL"))))
						(cdr c)
					))))
		(create_functions (params r fun_list c)
			(cond 
				((null c)
					r)
				(T
					(create_functions
						params
						(append r 
							(list (append
								(list
									'defun 
									(get_pair_table fun_list (caar c)) 
									(append params (cadar c))
								)
								(lisp3_rename_call (cddar c) fun_list params)
						)))
						fun_list
						(cdr c)
					)
				)))
		(label_to_function (params defun_ls labels_ls fun_list c)
			(cond
				((null c)
					(list defun_ls labels_ls))
				((has_label (car c))
					(label_to_function
						params
						(append
							defun_ls
							(lisp3_rename_call (cddar c) fun_list params)
							(cdr c)
						)
						(append
							labels_ls
							(create_functions params '() fun_list (cadar c))
						)
						fun_list
						'()
					))
				(T
					(label_to_function 
						params 
						(append defun_ls (list (car c)))
						labels_ls
						fun_list
						(cdr c)
					))))
		(distinct_label_functions (c)
			(cond 
				((not (contain_label (get_body c)))
					(list c))
				(T 
					(label_to_function 
					 	(caddr c)
					 	(list 'defun (cadr c) (caddr c))
					 	'()
					 	(create_pair '() (get_label (get_body c)))
					 	(get_body c)
					))))
		(f (result result_labels temp restart c)
			(cond 
				((not (null restart))
					(f
						'()
						'()
						nil
						nil
						c
					))
				((and (null c) (null result_labels))
					(append result result_labels))
				((null c)
					(f
						result
						result_labels
						nil
						T
						(append result result_labels)
					))
				((not (null temp))
					(f
						(append result (list (car temp)))
						(append result_labels (cadr temp))
						nil
						nil
						(cdr c)
					))
				((is_function (car c))
					(f
						result
						result_labels
						(distinct_label_functions (car c))
						nil
						c
					))
				(T 
					(f
						(append result (list (car c)))
						result_labels
						nil
						nil
						(cdr c)
					)))))
	(f '() '() nil nil code)))

(defun lisp3(compiler)
	(create_compile
		(get_all_gfunctions_compile compiler)
		(get_all_lfunctions_compile compiler)
		(get_all_variables_compile compiler)
		(lisp3_labels (get_code_compile compiler))
))

