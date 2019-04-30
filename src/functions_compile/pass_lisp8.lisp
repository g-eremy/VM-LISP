(defun lisp8_getfunctions(compiler)
	(labels (
		(is_function(code)
			(equal (caar code) 'defun))
		(get_fname(code)
			(cadar code))
		(get_params(code)
			(caddar code))
		(f (acc_ls code)
			(cond
				((null code) acc_ls)
				((is_function code)
					(f
						(cons (list (get_fname code) (get_params code)) acc_ls)
						(cdr code)
					))
				(T
					(f acc_ls (cdr code))
				)
			)
		)
	)
	(f (get_all_gfunctions_compile compiler) (get_code_compile compiler)))
)

(defun lisp8_rename(compiler)
	(labels (
		(is_function(c)
			(equal (car c) 'defun))
		(get_body(c)
			(cdddr c))
		(get_title(c)
			(list 'defun_asm (cadr c)))
		(make_fun(c)
			(append
				(get_title c)
				(list (f '() (get_body c)))
			))
		(parse_fun(name)
			(cond
				((is_none_evaluate_macro_compile compiler name) 'call_nevalute_macro)
				((is_list_function_compile compiler name) 'call_lisp_operator)
				((equal name 'call_main) name)
				(T 'call_fun)
			))
		(parse_params(name params)
			(cond
				((is_none_evaluate_macro_compile compiler name) params)
				(T (f '() params))
			))
		(parse (name params)
			(list
				(parse_fun name)
				name
				(parse_params name params)
			))
		(f (result c)
			(cond
				((null c) 
					result)
				((and (consp (car c)) (is_function (car c)))
					(f
						(append result (list (make_fun (car c))))
						(cdr c)
					))
				((and (consp (car c)))
					(f 
						(append result (list (parse (caar c) (cdar c))))
						(cdr c)
					))
				(T 
					(f 
						(append result (list (car c)))
						(cdr c) 
				))
			)
		))
	(f '() (get_code_compile compiler)))
)

(defun lisp8(compiler)
	(labels (
		(f (c)
			(create_compile
				(get_all_gfunctions_compile c)
				(get_all_lfunctions_compile c)
				(get_all_variables_compile c)
				(lisp8_rename c)
			)))
	(f	(create_compile
			(lisp8_getfunctions compiler)
			(get_all_lfunctions_compile compiler)
			(get_all_variables_compile compiler)
			(get_code_compile compiler)
	))
))
