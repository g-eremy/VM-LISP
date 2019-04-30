(defun lisp7_mainify (code)
	(labels (
		(is_function (ls)
			(equal (car ls) 'defun)
		)
		(f (main_ls defun_ls c)
			(cond
				((null c) 
					(append (list (cons 'call_main main_ls)) defun_ls)
				)
				((is_function (car c))
					(f main_ls (append defun_ls (list (car c))) (cdr c))
				)
				(T

					(f (append main_ls (list (car c))) defun_ls (cdr c))
				)
			)
		)
	)
	(f '() '() code))
)

(defun lisp7(compile)
	(create_compile
		(get_all_gfunctions_compile compile)
		(get_all_lfunctions_compile compile)
		(get_all_variables_compile compile)
		(lisp7_mainify (get_code_compile compile))
	))
