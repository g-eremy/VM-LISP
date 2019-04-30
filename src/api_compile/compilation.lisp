(defun compile_file(src dst)
	(labels (
		(f(file ls)
			(cond
				((null ls) (close file))
				(T 
					(write (car ls) :stream file)
					(f file (cdr ls))))
		))
		(f
			(open
				dst
				:if-does-not-exist :create
				:direction :output
			)
			(compile_code (lisp1 src))
	))
)

(defun compile_code(code)
	(get_code_compile (pass_asm (lisp8 (lisp7 (lisp6 (lisp5 (lisp4 (lisp3 (lisp2 (create_compile '() '() '() code))))))))))
)

(defun compile_call_fun(code)
	(compile_code (list code)))
