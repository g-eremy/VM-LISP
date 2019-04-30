(defun meta_eval_lisp(vm fname)
	(labels
		(
			(load_param(pos)
				(set_asm 
					vm 
					(+ (get_register_vm vm 'SP) pos 1)
					'R0
				)
				(load_asm
					vm
					'R0
					'R0
				)
				(get_register_vm
					vm
					'R0
				)
			)
			(get_params(r n)
				(cond
					((= n 0) r)
					(T
						(get_params
							(append r (list (load_param n)))
							(- n 1)
						)
				))
			)
			(apply_push_params(params)
				(cond
					((not (null params))
						(set_asm vm (car params) 'R0)
						(push_asm vm 'R0)
						(apply_push_params (cdr params))
				))
			)
			(apply_pop_params(params)
				(cond
					((not (null params))
						(pop_asm vm 'R1)
						(apply_pop_params (cdr params))
				))
			)
			(apply_user_fun(fun params)
				(push_asm vm 'PC) ; save current PC
				; begin fun_call
				(push_asm vm 'FP)
				(apply_push_params params)
				(set_asm vm (list-length params) 'R1)
				(push_asm vm 'R1)
				(move_asm vm 'SP 'FP)
				; fake jsr
				(set_asm vm 1 'R1)
				(push_asm vm 'R1)
				(jump_asm vm (get_function_pos_vm vm fun))
				; run apply
				(exec_vm vm)
				; pop
				(pop_asm vm 'R1)
				(apply_pop_params params)
				(pop_asm vm 'FP)
				(pop_asm vm 'PC)
				(get_register_vm vm 'R0)
			)
			(apply_lisp_fun(fun params)
				(apply fun params))
			(setf_parse_params(r params)
				(cond
					((null params) r)
					(T (setf_parse_params (cons `(quote ,(car params)) r) (cdr params)))
				))
			(setf_parse(reversed_params)
				`(setf ,(cons (car reversed_params) (setf_parse_params '() (cddr reversed_params))) (quote ,(cadr reversed_params))))
			(lisp_fun(params)
				(cond
					((equal fname 'quote)
						(car params))
					((equal fname 'setf)
						(eval (setf_parse (reverse params))))
					((and (equal fname 'apply) (hash_function_vm vm (car params)))
						(apply_user_fun (car params) (cadr params)))
					((equal fname 'apply)
						(apply_lisp_fun (car params) (cadr params)))
					((equal fname 'eval)
						(eval (car params)))
					(T
						(apply fname params))
			))
		)
		(rtn_asm vm)
		(load_param 0)
		(set_asm
			vm
			(lisp_fun (get_params '() (get_register_vm vm 'R0)))
			'R0
		)
))

(defun meta_eval_asm(vm fasm params)
	(cond
		((equal fasm 'set) (set_asm vm (car params) (cadr params)))
		((equal fasm 'move) (move_asm vm (car params) (cadr params)))
		((equal fasm 'load) (load_asm vm (car params) (cadr params)))
		((equal fasm 'store) (store_asm vm (car params) (cadr params)))
		((equal fasm 'push) (push_asm vm (car params)))
		((equal fasm 'pop) (pop_asm vm (car params)))
		((equal fasm 'add) (add_asm vm (car params) (cadr params)))
		((equal fasm 'sub) (sub_asm vm (car params) (cadr params)))
		((equal fasm 'mul) (mul_asm vm (car params) (cadr params)))
		((equal fasm 'div) (div_asm vm (car params) (cadr params)))
		((equal fasm 'incr) (incr_asm vm (car params)))
		((equal fasm 'decr) (decr_asm vm (car params)))
		((equal fasm 'cmp) (cmp_asm vm (car params) (cadr params)))
		((equal fasm 'test) (test_asm vm (car params)))
		((equal fasm 'jmp) (jump_asm vm (car params)))
		((equal fasm 'jsr) (jsr_asm vm (car params)))
		((equal fasm 'jnil) (jnil_asm vm (car params)))
		((equal fasm 'jtrue) (jtrue_asm vm (car params)))
		((equal fasm 'rtn) (rtn_asm vm))
))
