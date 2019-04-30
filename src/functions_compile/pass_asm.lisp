(defun choose_reg(regs_forbidden)
	(labels (
		(f (ls)
			(cond
				((not (member (car ls) regs_forbidden)) (car ls))
				(T (f (cdr ls))))))
	(f (get_all_rn_registers))))

(defun calc_pos_in_stack(variable reg_pos compiler)
	(list
		(list 'set (get_variable_pos_compile compiler variable) reg_pos)
		(list 'add 'FP reg_pos)
	))

(defun load_value_in_stack(variable reg_dest compile)
	(append
		(calc_pos_in_stack
			variable
			reg_dest
			compile
		)
		(list (list 'load reg_dest reg_dest))
	))

(defun store_value_in_stack(variable reg_value compile)
	(labels (
		(f (reg_pos)
			(append
				(calc_pos_in_stack
					variable
					reg_pos
					compile
				)
				(list (list 'store reg_value reg_pos))
			)))
	(f 
		(choose_reg (list reg_value))
	)))

(defun push_callstack (fname callstack)
	(cond 
		((null callstack) callstack)
		(T
			(cons fname callstack))))

(defun pass_asm_eval(code params)
	(labels (
		(add_params (r ls)
			(cond
				((null ls) r)
				(T (add_params (append r (list (car ls))) (cdr ls))))))
	(cond
		((consp code) (apply (car code) (append (cdr code) (add_params '() params))))
		(T code))))

(defun eval_parameter(fname parameter compile callstack)
	(cond
		((consp parameter)
			(pass_asm_eval parameter (list compile (push_callstack fname callstack)))
		)
		((is_variable_compile compile parameter)
			(load_value_in_stack parameter 'R0 compile)
		)
		(T 
			(list (list 'set parameter 'R0))
		)
))

(defun push_params(fname params compile callstack)
	(labels (
		(f (r ls)
			(cond
				((null ls)
					r)
				(T
					(f
						(append
							r
							(eval_parameter
								fname
								(car ls)
								compile
								callstack
							)
							(list (list 'push 'R0))
						)
						(cdr ls)
					)
				))))
	(f '() params)))

(defun pop_params(params)
	(labels (
		(f (r ls)
			(cond
				((null ls)
					r)
				(T
					(f
						(append
							r
							(list (list 'pop 'R1))
						)
						(cdr ls)
					)))))
	(f '() params)))

(defun code_to_asm(code compile callstack)
	(labels (
		(f (r c)
			(cond
				((null c) r)
				((consp (car c))
					(f
						(append
							r
							(pass_asm_eval (car c) (list compile callstack))
						)
						(cdr c)
					))
				((is_variable_compile compile (car c))
					(f
						(append
							r
							(load_value_in_stack (car c) 'R0 compile)
						)
						(cdr c)
					))
				(T
					(f
						(append
							r
							(list (list 'set (car c) 'R0))
						)
						(cdr c)
					))
			)
		))
	(f '() code)))

(defun uni_func (fname parameter op compiler callstack)
	(append
		(eval_parameter
			fname
			parameter
			compiler
			callstack
		)
		op
	))

(defun bin_func (fname params compiler callstack)
	(append
		(push_params
			fname
			(cons (car params) (cons (cadr params) nil))
			compiler
			callstack
		)
		(list (list 'pop 'R1))
		(list (list 'pop 'R0))
		(list (list fname 'R1 'R0))
	)
)

(defun make_if(ifcond ifthen ifelse compile callstack)
	(labels (
		(f (symelse symcdr)
			(append
				(code_to_asm (list ifcond) compile callstack)
				(list (list 'test 'R0))
				(list (list 'JTRUE symelse))
				(code_to_asm (list ifthen) compile callstack)
				(list (list 'JMP symcdr))
				(list (list 'label symelse))
				(code_to_asm (list ifelse) compile callstack)
				(list (list 'label symcdr))
			)))
	(f (gensym "ELSE") (gensym "IFCDR"))))

(defun call_lisp_operator (fname params compiler callstack)
	(cond
		((equal fname 'if)
			(make_if (car params) (cadr params) (caddr params) compiler callstack))
		((equal fname 'null)
			(uni_func
				'null
				(car params)
				(list
					(list 'test 'R0)
					(list 'move 'FNIL 'R0)
				)
				compiler
				callstack
				)
		)
		((equal fname '=)
			(append
				(bin_func 'cmp params compiler callstack)
				(list (list 'move 'FE 'R0))
			)
		)
		((equal fname '>)
			(append
				(bin_func 'cmp params compiler callstack)
				(list (list 'move 'FL 'R0))
			)
		)
		((equal fname '<)
			(append
				(bin_func 'cmp params compiler callstack)
				(list (list 'move 'FG 'R0))
			)
		)
		((equal fname '+)
			(bin_func 'add params compiler callstack)
		)
		((equal fname '-)
			(bin_func 'sub params compiler callstack)
		)
		((equal fname '*)
			(bin_func 'mul params compiler callstack)
		)
		((equal fname '/)
			(bin_func 'div params compiler callstack)
		)
	)
)

(defun call_nevalute_macro(fname params compile callstack)
	(list (list 'set (car params) 'R0))
)

(defun call_fun(fname params compile callstack)
	(labels 
		(
			(edit_params (r call_params fun_params)
				(cond
					((null call_params)
						r
					)
					(T
						(edit_params
							(append
								r
								(eval_parameter
									fname
									(car call_params)
									compile
									callstack
								)
								(store_value_in_stack
									(car fun_params)
									'R0
									compile
								)
							)
							(cdr call_params)
							(cdr fun_params)
						))))
			(check_callstack (ls)
				(cond
					((null ls)
						T
					)
					((equal fname (car ls))
						(check_callstack (cdr ls))
					)
					(T
						nil
					)))
			(is_terminal()
				(and
					(not (null callstack))
					(check_callstack callstack)))
		)
		(cond
			((equal fname 'progn)
				(code_to_asm params compile callstack))
			((and (equal fname 'setf) (null (caddr params)))
				(append
					(eval_parameter fname (cadr params) compile callstack)
					(store_value_in_stack (car params) 'R0 compile)
				))
			((is_terminal)
				(append
					(edit_params
						'()
						params
						(get_gfunction_params_compile compile fname)
					)
					(list (list 'jmp fname))
				)
			)
			(T
				(append
					(list (list 'push 'FP))
					(push_params fname params compile callstack)
					(list (list 'set (list-length params) 'R1))
					(list (list 'push 'R1))
					(list (list 'move 'SP 'FP))
					(list (list 'jsr fname))
					(list (list 'pop 'R1))
					(pop_params params)
					(list (list 'pop 'FP))
				)
))))

(defun defun_asm(fname body compile callstack)
	(append
		(list (list 'label fname))
		(code_to_asm body compile (cons fname nil))
		(list (list 'rtn))
	))

(defun call_main(fname body compile callstack)
	(append
		(code_to_asm body compile callstack)
		(list (list 'halt))
))

(defun pass_asm(compile)
	(labels (
		(f (r code)
			(cond
				((null code) r)
				(T 
					(f 
						(append r (pass_asm_eval (car code) (list compile nil)))
						(cdr code) 
					))
			)
		))
	(create_compile
		(get_all_gfunctions_compile compile)
		(get_all_lfunctions_compile compile)
		(get_all_variables_compile compile)
		(f '() (get_code_compile compile))
)))
