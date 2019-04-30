(defun load_code_asm(vm code)
	(labels (
		(push_code(c)
			(set_asm vm (car c) 'R0)
			(push_asm vm 'R0)
			(cond
				((equal (caar c) 'label)
					(set_function_pos_vm vm (cadar c) (+ (get_register_vm vm 'SP) 1))))
			(cond
				((null (cdr c)) T)
				(T (push_code (cdr c)))))
		(parse_jmp(c ls pos end)
			(cond 
				((and (member (car c) ls) (hash_function_vm vm (cadr c)))
					(set_memory_vm vm pos (list (car c) (get_function_pos_vm vm (cadr c))))
			))
			(cond
				((> pos end)
					(parse_jmp (get_memory_vm vm (- pos 1)) ls (- pos 1) end)
			))
		))
	(move_asm
		vm
		'BP
		'PC
	)
	(push_code
		code
	)
	(move_asm
		vm
		'SP
		'BP
	)
	(move_asm
		vm
		'SP
		'FP
	)
	(parse_jmp
		(get_memory_vm vm (get_register_vm vm 'PC))
		(get_all_jump_asm)
		(get_register_vm vm 'PC)
		(+ (get_register_vm vm 'BP) 1)
	)
))

(defun load_file_asm(vm file)
	(labels (
		(f (r value stream)
			(cond
				((null value) r)
				(T (f (append r (list value)) (read stream nil nil) stream))
			))
		(reading (stream r)
			(f r (read stream nil nil) stream)
		))
	(load_code_asm vm (reading (open file) '()))))

(defun exec_vm(vm)
	(labels (
		(exec_code(code)
			(cond
				((or (equal (car code) 'HALT) (= (get_register_vm vm 'PC) 0))
					(get_register_vm vm 'R0))
				(T 
					(meta_eval_asm
						vm
						(car code)
						(cdr code)
					)
					(set_register_vm
						vm
						'PC
						(- (get_register_vm vm 'PC) 1)
					)
					(exec_code (get_memory_vm vm (get_register_vm vm 'PC)))
				)
		))
	)
	(exec_code (get_memory_vm vm (get_register_vm vm 'PC)))
))
