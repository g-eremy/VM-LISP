(defun get_all_jump_asm()
	'(
		JMP
		JSR
		JGT
		JGE
		JLT
		JLE
		JEQ
		JNE
		JTRUE
		JNIL
	))

; BEGIN REGISTERS MANAGEMENT

(defun set_asm(vm value reg_dest)
	(set_register_vm vm reg_dest value))

(defun move_asm(vm src_reg dest_reg)
	(set_register_vm
		vm
		dest_reg
		(get_register_vm vm src_reg)
	))
	
; END REGISTERS MANAGEMENT

; BEGIN STACK MANAGEMENT
	
(defun load_asm(vm reg_src reg_dest)
	(set_register_vm
		vm
		reg_dest
		(get_memory_vm vm (get_register_vm vm reg_src))
	))
	
(defun store_asm(vm reg_src reg_dest)
	(set_memory_vm
		vm
		(get_register_vm vm reg_dest)
		(get_register_vm vm reg_src)
	))
	
(defun push_asm(vm reg_src)
	(store_asm
		vm
		reg_src
		'SP
	)
	(set_register_vm
		vm
		'SP
		(- (get_register_vm vm 'SP) 1)
	)
	(get_register_vm vm reg_src)
)

(defun pop_asm(vm reg_dest)
	(set_register_vm
		vm
		'SP
		(+ (get_register_vm vm 'SP) 1)
	)
	(load_asm
		vm
		'SP
		reg_dest
	)
)

; END STACK MANAGEMENT

; BEGIN ARITH' OPERATORS

(defun add_asm(vm src_reg dest_reg)
	(set_register_vm
		vm
		dest_reg
		(+
			(get_register_vm vm dest_reg)
			(get_register_vm vm src_reg)
		)
	))

(defun sub_asm(vm src_reg dest_reg)
	(set_register_vm
		vm
		dest_reg
		(-
			(get_register_vm vm dest_reg)
			(get_register_vm vm src_reg)
		)
	))
	
(defun mul_asm(vm src_reg dest_reg)
	(set_register_vm
		vm
		dest_reg
		(*
			(get_register_vm vm dest_reg)
			(get_register_vm vm src_reg)
		)
	))
	
(defun div_asm(vm src_reg dest_reg)
	(set_register_vm
		vm
		dest_reg
		(/
			(get_register_vm vm dest_reg)
			(get_register_vm vm src_reg)
		)
	))

(defun incr_asm(vm dest_reg)
	(set_register_vm
		vm
		dest_reg
		(+
			(get_register_vm vm dest_reg)
			1
		)
	))
	
(defun decr_asm(vm dest_reg)
	(set_register_vm
		vm
		dest_reg
		(-
			(get_register_vm vm dest_reg)
			1
		)
	))

; END ARITH' OPERATORS

; BEGIN CMP

(defun cmp_asm(vm reg_src1 reg_src2)
	(labels (
		(f (v1 v2)
			(set_register_vm vm 'FL (< v1 v2))
			(set_register_vm vm 'FE (= v1 v2))
			(set_register_vm vm 'FG (> v1 v2))
		))
	(f
		(get_register_vm vm reg_src1)
		(get_register_vm vm reg_src2)
	)))

; END CMP

; BEGIN TEST

(defun test_asm(vm reg_src)
	(set_register_vm
		vm
		'FNIL
		(null (get_register_vm vm reg_src))
	)
)

; END TEST

; BEGIN JUMP

(defun jump_asm(vm label)
	(cond
		((numberp label)
			(set_register_vm
				vm
				'PC
				label
			))
		(T (meta_eval_lisp vm label))
))

(defun jsr_asm(vm pos)
	(push_asm vm 'PC)
	(jump_asm vm pos))
	
(defun jnil_asm(vm pos)
	(cond
		((null (get_register_vm vm 'FNIL)) (jump_asm vm pos))))
		
(defun jtrue_asm(vm pos)
	(cond
		((not (null (get_register_vm vm 'FNIL))) (jump_asm vm pos))))

(defun rtn_asm(vm)
	(pop_asm vm 'PC))

; END JUMP
