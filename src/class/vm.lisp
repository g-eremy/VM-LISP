(defun create_vm(memory_size)
	(labels (
		(init()
			(list
				(create_registers)
				(make-array (list memory_size))
				(make-hash-table)
			))
		(f(vm)
			(set_register_vm vm 'PC (- memory_size 1))
			(set_register_vm vm 'BP (- memory_size 1))
			(set_register_vm vm 'SP (- memory_size 1))
			vm
		))
	(f (init))))


; BEGIN REGISTER

(defun get_all_registers_vm(vm)
	(car vm))

(defun get_register_vm(vm rname)
	(get_register (get_all_registers_vm vm) rname))

(defun set_register_vm(vm rname value)
	(set_register (get_all_registers_vm vm) rname value))

; END REGISTER

; BEGIN MEMORY

(defun get_all_memory_vm(vm)
	(cadr vm))

(defun get_memory_vm(vm pos)
	(aref (cadr vm) pos))

(defun set_memory_vm(vm pos value)
	(setf (aref (cadr vm) pos) value))

(defun push_memory_vm(vm value)
	(set_memory_vm
		vm
		(get_register_vm vm 'SP)
		value
	))

; END MEMORY

; BEGIN FUNCTION

(defun get_all_functions_vm(vm)
	(caddr vm))

(defun get_function_pos_vm(vm fname)
	(gethash fname (get_all_functions_vm vm)))

(defun hash_function_vm(vm fname)
	(not (null (get_function_pos_vm vm fname))))
	
(defun set_function_pos_vm(vm fname pos)
	(setf (gethash fname (get_all_functions_vm vm)) pos))

; END FUNCTION
