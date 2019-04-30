(defun get_all_rn_registers()
	'(R2 R1 R0))

(defun get_all_registers()
	(append 
		'(FL FE FG FNIL PC BP SP FP)
		(get_all_rn_registers)
	))

(defun create_registers()
	(create_pair_table (get_all_registers) 0))

(defun get_register(reg rname)
	(get_pair_table reg rname))

(defun set_register(reg rname value)
	(set_pair_table reg rname value))

(defun is_register(rname)
	(not (null (member (get_all_registers) rname))))
