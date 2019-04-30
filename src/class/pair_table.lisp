(defun create_pair_table(init_names init_value)
	(labels (
		(f(r ls)
			(cond 
				((null ls) r)
				(T (f (cons (list (car ls) init_value) r) (cdr ls)))
		)))
	(f '() init_names)))

(defun get_first_key_pair_table(instance)
	(caar instance))
	
(defun get_first_value_pair_table(instance)
	(cadar instance))
	
(defun get_pair_table(instance key)
	(cond
		((null instance) nil)
		((equal (caar instance) key) (cadar instance))
		(T (get_pair_table (cdr instance) key))))
		
(defun has_pair_table(instance key)
	(cond
		((null instance) nil)
		((equal (caar instance) key) T)
		(T (has_pair_table (cdr instance) key))))

(defun set_pair_table(instance key v)
	(cond
		((null instance) nil)
		((equal (caar instance) key) (setf (cadar instance) v))
		(T (set_pair_table (cdr instance) key v))))

(defun add_pair_table(instance key v)
	(labels (
		(f (r ls)
			(cond 
				((null ls) (cons (list key v) r))
				(T (f (cons (list (caar ls) (cadar ls)) r) (cdr ls)))
		)))
	(cond
		((null (get_pair_table instance key)) (f '() instance))
		(T (set_pair_table instance key v)))))
