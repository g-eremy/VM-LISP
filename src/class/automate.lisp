(defun get_begin_automate(automate)
	(car automate))

(defun get_n_automate(automate n)
	(cond
		((null automate) nil)
		((= n 0) (car automate))
		(T (get_n_automate (cdr automate) (- n 1)))))

(defun eval_automate(automate code)
	(labels (
		(f (r node ls getting_next processing)
			(cond
				((null node)
					nil)
				((null ls)
					r)
				(getting_next
					(f r (get_next_autonode node automate (car ls)) ls '() T))
				((and processing (is_end_autonode node))
					(append r (processing_autonode node ls)))
				(processing
					(f (append r (list (processing_autonode node (car ls)))) node (cdr ls) nil nil))
				((and (consp ls) (consp (car ls)))
					(f (append r (list (f '() node (car ls) nil nil))) node (cdr ls) nil nil))
				((consp ls)
					(f r node ls T nil))
				(T 
					(f r node (list ls) T nil))
		)))
	(f '() (get_begin_automate automate) code nil nil)))

(defun eval_automate_rc(automate code)
	(labels (
		(is_function (funct)
			(equal (car funct) 'defun))
		(eval_defun(node ls)
			(append
				(list 'defun)
				(list (cadr ls))
				(list (caddr ls))
				(f '() node (cdddr ls))))
		(pf (next_node ls)
			(cond
				((is_end_autonode next_node) 
					(processing_autonode next_node ls))
				(T 
					(f (list (processing_autonode next_node (car ls))) next_node (cdr ls)))))
		(f (r node ls)
			(cond
				((null node)
					'())
				((null ls) 
					r)
				((and (consp (car ls)) (is_function (car ls)))
					(f 
						(append r (list (eval_defun node (car ls))))
						node
						(cdr ls) 
					))
				((and (consp (car ls)))
					(f 
						(append
							r 
							(list (pf (get_next_autonode node automate (caar ls)) (car ls)))
						)
						node
						(cdr ls)
					))
				(T
					(f
						(append r (list (car ls)))
						node
						(cdr ls)
				))
		)))
	(f '() (get_begin_automate automate) code)))

(defun get_char_autolink(link)
	(car link))

(defun get_state_autolink(link)
	(cadr link))

(defun get_links_autonode(node)
	(car node))

(defun get_other_autonode(node)
	(cadr node))

(defun is_end_autonode(node)
	(= (caddr node) 1))

(defun get_callback_autonode(node)
	(cadddr node))

(defun has_callback_autonode(node)
	(not (null (get_callback_autonode node))))

(defun get_params_autonode(value node)
	(cond
		((null (cddddr node)) (list value))
		(T (append (list value) (cddddr node)))))

(defun processing_autonode(node value)
	(cond
		((has_callback_autonode node)
			(apply (get_callback_autonode node) (get_params_autonode value node)))
		(T value)))

(defun get_next_autonode(node automate symb)
	(labels (
		(f (links)
			(cond
				((null links)
					(get_n_automate automate (get_other_autonode node)))
				((equal (get_char_autolink (car links)) symb)
					(get_n_automate automate (get_state_autolink (car links))))
				(T
					(f (cdr links))))))
	(f (get_links_autonode node))))
