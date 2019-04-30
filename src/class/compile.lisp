; CLASS COMPILE

; CONSTRUCTOR

(defun create_compile(gfunctions lfunctions variables code)
	(list
		'(
			if
			+
			-
			*
			/
			=
			>
			<
			null
		)
		'(
			QUOTE
		)
		gfunctions
		lfunctions
		variables
		code
	))

; BEGIN LISP FUNCTIONS (LISP functions)

(defun get_all_lisp_functions_compile(c)
	(car c))
	
(defun is_list_function_compile(c name)
	(member name (get_all_lisp_functions_compile c)))

; END LISP FUNCTIONS

; BEGIN NONE EVALUATE (PARAMTER) MACRO

(defun get_none_evaluate_macro_compile(c)
	(cadr c))
	
(defun is_none_evaluate_macro_compile(c fname)
	(member fname (get_none_evaluate_macro_compile c)))

; END NONE EVALUATE (PARAMTER) MACRO
	
; BEGIN GFUNCTIONS (globals functions)

(defun get_all_gfunctions_compile(c)
	(caddr c))

(defun is_gfunction_compile(c name)
	(has_pair_table (get_all_gfunctions_compile c) name))

(defun get_gfunction_params_compile(c name)
	(get_pair_table (get_all_gfunctions_compile c) name))

; END GFUNCTIONS

; BEGIN LFUNCTIONS (locals functions)

(defun get_all_lfunctions_compile(c)
	(cadddr c))

; END LFUNCTIONS

; BEGIN VARIABLES

(defun get_all_variables_compile(c)
	(cadddr (cdr c)))

(defun is_variable_compile(c sym)
	(not (null (get_pair_table (get_all_variables_compile c) sym))))

(defun get_variable_pos_compile(c sym)
	(get_pair_table (get_all_variables_compile c) sym))

; END VARIABLES

; BEGIN CODE

(defun get_code_compile(c)
	(cadddr (cddr c)))

; END CODE
