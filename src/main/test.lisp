(defun test_compile_fibo()
	(compile_file "test/fibo.lisp" "asm/fibo.asm"))

(defun test_compile_fact()
	(compile_file "test/fact.lisp" "asm/fact.asm"))

(defun test_compile_compiler()
	(compile_file "bin/launch.lisp" "asm/launch.asm"))

(defun test_main(vm)
	(print "Compilation FIBO...")
	(test_compile_fibo)
	(print "Compilation FACT...")
	(test_compile_fact)
	(print "Chargement code FIBO...")
	(load_file_asm vm "asm/fibo.asm")
	(print "Execution FIBO...")
	(exec_vm vm)
	(print "Chargement code FACT...")
	(load_file_asm vm "asm/fact.asm")
	(print "Execution FACT...")
	(exec_vm vm)
)

(test_main (create_vm 1024))
