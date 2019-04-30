(format t "~%~%--------------------------------------------------------------------------~%~%")

(format t "Compilation, chargement, et execution de FACT et FIBO :~%")
(load "load")

(format t "~%~%--------------------------------------------------------------------------~%~%")

(format t "Compilation du compilateur, du chargeur et de la VM :~%")
(print '(compile_file "bin/launch.lisp" "asm/launch.asm"))
(compile_file "bin/launch.lisp" "asm/launch.asm")

(format t "~%~%--------------------------------------------------------------------------~%~%")

(format t "Creation de la VM, et chargement du code compile du compilateur, du chargeur et de la VM :~%")
(print '(setf bigvm (create_vm 1048576)))
(setf bigvm (create_vm 1048576))

(print '(load_file_asm bigvm "asm/launch.asm"))
(load_file_asm bigvm "asm/launch.asm")

(format t "~%~%--------------------------------------------------------------------------~%~%")
(format t "Execution du code charge de la compilation, chargement et execution de FACT et FIBO : ~%")

(print '(exec_vm bigvm))
(exec_vm bigvm)

(format t "~%~%--------------------------------------------------------------------------~%~%")
