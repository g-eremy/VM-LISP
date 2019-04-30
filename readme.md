----------------------- USER INFO ---------------------------

Dans ce projet, nous avons décidé de ne pas utiliser la macro loop, et donc d'utiliser la récursion terminale à la place.
Pour ce faire, nous utilisons un makefile qui concatène l'ensemble des fichiers LISP dans le dossier "src" dans le fichier "bin/launch.lisp".
Ensuite, on compile ce fichier à l'aide du compilateur natif de LISP (à noter que load.lisp va charger le fichier launch compilé, et modifier la variable *print-gensym*).

Le fichier "test.lisp" à la racine du projet, compile, charge et exécute fibo et fact.
Ensuite, cela compile, charge et exécute le compilateur, le chargeur er la VM.
Le compilateur, le chargeur et la VM vont être chargé et exécuté.
Le compilateur compilé va compilé fibo et fact. 
Le chargeur compilé va charger fibo et fact qui ont été compilé avec le compilateur compilé
Et enfin, la VM compilé (qui aura donc fibo et fact en mémoire) va être exécuté.
> (load "test.lisp")

Pour utiliser le compilateur ainsi que la VM, veuillez suivre les étapes suivantes :

- ouvrir un terminal dans à la racine du projet :
> make all
> clisp
> (load "load.lisp")

- Pour compiler un fichier :
> (compile_file "fichier_source.lisp" "fichier_destination.asm")

- Pour compiler directement un ensemble d'instructions/fonctions (donc une liste contenant des déclarations de fonctions, et instructions) :
> (compile_code '( (f1 1 2 3) (f2 2 3 4) (defun test(ls) (car ls)) ))

- Pour compiler directement une seule instruction, ou une seule déclaration de fonction :
> (compile_call_fun '(fibo 15)) (compile_call_fun '(defun test(ls) (car ls))) 

- Pour créer une VM, utiliser la fonction suivante (où size est la taille qui sera alloué pour la mémoire)
> (create_vm size)

- Vous pouvez stocker la VM dans une variable globale :
> (setf vm_size (create_vm size))

- Pour charger du code assembleur généré avec le compilateur (où vm_size est la variable contenant la VM)
> (load_file_asm vm_size "fichier_destination.asm")

- pour exécuter le code chargé dans la VM :
> (exec_vm vm_size)

----------------------- DEV INFO ----------------------------

Les différentes passes du compilateur :

- code LISP à compiler
: LISP 1

- transformer les backquotes en quotes
: LISP 2

- transformer les labels :
+ renommer le nom des fonctions locales par un symbole généré
+ renommer l'appel des fonctions locales par la symbole généré et ajouter les paramètres de la fonction "englobante"
+ redéfinir les fonctions locales en fonction globales
: LISP 3

- transformer les cond en if (le corp de chaque case du cond doit être dans un progn)
: LISP 4

- transformer les cadr, cddr, caar, et toute la famille en utilisant des car et des cdr
+ transformer funcall en apply
+ transformer and et or en une succession de if (évaluation paresseuse)
+ transformer >= : (or (> a b) (= a b)) (le or ici sera aussi transformé en une succession de if)
+ transformer <= : (or (< a b) (= a b)) (le or ici sera aussi transformé en une succession de if)
+ transformer les opéteurs arithmétiques + - / * pour qu'ils soient toujours binaires : (+ a b c d) > (+ a (+ b (+ c d)))
+ transformer setq en setf
+ transformer setf (permettra de savoir quelle fonction est utilisé sur le premier paramètre afin que n'importe quel type de setf fonctionne)
	> (setf (car ls) 5) : (setf ls 5 'car)
	> (setf (car (test ls) 10)) : (setf (test ls) 5 'car)
	> (setf (aref tab pos) 1) : (setf tab pos 1 'aref)
: LISP 5

- renommer le nom des variables (liste des arguments) par un symbole unique et les mettre dans la table des pairs référençant l'ensemble des variables (associé à sa position dans la pile)
: LISP 6

- Passer le main du programme au début du code
: LISP 7

- répertorier toute les fonctions utilisateurs
+ renommer l'appel des fonctions par (call_lisp_operator 'cons/car/cdr... liste_parametres) lorsque l'utilisateur appelle une fonction LISP que l'on gère
+ renommer l'appel de quote et autre macro qui n'évalue pas leurs paramètres que l'on gère à la compilation par : (call_nevalute_macro 'quote paramètre)
+ renommer l'appel des autres fonctions par (call_fun 'nom_fonction liste_parametres)
+ renommer les defun en defun_asm
: LISP 8 ("pipe" between LISP et ASM)

- transformer le code en assembleur LISP en évaluant le code précédemment généré
: Assembleur LISP

-------------------------------------------------------------

Les différentes étapes d'implémentation :

- from LISP 1 to LISP 2
- from LISP 2 to LISP 3
- from LISP 3 to LISP 4
- from LISP 4 to LISP 5
- from LISP 5 to LISP 6
- from LISP 6 to LISP 7
- from LISP 7 to LISP 8
- from LISP 8 to ASM (c'est en quelque sorte une eval du code de LISP 8)
- Création de la VM
- Chargement du code
- Exécution du code

-------------------------------------------------------------

Points d'améliorations :

- Implémenter let
- Implémenter les variables globales
- Implémenter les macro (defmacro)
- Implémenter les paramètres optionnels 
- Ajouter des optimisations à la compilation telles que :
	> calculer directement le résultat lorsque qu'il y a une opéation arithmétique entre 2 constantes
	> simplifier les calculs logiques (and et or) lorsque T et nil sont utilisé
	> lorsqu'une fonction n'utilise pas la récursion ou des fonctions qui appellent cette même fonction, remplacer tous les appels de la dites fonction par son code
