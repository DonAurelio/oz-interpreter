#lang eopl


;;Integrantes:
;;Luz Derlyn Garzon Martinez         1227683
;;Julian Garcia Rico                 1225435
;;Aurelio Antonio Vivas Meza         1110348
;;Diana Carolina Hernandez           1031083

;__________________________________________________________ ESPECIFICACION  LEXICA ________________________________________________
(define especif-lexica-subOz
  '((blanco-esp
     (whitespace) skip)
    (comentario
     ("%" (arbno (not #\newline))) skip)
    ;Numeros
    (entero
     (digit (arbno digit)) string)
    (flotante
     (digit (arbno digit) "." digit (arbno digit)) string)
    (entero
     ("~" digit (arbno digit)) string)
    (flotante 
     ("~" digit (arbno digit) "." digit (arbno digit) ) string)
    ;Variables
    (variable-anonima ("_") symbol)
    (variable ("A" (arbno (or letter digit))) symbol)
    (variable ("B" (arbno (or letter digit))) symbol)
    (variable ("C" (arbno (or letter digit))) symbol)
    (variable ("D" (arbno (or letter digit))) symbol)
    (variable ("E" (arbno (or letter digit))) symbol)
    (variable ("F" (arbno (or letter digit))) symbol)
    (variable ("G" (arbno (or letter digit))) symbol)
    (variable ("H" (arbno (or letter digit))) symbol)
    (variable ("I" (arbno (or letter digit))) symbol)
    (variable ("J" (arbno (or letter digit))) symbol)
    (variable ("K" (arbno (or letter digit))) symbol)
    (variable ("L" (arbno (or letter digit))) symbol)
    (variable ("M" (arbno (or letter digit))) symbol)
    (variable ("N" (arbno (or letter digit))) symbol)
    (variable ("O" (arbno (or letter digit))) symbol)
    (variable ("P" (arbno (or letter digit))) symbol)
    (variable ("Q" (arbno (or letter digit))) symbol)
    (variable ("R" (arbno (or letter digit))) symbol)
    (variable ("S" (arbno (or letter digit))) symbol)
    (variable ("T" (arbno (or letter digit))) symbol)
    (variable ("U" (arbno (or letter digit))) symbol)
    (variable ("V" (arbno (or letter digit))) symbol)
    (variable ("W" (arbno (or letter digit))) symbol)
    (variable ("X" (arbno (or letter digit))) symbol)
    (variable ("Y" (arbno (or letter digit))) symbol)
    (variable ("Z" (arbno (or letter digit))) symbol)
    
    ;Etiquetas 
    (etiqueta ( (concat "'" (arbno (or letter digit)) "'") ) symbol)
    (etiqueta ( (concat "a" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "b" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "c" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "d" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "e" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "f" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "g" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "h" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "i" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "j" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "k" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "l" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "m" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "n" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "o" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "p" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "q" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "r" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "s" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "t" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "u" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "v" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "w" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "x" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "y" (arbno (or letter digit))) ) symbol)
    (etiqueta ( (concat "z" (arbno (or letter digit))) ) symbol)
    
    ))

;__________________________________________________________ GRAMATICA  ________________________________________________

(define gramatica-subOz 
  '(  
    ;Programa
    (programa ( expresion ) a-programa)
    ;Cuerpo
    (cuerpo ( expresion (arbno expresion) ) cuerpoc)
    ;Expresion
    (expresion (variable-anonima) var-anon-exp)
    (expresion (variable) var-exp)
    ;--Creacion variables locales
    (expresion ("local"  variable (arbno variable) "in" cuerpo "end") local-exp)
    ;--Asignacion
    (expresion ("set" expresion "=" expresion) set-exp)
    ;--Operacion con variables
    (primitiva ("isdet?") isdet-prim)
    (primitiva ("isfree?") isfree-prim)
    ;Numeros
    (expresion (entero) entero-exp)
    (expresion (flotante) flotante-exp)
    ;Registros
    ;NOTA: modificacion para la expresion registro, debido a que dos producciones no deben empezar por el mismo no terminal o terminal  en gramaticas LL1.
    ;Debido a eso un record-exp empieza por !. y un atomo por una etiqueta
    (expresion (etiqueta) atom-exp)
    (expresion ("!" etiqueta  "(" etiqueta ":" expresion (arbno etiqueta ":" expresion) ")" ) record-exp)
    ;--listas 
    (expresion ("'|'" etiqueta ":" expresion etiqueta ":" expresion) list-exp1)
    (expresion ("[" expresion (arbno expresion) "]") list-exp2)
    ;NOTA se agregra el no terminal asg-record-type que permite la busquedas del tipo .(.F1.2).1 y tambien .F1.2
    (expresion ("." asg-record-type "." expresion) asg-record-exp)
    (asg-record-type ("(" "." asg-record-type "." expresion")" ) asg-record-anidado)
    (asg-record-type (expresion) asg-record-simple)
    
    ;Funciones y procedimientos
    (expresion ("proc" "{" nombre-proc (arbno variable) "}" cuerpo "end") proc-exp)
    (expresion ("fun" "{" nombre-proc (arbno variable) "}" cuerpo "end") fun-exp)
    (expresion ("{" expresion (arbno expresion) "}") app-exp)
    ;--Nombre para procedimientos y funciones
    (nombre-proc ( "$" ) nombre-proc-anon)
    (nombre-proc ( variable ) nombre-proc-var)
    
    ;Ejecucion condicional
    (expresion ("if" expresion "then" expresion (arbno "elseif" expresion "then" expresion) "else" expresion)
               if-exp)
    ;NOTA: Se agrego a la expresion case el no terminal end-case ya que hay varias expresiones que levan cuerpo presentan conflicto.
    (expresion ( "case" expresion "of" patron "then" cuerpo (arbno "[]" patron "then" cuerpo) (arbno "else" cuerpo) "end-case" ) case-exp)
    
    ;--Patron
    (patron (etiqueta end-record-pat) record-pat)
    (end-record-pat ( "(" (arbno etiqueta ":" patron) ")") end-record-pat1)
    (endl-record-pat() end-record-pat2)
    (patron (variable) var-pat)
    (patron (entero) int-pac)
    (patron (flotante) float-pac)
    
    ;Especiales
    (expresion ("skip") skip-exp)
    ;Primitivas
    (expresion ( primitiva "{" (arbno expresion) "}")  prim-exp)
    
    ;--Operaciones aritmeticas
    (primitiva ("+") sum-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("-") sub-prim)
    (primitiva ("/") div-prim)
    ;--Comparacion
    (primitiva ("<") menor-prim)
    (primitiva ("=<") meneq-prim)
    (primitiva (">") mayor-prim)
    (primitiva (">=") mayig-prim)
    (primitiva ("==") igual-prim)
    ;--Logicas
    (primitiva ("orelse") orelse-prim)
    (primitiva ("andthen") andthen-prim)
    ;--Unificiacion
    (primitiva ("=") unif-prim)
    ;--Operaciones sobre puertos
    (primitiva ("newport") newport-prim)
    (primitiva ("isport?") isport-prim)
    (primitiva ("send") send-prim)
    ;--Operaciones de celdas
    (primitiva ("newcell") newcell-prim)
    (primitiva ("iscell?") iscell-prim)
    (primitiva ("@") acces-prim)
    (primitiva ("setcell") setcell-prim)
    ;Ciclos
    (expresion ("for" expresion ".." expresion "do" cuerpo "end" ) for-exp)
    
    )
  )

;Permite crear automaticamente los tipos de datos para las expresiones del lenguaje
(sllgen:make-define-datatypes especif-lexica-subOz gramatica-subOz)



;______________________________________________ INTERPRETADOR _____________________________________
(define scan&parse
  (sllgen:make-string-parser especif-lexica-subOz gramatica-subOz))

(define interpretador
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (eval-program pgm))
                        (sllgen:make-stream-parser
                         especif-lexica-subOz
                         gramatica-subOz )))

;_____________________________________________REGISTROS_______________________________________________
(define-datatype regVal regVal?
  (atomo (id symbol?))
  (registro (nombre symbol?)
            (campos (list-of symbol?))
            (referencias (list-of number?))))

;; Permite crear un registro, cuyos campos estan ligados a una variable en el almacen
;; nom: nombre del registro
;; campos: campos del registro
;; refs: numeros que indican las posiciones donde estan ligados los campos del registro en el almacen
(define nuevo-registro
  (lambda (nom campos refs)
    (registro nom campos refs)))

;; Permite obtener la referencia o posocion a la variable del store a la que apunta un campo
;; reg: registro en el cual se buscara el campo al cual se le buscara el valor en el almacen
;; campos: hace referencia al campo del dual se desea encontrar el valor
(define apply-registro
  (lambda (reg campo)
    (cases regVal reg
      (registro (nom campos refs) (let 
                                      ((pos (list-find-position campo campos)))
                                    (if (number? pos)
                                        (list-ref refs pos)
                                        (eopl:error 'apply-registro "El campo ~s  no se encuentra en el registro" campo))))
      (else (eopl:error 'apply-registro "La estructura ~s  no es u  registro" reg)))))


;; Funciones observadoras para tipo de dato registro
(define registro-nombre
  (lambda (reg)
    (cases regVal reg
      (registro (nom campos refs) nom)
      (else (eopl:error 'registro-nombre "el valor dado ~s no es un registro" reg)))))

(define registro-campos
  (lambda (reg)
    (cases regVal reg
      (registro (nom campos refs) campos)
      (else (eopl:error 'registro-campos "el valor dado ~s no es un registro" reg)))))

(define registro-referencias
  (lambda (reg)
    (cases regVal reg
      (registro (nom campos refs) refs)
      (else (eopl:error 'registro-referencias "el valor dado ~s no es un registro" reg)))))




;; Funciones que permiten comparar registros
(define equal-registros?
  (lambda (reg1 reg2)
    (let 
        ((nom1 (registro-nombre reg1))
         (nom2 (registro-nombre reg2)))
      (if (equal-nombre-registros? nom1 nom2)
          (let
              ((campos1 (registro-campos reg1))
               (campos2 (registro-campos reg2)))
            (if (equal-campos-registro? campos1 campos2)
                (let
                    ((refs1 (registro-referencias reg1))
                     (refs2 (registro-referencias reg2)))
                  (if (equal-referencias-registro? refs1 refs2)
                      #t
                      #f))
                #f))
          #f))))

(define equal-nombre-registros?
  (lambda (nom-reg1 nom-reg2)
    (equal? nom-reg1 nom-reg2)))

(define equal-campos-registro?
  (lambda (campos-reg1 refs-reg1 campos-reg2 refs-reg2)
    (equal? (length campos-reg1) (length campos-reg2))))




(define equal-referencias-registro?
  (lambda (refs-reg1 refs-reg2)
    (cond
      [(null? refs-reg1) #t]
      [(equal? #f (equal-referencias-registro-aux? (car refs-reg1) refs-reg2)) #f]
      [else (equal-referencias-registro? (car refs-reg1) refs-reg2)])))

(define equal-referencias-registro-aux?
  (lambda (ref1 refs-reg2)
    (cond
      [(null? refs-reg2) #t]
      [(not (equal? (apply-almacen ref1) (apply-almacen (car ref1)))) #f]
      [else (equal-referencias-registro-aux? ref1 (cdr refs-reg2))])))


;______________________________________ PROCEDIMIENTOS Y FUNCIONES _____________________________________


;; Definicion del tipo de dato ProcVal
(define-datatype procVal procVal?
  (proc-closure (nomb-proc nombre-proc?)
                (ids (list-of symbol?))
                (cuerpo-proc  cuerpo?)
                (env environment?))
  
  (func-closure (nomb-func nombre-proc?)
                (ids (list-of symbol?))
                (cuerpo-func  cuerpo?)
                (env environment?)))

;; Funciones observadoras
(define procVal-anonimo?
  (lambda (proc)
    (cases procVal proc
      (proc-closure (nom ids cuerpo env) (cases nombre-proc nom
                                           (nombre-proc-anon () #t)
                                           (nombre-proc-var (var) #f)))
      (func-closure (nom ids cuerpo env) (cases nombre-proc nom
                                           (nombre-proc-anon () #t)
                                           (nombre-proc-var (var) #f))))))

(define procVal-nombre
  (lambda (proc)
    (cases procVal proc
      (proc-closure (nom ids cuerpo env) (cases nombre-proc nom
                                           (nombre-proc-anon () "")
                                           (nombre-proc-var (var) var )))
      (func-closure (nom ids cuerpo env) (cases nombre-proc nom
                                           (nombre-proc-anon () "")
                                           (nombre-proc-var (var) var))))))


;;Funcio que permite crear un procedimiento 
;;nom-proc: nombre del procedimiento
;;ids: idetificadores parametros del procedimiento
;;cuerpo: cuerpo del procedimiento
(define make-procedure
  (lambda (nom-proc ids cuerpo env)
    (func-closure nom-proc ids cuerpo env)))

;;Funcion que permite crear una funcion
;;nom-fun: nombre de la funcion
;;ids: identificadores de los parametros de la funcion
;;cuerpo: cuerpo de la funcion 
(define make-function
  (lambda (nom-fun ids cuerpo env)
    (func-closure nom-fun ids cuerpo env)))

;______________________________________________ VARIABLE _______________________________________________
;; Definicion del tipo de dato Variable
;; referencia: es el identificador de la posición de la variable en el almacén para almacenar su contenido.***
;; valor: almacena una referencia o un valor del lenguaje como procedimiento, registro, celda, entre otros.***
(define-datatype varVal varVal?
  (variable (espacio vector?)))


;; Funcion que permite crear una nueva variable que contiene un valor de una sola posicion
;; valor : es el valor con el cual se va a asignar la variable
(define nueva-variable
  (lambda (valor)
    (variable (make-vector 1 valor))))

;; Funcion que permite setear una variable 
;; valor : es el valor que se va a asignar a la variable
;; var : variable que se va a setear
(define set-variable
  (lambda (valor var)
    (cases varVal var
      (variable (espacio) 
                (nueva-variable valor)))))

;; Funcion que permite obtener el contenido de una variable
;; var : variable 
(define apply-var
  (lambda (var)
    (cases varVal var
      (variable (espacio)
                (vector-ref espacio 0)))))

;; Funcion que permire saber si una variable en la posicion pos del store, esta determinada (no es igual a "_") o no 
;; pos: posicion de la variable a verificar en el store 
;; retorna true si la variable en la posicion pos esta determinada
;; retorna false en caso contrario
(define isdet?
  (lambda (pos)
    (not (equal? "_" (apply-almacen pos)))))

;_________________________________________________ ALMACEN _______________________________________________
;; Definicion del tipo de dato Almacen
;; variables: es un vector que almacena variables, cuando se crea una variable se almacena en el almacen con un "_" inidicando que no esta asignada aun
;; la posicion de cada variable en el almacen corresponde a una ligadura en el ambiente.
(define-datatype almacen almacen?
  (un-almacen (variables  vector?)))



;;Funcion para extender el almacén dada una lista de identificadores y retornando una lista de posiciones a variables en el alamcen 
;;en la misma posicion de los identificadores pasados como parametro
;;ids: lista de identificadores

(define extend-almacen 
  (lambda (ids)
    (map (lambda (x) (extend-almacen-aux)) ids)))

;;Permite extender el alamcen retornando la posicion donde se crea la variable que sera referenciada con la posicio retornada
;; no recibe argumentos
(define extend-almacen-aux
  (lambda ()
    (let
        ((sig-posicion-libre (prox-val-almacen)))
      (begin 
        (cases almacen almacen-inicial
          (un-almacen (variables) 
                      (set! almacen-inicial (un-almacen(list->vector 
                                                        (append (vector->list variables) 
                                                                (vector->list (make-vector 1 (nueva-variable "_")))))))))
        sig-posicion-libre))))


;; Función que retorna la próxima proxima referencia que se creara en el alamacen antes de crear la variable 
;; store : es el alamcen al cual se le calcula el tamaño
;; Pruebas:  (prox-val-almacen (extend-almacen 2 (un-almacen #())))
(define prox-val-almacen
  (lambda ()
    (cases almacen almacen-inicial
      (un-almacen (variables) 
                  (vector-length variables)))))


;; Funcion que busca en el almacén la refrencia y retorna el valor,
;; teniendo en cuenta que si en la primera busqueda del valor se encuentra un numero esto quiere decir 
;; que esta variable contenia una referencia , por ende se debe volver a usar apply-almacen con ese 
;; valor para encontrar el valor buscado.
;; pos : posicion de una variable en el almacen
;; store : el alamcen que contiene la variable
(define apply-almacen 
  (lambda (pos) 
    (cases almacen almacen-inicial
      (un-almacen (variables)
                  (let
                      ((valor (apply-var (vector-ref variables pos))))
                    (if (number? valor)
                        (apply-almacen valor)
                        valor))))))


;; Funcion que asigna un valor en la misma posicion de la referencia en el vector valores del almacen.
;; pos : posicion de la variable que se va a setear en el alamcen 
;; valor : el valor que se va a asignar a la variable en dicha poscion
;; store : el almacen que contiene la variable
(define set-variable-almacen  
  (lambda (pos valor)
    (cases almacen almacen-inicial
      (un-almacen (variables)
                  (set! almacen-inicial
                        (un-almacen (begin (let
                                               ((contenido (apply-var (vector-ref variables pos))))
                                             ;;Si la variable contiene un numero
                                             (if (number? contenido)
                                                 ;;entonces es porque esta apuntando a otra variable, entonces se setea la variable que esta en la posicion contenido
                                                 (vector-set! variables contenido (nueva-variable valor))
                                                 ;;en caso contrario entonces se setea la variabe que esta en pos
                                                 (vector-set! variables pos (nueva-variable valor))))
                                            variables)))))))

;; Funcion que permite que haya en el almacen una unica referenciacion en caso de que una variable 
;; apunte a otra variable en el almacen
(define refrescar-almacen 
  (lambda (old-ref new-ref)
    (set! almacen-inicial (un-almacen (list->vector (refrescar-almacen-aux old-ref new-ref))))))


(define refrescar-almacen-aux
  (lambda (old-ref new-ref)
    (cases almacen almacen-inicial
      (un-almacen (variables) 
                  (let 
                      ((vars (vector->list variables)))
                    (let loop ((vars vars))
                      (cond
                        [(null? vars) '()]
                        [(equal? old-ref (apply-var (car vars))) (cons (nueva-variable new-ref) (loop (cdr vars)))]
                        [else (cons (car vars) (loop (cdr vars)))])))))))

;;_______________________________________________ AMBIENTES __________________________________________
;Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (ids (list-of symbol?))
                       (pos (list-of number?))
                       (env environment?)) )


;Función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))      

;extend-env: <list-of symbol?> * <list-of number?> * enviroment -> enviroment
;Función que crea un ambiente extendido 
;ids: lista de identificadores de variables
;pos: lista de referencias a variables en el store
(define extend-env
  (lambda (ids pos env)
    (let
        ((new-env (extended-env-record ids pos env)))
      (begin (display "\n")
             (display "Ambiente: ")
             (display new-env)
             (display "\n")
             (display "\n")
             new-env))))

;apply-env: environment * symbol -> number
;Función que busca un símbolo en un ambiente
;sym: identifficador de variable a buscar
;env: ambiente en el cual se busca el identificador
(define apply-env
  (lambda (sym env)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env " ~s  no a sido definida" sym))
      (extended-env-record (syms pos env)
                           (let ((n (list-find-position sym syms)))
                             (if (number? n)
                                 (list-ref pos n)
                                 (apply-env sym env)))))))

;Funciones auxiliares para encontrar la posición de un símbolo
;en la lista de símbolos de un ambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (equal? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))




;;_______________________________________________ PRIMITIVAS ______________________________________

;    ;--Operaciones sobre puertos
;    (primitiva ("newport") newport-prim)
;    (primitiva ("isport?") isport-prim)
;    (primitiva ("send") send-prim)
;    ;--Operaciones de celdas
;    (primitiva ("newcell") newcell-prim)
;    (primitiva ("iscell?") iscell-prim)
;    (primitiva ("@") acces-prim)
;    (primitiva ("setcell") setcell-prim)
;--Operaciones aritmeticas

(define apply-primitiva
  (lambda (prim args)
    (cases primitiva prim
      (isdet-prim () (isdet-prim?(car args)))
      (isfree-prim () (isfree-prim? (car args)))
      (sum-prim ()   (operacion-naria + args))
      (mult-prim () (operacion-naria * args))
      (sub-prim () (operacion-binaria - args))
      (div-prim () (operacion-binaria / args))
      (menor-prim () (operacion-binaria-comparacion < args))
      (meneq-prim () (operacion-binaria-comparacion <= args))
      (mayor-prim () (operacion-binaria-comparacion > args))
      (mayig-prim () (operacion-binaria-comparacion >= args))
      (igual-prim () (igual-prim? args))
      (orelse-prim () (orelse args))
      (andthen-prim () (andthen args))
      
      (else "No se reconoce el operador"))))

;operaciones de comparacion o igualdad
(define igual-prim?
  (lambda (args)
    (cond
      ;[((list-of regVal?) args) (equal-registros? (car args) (cadr args))]
      [else (equal? (car args) (cadr args))])))

;operaciones con variables 
(define isdet-prim? 
  (lambda (x)
    (boolean-to-symbol (not (equal? x "_")))))

(define isfree-prim? 
  (lambda (x)
    (boolean-to-symbol (equal? x "_"))))

; Operaciones aritmeticas
(define operacion-binaria 
  (lambda (op args)
    (if ((list-of string?) args)
        (let
            ((nums (strings-to-numbers args)))
          (if (numeros-mismo-tipo? nums)
              (number-to-string (op (car nums) (cadr nums)))
              (eopl:error 'operacion-binaria  "No es posible operar numeros de tipos diferentes ~s" args)))
        (eopl:error 'operacion-binaria  "Los argumentos nos son numeros ~s" args))))

;; Permite evaluar primitivas que reciben numeros y retornan booleanos 
(define operacion-binaria-comparacion
  (lambda (op args)
    (if ((list-of string?) args)
        (let
            ((nums (strings-to-numbers args)))
          (if (numeros-mismo-tipo? nums)
              (boolean-to-symbol (op (car nums) (cadr nums)))
              (eopl:error 'operacion-binaria  "No es posible operar numeros de tipos diferentes ~s" args)))
        (eopl:error 'operacion-binaria  "Los argumentos nos son numeros ~s" args))))


(define operacion-naria
  (lambda (op args)
    (if ((list-of string?) args)
        (number-to-string 
         (let 
             ((nums (strings-to-numbers args)))
           (if (numeros-mismo-tipo? nums)
               (cond 
                 [(equal? op +) (let loop1 ((op op) (nums nums)) (if (null? nums) 0 (+ (car nums) (loop1 op (cdr nums)))))]
                 [(equal? op *) (let loop2 ((op op) (nums nums)) (if (null? nums) 1 (* (car nums) (loop2 op (cdr nums)))))]
                 [else (eopl:error 'operacion-naria  "No se reconoce el operador ~s" op)])
               (eopl:error 'operacion-naria  "Los argumentos nos son del mismo tipo ~s" args))))
        (eopl:error 'operacion-naria  "Los argumentos nos son numeros ~s" args))))

;;Operaciones logicas
(define orelse
  (lambda (args)
    (let 
        ((val1 (symbol-to-boolean (car args)))
         (val2 (symbol-to-boolean (cadr args))))
      (if (not (or (number? val1) (number? val2)))
          (boolean-to-symbol (or val1 val2))
          (begin
            (display (string-append (symbol->string val1) " " (symbol->string val2)))
            (display "\n")
            (eopl:error 'orelse "Los argumentos ingresados no son booleanos o no son del mismo tipo")) ))))

(define andthen
  (lambda (args)
    (let 
        ((val1 (symbol-to-boolean (car args)))
         (val2 (symbol-to-boolean (cadr args))))
      (if (not (or (number? val1) (number? val2)))
          (boolean-to-symbol (and val1 val2))
          (begin
            (display (string-append (symbol->string val1) " " (symbol->string val2)))
            (display "\n")
            (eopl:error 'andthen "Los argumentos ingresados no son booleanos o no son del mismo tipo")) ))))


;; Convertidores 
(define boolean-to-symbol
  (lambda (sym)
    (cond
      [(equal? sym #t) 'true]
      [(equal? sym #f) 'false]
      [else (eopl:error 'symbol-to-boolean "No fue posible convertir el booleano ~s a symbol" sym) ])))

(define symbol-to-boolean
  (lambda (sym)
    (cond
      [(equal? sym 'true) #t]
      [(equal? sym 'false) #f ]
      [else (eopl:error 'symbol-to-boolean "El valor ~s no es booleano" sym) ])))

(define strings-to-numbers
  (lambda (strs)
    (map (lambda (x) (string-to-number x)) strs)))


(define string-to-number
  (lambda (str)
    (if (not (string=? str "_"))
        (cond 
          [(equal? "~" (substring str 0 1)) (* (string->number (substring str 1)) -1)]
          [else (string->number str)])
        (eopl:error 'string-to-number "No fue posible convertir el argumento ~s  a un numero" str))))

(define numbers-to-strings
  (lambda (nums)
    (map (lambda (x) (number-to-string x)) nums)))

(define number-to-string
  (lambda (num)
    (cond 
      [(< num 0) (string-append "~" (number->string (* num -1)))]
      [else (number->string num)])))


(define numeros-mismo-tipo? 
  (lambda (li)
    (cond 
      [(exact? (car li)) ((list-of exact?) li)]
      [(inexact? (car li)) ((list-of inexact?) li)]
      [else #f])))



;;________________________________ PRIMITIVA DE UNIFICACION ________________________________


(define unificaciones
  (lambda (vars values) 
    (map (lambda (x y) (unificacion x y)) vars values)))

(define unificacion
  (lambda (a b)
    (begin 
      (display "Unificacion\n")
      (display "val1: ")
      (display a )
      (display "  val2:  ")
      (display b)
      (display "\n\n")
      (cond
        
        
        ;caso 1: cuando tenermos dos numeros, es decir dos posiciones en el store.
        [(and (number? a) (number? b)) (unificar-variables-en-posiciones a b)]
        
        
        ;caso 2: cuando tenermos una posicion en el store y un valor representado en string, se verifica si la variable en la posicion a en el store esta determinada o no.
        ;en caso de estar determinada entonces se arroja un error ya que no se puede volver a asignar el valor de una variable, en caso contrario se asigna el string a la
        ;variable en la posicion a en el store
        [(and (number? a) (string? b)) (if (not (isdet? a))  
                                           (set-variable-almacen a b) 
                                           (eopl:error 'unificacion-prim "2) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" a b a))]
        
        
        
        ;caso 3: el mismo caso anterior pero con a string y b posicion en el store
        [(and (string? a) (number? b)) (if (not (isdet? b))  
                                           (set-variable-almacen b a) 
                                           (eopl:error 'unificacion-prim "3) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" b a b))]
        
        
        
        ;caso 4: cuando a y b son string, la unificacion es validad si los strings son iguales
        [(and (string? a) (string? b)) (if (equal? a b) 0 (eopl:error 'unificacion "4) No es posible unificar ~s = ~s, debido a que no son iguales" a b))]
        
        
        
        ;caso 5: cuando a es una posicion en el store y b es un valor de tipo procedimiento, la unificacion es valida si a no a sido determinado
        [(and (number? a) (procVal? b)) (if (not (isdet? a)) 
                                            (set-variable-almacen a b)
                                            (eopl:error 'unificacion "5) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" a b a))]
        
        
        
        ;caso 6: el mismo caso 5 pero cuando a es un valor de tipo procedimiento y b es una posicion en el store
        [(and (procVal? a) (number? b)) (if (not (isdet? b)) 
                                            (set-variable-almacen b a)
                                            (eopl:error 'unificacion "6) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" b a b))]
        
        
        ;Procedimientos ??????
        ;        ;caso 7: cuando a y b son valores de tipo procedimiento, la unificacion es valida si los dos procedimientos son iguales
        ;        [(and (procVal? a) (procVal? b) (if (not (equal? a b)) (eopl:error 'unificacion-prim "No es posible unificar ~s = ~s, debido a que no son iguales" b a b)))]
        ;       
        
        
        
        ;caso 8: cuando a es una posicion en el store y b es un valor de tipo registro, la unificacion es valida si a no esta determinada
        [(and (number? a) (regVal? b) (if (not (isdet? a))
                                          (set-variable-almacen a b)
                                          (eopl:error 'unificacion "8) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" a b a)))]
        
        
        
        ;caso 9: el mismo caso 8 pero cuando b es una posicion en el store y a es un valor de tipo procedimiento
        [(and (regVal? a) (number? b)) (if (not (isdet? b))
                                           (set-variable-almacen b a)
                                           (eopl:error 'unificacion "9) No es posible unificar ~s = ~s, debido a que  ~s ya fue asignada" b a b))]
        
        
        
        
        ;caso 10: cuando a y b son valores de tipo registro entonces la unificacion tiene en cuenta los siguientes aspectos:
        ;a) el numero de campos de ambos registros deben tener el mismo tamaño
        ;b) los nombres deben de los registros deben ser iguales
        ;c) los cada campo debe apuntar a la misma variable en el almacen
        [(and (regVal? a) (regVal? b)) (unificar-registros a b)]
        
        
        
        [else (eopl:error 'unificacion-prim "No es posible unificar ~s = ~s " a b)])
      
      )))

(define unificar-variables-en-posiciones
  (lambda (pos-var1 pos-var2)
    
    (begin 
      
      
      (display "unificar-variables-en-posiciones")
      (display "\n")
      (display "   pos-var1: ")
      (display pos-var1)
      (display "   pos-var2: ")
      (display pos-var2)
      (display "\n\n")
      
      (cond
        
        
        ;caso 1: cuando pos-var1 y pos-var2 ambas estan determinadas, entonces la unificacion es valida, si el valor de la variable que esta en la posicion 
        ;pos-var1 es igual al valor de la variable que esta en la posicion pos-var2 en el store
        [(and (isdet? pos-var1) (isdet? pos-var2)) (if (equal? (apply-almacen pos-var1) (apply-almacen pos-var2))
                                                       0
                                                       (eopl:error 'unificar-variables-en-posiciones "No es posible unificar ~s = ~s, debido a que no son iguales" pos-var1 pos-var2))]
        
        
        ;caso 2: cuando pos-val1 , pos-val2 ambas no estan determinadas, la politica tomada para este caso es:
        ;la variable que esta en la posicion pos-var2 va a contener pos-var1, es decir la variable en la posicion pos-var2 en el store
        ;va a apuntar a la variable que esta en la posicion pos-var1
        ;IMPORTANTE: Cuando pos-var1 = pos-var2 , significa que hacen referencia a la misma variable en el store, entonces
        ;como no estan permitidas las referencias ciclicas se genera un error
        [ (and (not(isdet? pos-var1)) (not(isdet? pos-var2))) 
          (if (not (equal? pos-var1 pos-var2))
              (begin
                (set-variable-almacen pos-var2 pos-var1)
                (refrescar-almacen pos-var2 pos-var1))
              (eopl:error 
               'unificar-variables-en-posiciones 
               "No es posibe unificar ~s = ~s debido a que hacen referencia a la misma variable en el almacen y no estan permitidas las referencias ciclicas  " pos-var1 pos-var1 ))]
        
        
        
        ;caso 3: post-var1 no determinada y pos-var2 determinada
        [(and (not (isdet? pos-var1)) (isdet? pos-var2)) (set-variable-almacen pos-var2 pos-var1)]
        
        
        ;caso 4: post-var1 determinada y pos-var2 no determinada
        [(and (isdet? pos-var1) (not (isdet? pos-var2))) (set-variable-almacen pos-var1 pos-var2)]
        [else (eopl:error 'unificar-variables-en-posiciones "No fue posible unificar ~s = ~s " pos-var1 pos-var2)]))))


;; Funion que permite unificar los parametros con los agumentos de una funcion o procedimiento
;; param: numero que representa la ubicacion en el almacen del parametro formal de una funcion o procedimiento
;; arg: numero o valor que es pasado al procedimiento o funcion para ser unificado con el parametro,
;; cuando arg es un numero entonces representa la posicion en el almacen del argumento pasado como parametro

(define unificar-parametro
  (lambda (param arg)
    (set-variable-almacen param arg)))

(define unificar-parametros
  (lambda (params args)
    (map (lambda (param arg) (unificar-parametro param arg)) params args)))

;; Funcion que permite unificar registros 
;; un registro se puede unificar con otro, siempre y cuando tengan el mismo numero de campos.

(define unificar-registros
  (lambda (reg1 reg2)
    (let 
        ((refs-reg1 (registro-referencias reg1))
         (refs-reg2 (registro-referencias reg2)))
      (if (equal? (length refs-reg1) (length refs-reg2))
          (unificaciones refs-reg1 refs-reg2)
          (eopl:error 'unificar-registros "No es posible unificar ~s con ~s porque no tienen el mismo numero de campos" (registro-nombre reg1) (registro-nombre reg2))))))




;;______________________________________________ EVAL EXPRESION ___________________________________

;; Permite evaluar una expresion de tipo programa 
;; prog: arbol de sintaxis abstracta a evaluar 
(define eval-program 
  (lambda (prog)
    (cases programa prog
      (a-programa (body)
                  (eval-expresion body ambiente-inicial)))))


(define eval-expresion
  (lambda (exp env)
    (cases expresion exp
      (skip-exp () 'skip)
      (entero-exp (int) int)
      (flotante-exp (float) float)
      ;;El resultado de evaluar una variable anonima es crea una variable en el almacen, cuando se extiende el almacen se retorna la posicion de la variable nueva 
      (var-anon-exp (id) (extend-almacen-aux))
      (var-exp (sym) (apply-almacen (apply-env sym env)))
      (local-exp (id ids cuerpo) (eval-local-exp (append (list id) ids) cuerpo env))
      (set-exp (exp1 exp2) (let ((val1 (eval-expresion-en-referencias exp1 env))
                                 (val2 (eval-expresion-en-referencias exp2 env))) (unificacion val1 val2)))
      (proc-exp (nom ids cuerpo) (make-procedure nom ids cuerpo env))
      (fun-exp (nom ids cuerpo) (make-function nom ids cuerpo env))
      (app-exp (exp exps) (eval-app-exp exp exps env))
      (prim-exp (prim exps) (chequear-y-aplicar-primitiva prim exps env))
      (atom-exp (etiqueta) etiqueta) 
      (record-exp (nom eti exp etis exps) (eval-record-exp nom eti exp etis exps env))
      (asg-record-exp (asg-record-type exp) (eval-asg-record-type asg-record-type (eval-expresion exp env) env))
      (if-exp (test-exp true-exp test-exps true-exps false-exp) (eval-if-exp (append (list test-exp) test-exps) (append (list true-exp) true-exps) false-exp env))
      (for-exp (exp1 exp2 cuerpo) (eval-for-exp exp1 exp2 cuerpo env))
      (else "Expresion no encontrada"))))

;; Funcion que evalua una lista de expresiones 
;; exps : lista de expresiones a evaluar
;; env : el ambiente en el cual se evaluan las expresiones
;; eval-exps : <list-of expresion> * environment -> <list-of scheme-value>
(define eval-exps
  (lambda (exps env)
    (map (lambda (exp) (eval-expresion exp env))
         exps)))

(define eval-expresion-en-referencias
  (lambda (exp env)
    (cases expresion exp
      (var-exp (sym) (apply-env sym env))
      (else (eval-expresion exp env)))))

(define eval-expresiones-en-referencias
  (lambda (exps env)
    (map (lambda (x) (eval-expresion-en-referencias x env)) exps)))




;;Funcion que permite evaluar una expresion de tipo local
;;id: identificador 
;;ids: identificadores
;;cuero: cuerpo del local
;;env: una expresion de tipo local extiende el ambiente en el que se evalua mapeando los identificadores
(define eval-local-exp
  (lambda (ids cuerpo env)
    (if (chequear-ids-local-exp ids)
        (let
            ((refs (extend-almacen ids)))                    
          (eval-cuerpo cuerpo (extend-env ids refs env)))
        (eopl:error 'chequear-ids-local "La expresion local no tiene permitido identificadores repetidos ~s" ids))))

;;Funcion que permite chequear en una lista si todos los idetificadores son diferentes
;;idis: lista de identificadores
(define chequear-ids-local-exp
  (lambda (ids)
    (cond 
      [(null? ids) #t]
      [(number? (list-find-position (car ids) (cdr ids))) #f]
      [else (chequear-ids-local-exp (cdr ids))]
      )))

;; Funcion que permite evaluar las expresiones en un cuerpo 
;; el resultado de evaluar un cuerpo es el valor de la ultima expresion
;; eval-cuerpo : cuerpo * environment * store -> string
;; body : es el cuerpo
;; env : el ambiente en el cual se evalua el cuerpo 
(define eval-cuerpo
  (lambda (body env)
    (cases cuerpo body
      (cuerpoc (exp exps) 
               (let 
                   ((vals (eval-exps (append (list exp) exps) env)))
                 (list-ref vals (- (length vals) 1)))))))


;; Funcion que permite evaluar procedimientos y funciones
;; rator: procedimiento o funcion
;; exps: expresiones a evaluar en el procedimiento o funcion
(define eval-app-exp
  (lambda (rator exps env)
    (let
        ((closure (eval-expresion rator env))
         (args (eval-expresiones-en-referencias exps env))
         (refs (extend-almacen exps)))
      (begin
        (unificar-parametros refs args)
        (apply-app closure refs)))))


(define apply-app
  (lambda (proc refs)
    (cases procVal proc
      (proc-closure (nom ids cuerpo env) (begin (eval-cuerpo cuerpo (extend-env ids refs env)) 0))
      (func-closure (nom ids cuerpo env) (eval-cuerpo cuerpo (extend-env ids refs env)))
      (else (eopl:error 'apply-app "El valor obtenido ~s no es de tipo procedimiento" proc)))))


;;Permite evaluar las primitivas 
;;prim: primitiva a aplicar
;;exps: expresiones a evaluar en las primitivas
;;env: el ambiente en el que se evaluan las primitivas
(define chequear-y-aplicar-primitiva
  (lambda (prim exps env)
    (cases primitiva prim
      (unif-prim () (eval-unif-prim exps env))
      (else (eval-prim-exp prim exps env)))))

;; Funcion que permite aplicar la primitiva de unificacion ={ }
(define eval-unif-prim
  (lambda (exps env)
    (let 
        ((val1 (eval-expresion-en-referencias (car exps) env))
         (val2 (eval-expresion-en-referencias (cadr exps) env)))
      (unificacion val1 val2))))

;; Funcion que permite evaluar otras primitivas diferentes a la de la unificacion 
(define eval-prim-exp
  (lambda (prim exps env)
    (let
        ((args (eval-exps exps env)))
      (if (not (contiene-referencia? args))
          (apply-primitiva prim args)
          (apply-primitiva prim (let loop ((args args))
                                  (cond
                                    [(null? args) '()]
                                    [(number? (car args)) (cons (apply-almacen (car args)) (loop (cdr args)))]
                                    [else (cons (car args) (loop (cdr args)))])))))))


;; Funcion que permite saber si una lista de argumentos evaluados en el ambiente posee una referencia o numero 
;; en caso de poseer una referencia o numero entonces se buscar el valor de esa referencia en el almacen 
;; esto porque las primitivas son aplicadas solo con valores mas no  con referencias 
(define contiene-referencia?
  (lambda (args)
    (cond 
      [(null? args) #f]
      [(number? (car args)) #t]
      [else (contiene-referencia? (cdr args))])))


;;Funcion que permite evaluar expresiones de tipo registro. 
;;Ligando cada campo del registro a una varible en el alamcen 
;;nom: nombre del registro
;;eti: etiqueta del primer campos del registro
;;etis: resto de etiquetas del los campos de un registro
;;exp: expresion que correspinde a la primera etiqueta del registro
;;exps: expresiones que corresponde a las etis en su respectivo orden en el registro
(define eval-record-exp
  (lambda (nom eti exp etis exps env)
    (let
        ((campos (append (list eti) etis))
         (expss (append (list exp) exps)))
      (let 
          ((values (eval-expresiones-en-referencias expss env))
           (refs (extend-almacen campos)))
        
        (begin 
          (unificaciones values refs)
          (nuevo-registro nom campos refs))))))

;; Funcion que permite obtener el valor de un campos de un registro
;; asg-record-type: es una expresion que denota anidamientos o expresiones simples
;; es decir expresiones de este tipo .(.R1.a).campo1 o .R1.campo1
;; campo: campo cuyo valor se desea encontrar
(define eval-asg-record-type
  (lambda (asg-record-type1 campo env)
    (cases asg-record-type asg-record-type1
      (asg-record-simple (exp) (apply-almacen (apply-registro (eval-expresion exp env) campo)))
      (asg-record-anidado (asg-record-type1 exp) (eval-asg-record-type asg-record-type1 (eval-expresion exp env #f))))))


;;Funcion que permite evaluar una expresion de tipo condicional 
;;test-exps: lista con las expresiones booleanas a comprobar
;;true-exps: lista con las expresiones a evaluar en caso de que el primero de la lista test-exps tenga como valor booleano #t
;;false-exp: expresion a evaluar en caso de que la lista de  test-exps este vacia
;;env: ambiente en el que se evalua la expresion if-exp 
(define eval-if-exp
  (lambda (test-exps true-exps false-exp env)
    (cond
      [(null? test-exps) (eval-expresion false-exp env)]
      [(bool-value (eval-expresion (car test-exps) env)) (eval-expresion (car true-exps) env)]
      [else (eval-if-exp (cdr test-exps) (cdr true-exps) false-exp env)])))

;; Funcion que permite evaluar la expresion for
(define eval-for-exp 
  (lambda (exp1 exp2 cuerpo env)
    (let
        ((val1 (eval-expresion exp1 env))
         (val2 (eval-expresion exp2 env)))
      (if (and (string? val1) (string? val2))
          (let 
              ((num1 (string-to-number val1)) 
               (num2 (string-to-number val2)))
            (if (< num1 num2)
                (apply-for num1 num2 cuerpo env)
                (eopl:error 'eval-for-exp "no es posible aplicar for porque el primer valor ( ~s )  debe ser menor que el segundo valor ( ~s ) " val1 val2)))
          (eopl:error 'eval-for-exp "uno o los dos valores ~s , ~s no es numero" val1 val2)))))


;; Permite aplicar la expreson for
;(define apply-for
;  (lambda (ini fin cuerpo env)
;    (for-each 
;     (lambda (x) 
;       (begin (display "\n") 
;              (display (eval-cuerpo cuerpo env))
;              (display "\n"))) 
;       (iota (- fin ini)))))

(define apply-for
  (lambda (ini fin cuerpo env)
    (for-each 
     (lambda (x) 
       (eval-cuerpo cuerpo env)) 
     (iota (- fin ini)))))


;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))


;;Permite saber el valor booleano de una expresion 
;;value: expresion a convertir a booleano
(define bool-value
  (lambda (value)
    (symbol-to-boolean value)))


;; Almacen de datos
;; almacen-global: permite construir un almacén inicial.
(define almacen-inicial
  (un-almacen (list->vector '())))


;Ambiente inicial
(define ambiente-inicial
  (empty-env))

;;(interpretador)