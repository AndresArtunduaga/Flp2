#lang eopl
(require "ambientes.ss")
(require "SintaxisyGramatica.ss")
(provide (all-defined-out))
;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     '(x y z emptylist)
     (list 4 2 5 '() 
           ;;(closure '(x y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (add-prim) (list (var-exp 'x) (var-exp 'x)) ) '())))(empty-env))
     )
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
                   (if (true-value? (eval-bool-exp test-exp env))
                       (eval-expression true-exp env)
                       (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc)))))))

(define eval-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
           (igual-pred (args)
                       (if (= (eval-expression (car args) env)
                              (eval-expression (cadr args) env))
                           1 0))
           (cero-pred (arg)
                      (if (zero? (eval-expression arg env))
                          1 0))
           (mayor-pred (args)
                         (if (> (eval-expression (car args) env)
                                (eval-expression (cadr args) env))
                             1 0))
           (menor-pred (args)
                      (if (< (eval-expression (car args) env)
                             (eval-expression (cadr args) env))
                          1 0)))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (if (> (length args)1) (foldl + 0 args) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (substract-prim () (if (> (length args)1) (restar args) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (mult-prim () (if (> (length args)1) (foldl * 0 args) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (incr-prim () (if (= (length args)1) (+ (car args) 1) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (decr-prim () (if (= (length args)1) (- (car args) 1) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (min-prim () (if (= (length args)1) (- (car args)) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))
      (div-prim () (if (= (length args) 2) (floor (/ (car args) (cadr args))) (eopl:error 'apply-primitive "Attempt to apply non-procedure ~s" prim)))   
      (cons-prim () (cons (car args) (cadr args)))
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (list-prim () args)
      )))

;;************
(define foldl
  (lambda (proc acc l)
    (if (null? l)
        acc
        (foldl proc (proc acc (car l)) (cdr l)))))

(define restar
  (lambda (l)
    (if (null? l)
        0
        (- (car l) (restar (cdr l))))))


;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))