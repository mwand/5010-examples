#lang racket

(require "datatypes.rkt")
(require "environments.rkt")

(provide pgm-value)           ;; Program -> Value

;; Program -> Value
;; RETURNS: the value of the program
(define (pgm-value pgm)
  (expr-value
    (program-main pgm)
    (decls2env (program-decls pgm))))

;; Expr Env -> Value
;; RETURNS: the value of the given expression in the given environment
(define (expr-value expr env)
  (cond
    [(const? expr) (const-value expr)]
    [(variable? expr)
     (env-lookup env
                 (variable-name expr))]
    [(diff? expr)
     (-
      (expr-value (diff-expr1 expr) env)
      (expr-value (diff-expr2 expr) env))]
    [(ifzero? expr)
     (if
      (zero? 
       (expr-value (ifzero-test expr) env))
      (expr-value (ifzero-then expr) env)
      (expr-value (ifzero-else expr) env))]
    [(call? expr)
     (local
       ((define the-args
          (map
           (lambda (expr) (expr-value expr env))
           (call-exprs expr)))
        (define the-fcn
          (env-lookup env (call-name expr)))
        (define the-locals
          (fcn-decl-args the-fcn))
        (define the-body
          (fcn-decl-body the-fcn)))
       (expr-value the-body
                   (make-env the-locals the-args
                             env)))]
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Utility Functions.  
;;; These are here because they know about declarations.

;; decls2env : Decls -> Env
(define (decls2env decls)
  (make-env
    (map decl-name decls)
    (map decl-value decls)
    empty-env))





