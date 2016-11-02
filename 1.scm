; Environment
;; implementation : assoc-list
;; env's key will be just symbol
(define env.init '())

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wront "Too less values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too much values")))
        ((symbol? variables) (cons (cons variables values) env))))

(define (lookup key env)
  ;;; symbol(key for variable) environment -> value
  (if (pair? env)
      (if (eq? (caar env) key)
          (cdar env)
          (lookup key (cdr env)))
      (wrong "No such binding" id))) ;; error

(define (update! key env value)
  ;;; -> value
  (if (pair? env)
      (if (eq? (caar env) key)
          (begin (set-cdr! (car env) value)
                 value) ;; value
          (update! key (cdr env) value))
      (wrong "No such  binding" key))) ;; error

; Function
;; Implementation : function
(define (make-function variables body env)
  ;;; list-of-params expression environment -> value
  (lambda (values)
    (eprogn body (extend  variables values))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn))) ;; error

; error
(define (wrong msg e)
  '())


(define (atom? e) (not (pair? e)))
(define the-false-value (cons "false" "boolean")) ;; issue
(define empty-begin 813) ;; issue
(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env)) ;; value
      empty-begin)) ;; value for (begin)
(define (evlis exps env) ;; order of evaluation
  (if (pair? exps)
      (let ((argument1 (evaluate (car exps) env)));;left to right
        (cons argument1 (evlis (cdr exps) env)))
      '()))
; eval
(define (evalute e env)
  ;;; expression environment -> value
  (if (atom? e)
      (cond
        ((symbol? e) (lookup e env))
        ((or (number? e)(string? e)(char? e)(boolean? e)(vector? e)) e) ;; autoquote
        (else (wrong "Cannot evaluate" e))) ;; error?
      (case (car e)
        ;; special operator redefine meaningless.
        ((quote)  (cadr e)) ;; (quote 1) ?
        ;; haven't specified the value for boolean.
        ((if)     (if (not (eq? (evalutae (cadr e) env) the-false-value))
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ((begin)  (eprogn (cdr e) env)) ;; value for eprogn?
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env))) 
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env))))))