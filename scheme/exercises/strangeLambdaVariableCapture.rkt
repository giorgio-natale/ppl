#lang racket

; In this example we stack multiple lambdas that capture a mutating higher scope variable (f).
; All lambdas at the end reference the last value of the binding.
; On the other hand, if we create new variables and capture those (f1), each lambda at the end sees the value that the variable
; had when the lambda was defined.
; Possible explanation: In general lambda capture the REFERENCE of the variable. In f we change the value that the variable has. In f1
; we create new variables

(define m-stack '())

(define (push-stack f)
  (set! m-stack (cons f m-stack)))

(define (pop-stack)
  (let [(head (car m-stack))]
    (set! m-stack (cdr m-stack))
    head))

(define (f initial-value)
  (let [(val initial-value)]
    (let loop []
      (push-stack (lambda () val))
      (set! val (- val 1))
      (unless (<= val 0) (loop)))))

(define (f1 initial-value)
  (let loop [(val initial-value)]
    (push-stack (lambda () val))
    (unless (<= val 0) (loop (- val 1)))))

(display "First version:\n")
(f 5)
((pop-stack))
((pop-stack))
((pop-stack))
((pop-stack))
(display "Second version:\n")
(f1 5)
((pop-stack))
((pop-stack))
((pop-stack))
((pop-stack))