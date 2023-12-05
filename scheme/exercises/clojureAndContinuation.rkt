#lang racket

(define cont-lambda #f)
(define (f x)
  (call/cc (lambda (k)
             (set! cont-lambda (lambda ()
                                 (set! x (- x 1))
                                 (k)))))
  (display x)
  (display "\n"))

(f 3)
(cont-lambda) ; Value is 2: we have modified the x binding in f trhough a closure. Even after calling the continuation the binding changed

