#lang racket

(define-syntax multifun
    (syntax-rules()
    [(_ (fname) (param ...) (body))
    (define (fname param ...) body)]
    [(_ (fname1 fname2 ...) (param ...) (body1 body2 ...))
    (begin (define (fname1 param ...) body1) (multifun (fname2 ...) (param ...) (body2 ...)))]))

(multifun (f g) (x)
    ((+ x x x)
    (* x x)))

