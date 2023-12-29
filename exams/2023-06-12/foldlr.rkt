#lang racket

(define (_foldlr f z lst acc z1)
    (if (null? lst)
        (list z (acc z1))
        (let* [(x (car lst)) (xs (cdr lst))]
            (_foldlr f (f z x) xs (lambda (right-z) (acc (f right-z x))) z1))))

(define (foldlr f z lst)
    (_foldlr f z lst (lambda (right-z) right-z) z))


(foldlr string-append "" '("a" "b" "c"))