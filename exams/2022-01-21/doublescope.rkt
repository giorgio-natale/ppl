#lang racket
(define-syntax block
    (syntax-rules (then where <-)
        [(_ (body1 ...) then (body2 ...) where (var <- val1 val2) ...)
        (begin
            (let [(var val1) ...]
                body1 ...)
            (let [(var val2) ...]
                body2 ...))]))

(block
    ((displayln (+ x y))
    (displayln (* x y))
    (displayln (* z z)))
then 
    ((displayln (+ x y))
    (displayln (* z x)))
where (x <- 12 3) (y <- 8 7) (z <- 3 2))