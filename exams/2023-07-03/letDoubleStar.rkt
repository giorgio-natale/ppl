#lang racket

; NOTE: Important exercise [dot in syntax rules]


(define (counter)
  (let [(counter 0)] (lambda () (let [(old counter)] (set! counter (+ 1 counter)) old))))
(define-syntax let**
    (syntax-rules (def:)
        [(_ def: _ ((x x-bind)) exp ...) (let [(x x-bind)] exp ...)]
        [(_ def: def-val (x) exp ...) (let [(x def-val)] exp ...)]
        [(_ def: _ ((x x-bind) . rest) exp ...) (let [(x x-bind)] (let** def: x rest exp ...))]
        [(_ def: def-val (x . rest) exp ...) (let [(x def-val)] (let** def: x rest exp ...))]
        ))

;(let** def: 5 ((y 6) x (z 7) ehy) (displayln y) (displayln x) (displayln z) (displayln ehy))

(define myC (counter))
(let** def: (myC) (x y z) (displayln x) (displayln y) (displayln z))

(let** def: #f (a (b 1) (c (+ b 1)) d (e (+ d 1)) f) (list a b c d e f))
