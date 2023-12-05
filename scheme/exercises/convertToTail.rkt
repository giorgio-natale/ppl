#lang racket
(require racket/trace)

(define test-val 5)

(define (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(factorial test-val)

(define (tail-factorial n)
  (define (tail-factorial-helper n left)
    (if (< n 2)
        (* left 1)
        (tail-factorial-helper (- n 1) (* left n)) ))
  (tail-factorial-helper n 1))

(define (tail-factorial-cont n)
  (define (tail-factorial-helper n cont)
    (if (< n 2)
        (cont)
        (tail-factorial-helper (- n 1) (lambda () (* n (cont))))))
  (tail-factorial-helper n (lambda () 1)))

(tail-factorial test-val)

(tail-factorial-cont test-val)



(define (my-foldl f i L)
  (if (null? L)
      i
      (my-foldl f (f i (car L)) (cdr L) )))

(define (my-foldr f i L)
  (if (null? L)
      i
      (f (car L) (my-foldr f i (cdr L)))))

(define (tail-foldr f i L)
  (define (helper f i L cont)
    ))

(my-foldl + 0 '(3 4 5))
(my-foldr + 0 '(3 4 5))