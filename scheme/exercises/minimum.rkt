#lang racket

(define minimum (lambda (first . rest)
    (cond
      [(empty? rest) first]
      [(< first (car rest)) (apply minimum (cons first (cdr rest)))]
      [else (apply minimum rest)])))

(minimum -1 3 2 4 1)