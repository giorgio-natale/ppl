#lang racket

(define *breaks* '())

(define (push k) (set! *breaks* (append (list k) *breaks*)))
(define (pop) (when (cons? *breaks*) (let [(head (car *breaks*))] (set! *breaks* (cdr *breaks*)) head)))

(define (break ret-val) ((pop) ret-val))

(define-syntax For
    (syntax-rules (from to do)
        [(_ var from min to max do body ...)
        (let* [(emin min) (emax max) (inc (if (< emin emax) + -))]
            (call/cc (lambda (k)
                (push k)
                (let loop [(var emin)]
                    (unless (= var emax) body ... (loop (inc var 1))))
                (pop k))))]))


(For i from 1 to 10 do
    (displayln i)
    (when (= i 5)
        (break #t)))