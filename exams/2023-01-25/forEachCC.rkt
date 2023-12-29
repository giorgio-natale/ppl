#lang racket

(define *queue* '())

(define (push-cc k)
    (set! *queue* (append *queue* (list k))))

(define (use-cc)
    (when (cons? *queue*)
        (let [(k (car *queue*))]
            (set! *queue* (cdr *queue*))
            (k))))

(define (for-each/cc pred l f)
    (when (not (null? l))
        (let [(h (car l)) (t (cdr l))]
            (f h)
            (when (pred h)
                (call/cc (lambda (k) (push-cc k))))
            (for-each/cc pred t f))))

(for-each/cc odd? '(1 2 3 4) (lambda (x) (display "[FUN]: ")(displayln x)))