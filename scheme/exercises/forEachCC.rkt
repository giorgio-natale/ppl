#lang racket

; CARE: cdr doesn't work with an empty list

(define *queue* '())

(define (snoc lst f)
  (if (null? lst)
      (list f)
      (cons (car lst) (snoc (cdr lst) f))))

(define (append-queue f)
  (set! *queue* (snoc *queue* f)))

(define-syntax for-each/cc
  (syntax-rules ()
    [(_ condition iterable body)
     (let [(m-list iterable) (m-condition condition)]
       (unless (null? iterable)
         (let [(last-value (call/cc (lambda (k)
                                    (let loop [(sublist m-list)]
                                      (let [(head (car sublist))]
                                        (cond [(m-condition head) (append-queue (lambda ()
                                                                                  (k (cdr sublist))))]))  
                                      (unless (null? (cdr sublist)) (loop (cdr sublist)))))))]
         (cond [(list? last-value) (for-each body last-value)]))))]))

(define (prof-for-each/cc cnd L body)
  (when (cons? L)
    (let [(x (car L))]
      (call/cc (lambda (c)
                 (when (cnd x)
                   (set! *queue* (append *queue* (list c))))
                 (body x)))
      (prof-for-each/cc cnd (cdr L) body))))

(define (use-scc)
  (let [(head (car *queue*))]
    (set! *queue* (cdr *queue*))
    (head)))

(prof-for-each/cc odd?
             '(1 2 3 4)
             (lambda(x) (displayln x)))
(use-scc)