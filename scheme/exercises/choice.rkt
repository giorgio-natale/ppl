#lang racket

(define (insert-between v xs)
  (cond ((null? xs) xs)
        ((null? (cdr xs)) xs)
        (else (cons (car xs)
                    (cons v (insert-between v (cdr xs)))))))

(define (display-all . vs)
  (for-each display (insert-between " " vs)))

(define *paths* '())

(define (fail)
  (if (null? *paths*)
      (error "No more choices\n")
      (let [(head (car *paths*))]
        (head))))

(define (push-path f)
  (set! *paths* (cons f *paths*)))

(define (pop-path)
  (set! *paths* (cdr *paths*)))

(define (choose options)
  (if (null? options)
      (fail)
      (let [(next-option (car options))]
        (call/cc (lambda (k)
                 (set! options (cdr options)) 
                 (push-path (lambda ()
                              (if (null? options)
                                  (begin (pop-path) (fail))
                                  (let [(head (car options))]
                                    (set! options (cdr options))
                                    (set! next-option head)
                                    (k)))))))
        next-option)))


(let [(x (choose '(1 2 3))) (y (choose '(4 5 6)))]
  (display-all "X:" x "Y:" y "\n")
  (fail))