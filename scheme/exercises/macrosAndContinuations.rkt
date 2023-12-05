#lang racket



(define-syntax m-begin
  (syntax-rules ()
    [(_)(void)]
    [(_ b1 b2 ...)
     ((lambda () b1 b2 ...))]))

(define-syntax while
  (syntax-rules ()
    [(_ condition body ...)
      (let loop ()
         (when condition (begin body ... (loop))))]))

(define-syntax m-let
  (syntax-rules ()
    [(_ ((name value) ...) body ... )
     ((lambda (name ...) body ...) value ...)]))

(define-syntax m-let*
  (syntax-rules ()
    [(_ ((name value)) body ...)
     (m-let ((name value)) body ...)]
    [(_ ((n1 v1) (name value)...) body ...)
     (m-let ((n1 v1)) (m-let* ((name value) ...) body ...))]))

; TODO: define syntax for named let

; This for does not have a condition, but it can be exited through the tag defined in 'break: tag'
(define-syntax for
  (syntax-rules (from to break: do)
    [(_ counter from minExpr to maxExpr break: tag do body ...)
     (call/cc
      (lambda (tag)
        (let* [(min1 minExpr) (max1 maxExpr)]
          (let loop [(counter min1)]
            body ...
            (unless (> counter max1)
              (loop (+ counter 1)))))))]))

(for i from 1 to 10 break: get-out do
  (if (<= i 5)
      (display "Hello\n")
      (get-out)))
