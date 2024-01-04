#lang racket

(define (make-counter init-val)
  (let [(current-val init-val)]
    (define (get-val)
      current-val)
    (define (increment)
      (let [(res current-val)]
        (set! current-val (+ current-val 1))
         (void)))
    (lambda (message . args)
      (apply (case message
        [(get-val) get-val]
        [(increment) increment]) args))))

(define (make-logged-counter init-val)
  (let [(parent (make-counter init-val))
        (logs '())]
    (define (get-val)
      (let [(current-val (parent 'get-val))]
        (set! logs (append logs (list (string-append "Read value: " (number->string current-val)))))
        current-val))
    (define (increment)
      (set! logs (append logs (list "Incremented!")))
      (parent 'increment))
    (define (show-log)
      (displayln logs))
    (lambda (message . args)
      (apply (case message
        [(get-val) get-val]
        [(increment) increment]
        [(show-log) show-log]) args))))

(define-syntax def-let
  (syntax-rules ()
    [(_ (name . ()) val body ...) (let ((name val)) body ...)]
    [(_ (name . other-names) val body ...) (let ((name val)) (def-let other-names val body ...))]))

(define-syntax multi-define
  (syntax-rules ()
    [(_ [(name . args) . ()] [(exp ...) . ()]) (define (name . args) exp ...)]
    [(_ [(name . args) . other-names] [(exp ...) . other-bodies]) (begin (define (name . args) exp ...) (multi-define other-names [other-bodies]))]))


(define-syntax zip-with-sum
  (syntax-rules ()
    [(_ (x ...) (y ...)) (list (+ x y) ...)]))

(zip-with-sum (1 2 3) (4 5 6))

(define-syntax ignore-second-and-inc
  (syntax-rules ()
    [(_ (x1 x2) ...) (list (+ x1 1) ...)]))

(ignore-second-and-inc (1 2) (3 4) (5 6))


(define-syntax class
  (syntax-rules ()
    [(_ class-name (attr ...) ( ( (meth-name . meth-args)  meth ...) ...))
     (define (class-name . args)
       (def-let (attr ...) (void)
         (multi-define (meth-name . meth-args) ... (meth ...) ... )))]))

; TODO: end this