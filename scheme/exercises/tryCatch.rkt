#lang racket

(define *handlers* '())

(define (push-handler handler)
  (set! *handlers* (cons handler *handlers*)))

(define (pop-handler)
  (let [(head (car *handlers*))]
    (set! *handlers* (cdr *handlers*))
    head))

(define (throw exception)
  (if (null? *handlers*)
      (error exception)
      ((pop-handler) exception)))

(define-syntax try
  (syntax-rules (catch)
    [(_ tbody ... (catch exception cbody ...))
     (call/cc (lambda (k)
                (push-handler (lambda (exception-to-handle)
                                (if (equal? exception-to-handle exception)
                                    (k (begin cbody ...))
                                    (throw exception-to-handle))))
                (let [(res (begin tbody ...))]                       ; NOTE: we need this because otherwise the catch
                  (pop-handler)                                      ;       is not transparent for the calling: we would always return void
                  res)))]))

(define abs-only-for-positives(lambda (x)
   (if (< x 0)
       (throw 'negative-param)
       x)))

(define (abs-also-for-negatives x)
  (try (abs-only-for-positives x)
       (catch 'negative-param
              (display "Negative value!\n")
              (* x -1))))

(abs-also-for-negatives -1)