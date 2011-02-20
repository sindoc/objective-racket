#lang racket

(require "utils.rkt")

(provide make-dict)

(define (make-dict name)
  (define self '())
  (define tab '())
  (define (init)
    (set! self dispatch-dict)
    (set! tab (make-hash))
    self)
  (define (dispatch-dict op key . rest)
    (define (not-found) #f)
    (case op
      ((get)
       (let ((entry (hash-ref tab key not-found)))
         entry))
      ((add)
       (let ((value (car rest)))
         ;(show "Adding value:" value "for entry:" key "to table:" name)
         (hash-set! tab key value)))
      ((add+)
       (let ((value (car rest)))
         (unless (self 'get key)
           (self 'add key null))
         ;(show "Adding value:" value "for bucket entry:" key "to table:" name)
         (hash-set! 
          tab key
          (cons value (self 'get key)))))
      ((show)
       (hash-map tab gentext-format-key-val-pair))
      ((map) 
       (let ((proc (car rest)))
         (hash-map tab proc)))
      ((map+)
       (let ((proc (car rest))
             (match (self 'get key)))
         (cond 
           ((not (list? match))
            '())
           (else
            (map proc match)))))))
  (init))