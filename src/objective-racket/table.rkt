#lang racket

(require 
 "utils.ss"
 (for-syntax "utils.rkt"))

(provide deftable)

(define (make-dict name)
  (define self '())
  (define tab (make-hash))
  (define (init)
    (set! self dispatch-dict)
    self)
  (define (dispatch-dict op key . rest)
    (define (not-found) #f)
    (case op
      ((get)
       (let ((entry (hash-ref tab key not-found)))
         (when entry
           (show "Get entry" entry))
         entry))
      ((add)
       (let ((value (car rest)))
         (show "Adding value:" value "for entry:" key "to table:" name)
         (hash-set! tab key value)))
      ((add+)
       (let ((value (car rest)))
         (unless (self 'get key)
           (self 'add key null))
         (show "Adding value:" value "for bucket entry:" key "to table:" name)
         (hash-set! 
          tab key
          (cons value (self 'get key)))))
      ((show)
       (hash-map tab gentext-format-key-val-pair))
      ((map) 
       (let ((proc (car rest)))
         (hash-map tab proc)))
      ((map+)
       (show "\n\n\n\n" op key rest)
       (let ((fun (car rest))
             (match (self 'get key)))
         (when (not (list? match))
           (error "map+ must be used with a bucket list"))
         (map fun match)))))
  (init))

(define-syntax (deftable stx)
  (syntax-case stx ()
    ((_ name)
     (with-syntax 
         ((name-add  (make-id stx "~a-add"  #'name))
          (name-add+ (make-id stx "~a-add+" #'name))
          (name-get  (make-id stx "~a-get"  #'name))
          (name-map  (make-id stx "~a-map"  #'name))
          (name-map+ (make-id stx "~a-map+" #'name))
          (name-show (make-id stx "~a-show" #'name)))
       #'(begin
           (define name (make-dict 'name))
           (define (name-add  k v) (name 'add  k v))
           (define (name-add+ k v) (name 'add+ k v))
           (define (name-get  k  ) (name 'get  k))
           (define (name-map  f  ) (name 'map  f))
           (define (name-map+ k f) (name 'map+ k f))
           (define (name-show    ) (name 'show void)))))))