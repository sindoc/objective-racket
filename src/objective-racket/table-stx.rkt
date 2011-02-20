#lang racket

(require 
 "utils.ss"
 "table.rkt"
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt"))

(provide 
 deftable
 deftable-for-syntax)

(define-for-syntax (_deftable stx define)
  (syntax-case stx ()
    ((_ name)
     (with-syntax 
         ((name-add  (make-id stx "~a-add"  #'name))
          (name-add+ (make-id stx "~a-add+" #'name))
          (name-get  (make-id stx "~a-get"  #'name))
          (name-map  (make-id stx "~a-map"  #'name))
          (name-map+ (make-id stx "~a-map+" #'name))
          (name-show (make-id stx "~a-show" #'name))
          (define    define))
       #'(begin
           (define name (make-dict 'name))
           (define (name-add  k v) (name 'add  k v))
           (define (name-add+ k v) (name 'add+ k v))
           (define (name-get  k  ) (name 'get  k))
           (define (name-map  f  ) (name 'map  f))
           (define (name-map+ k f) (name 'map+ k f))
           (define (name-show    ) (name 'show void)))))))

(define-syntax (deftable stx)
  (_deftable stx #'define))

(define-syntax (deftable-for-syntax stx)
  (_deftable stx #'define-for-syntax))