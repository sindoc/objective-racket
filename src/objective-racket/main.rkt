#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table-stx.rkt")
 (for-syntax "check.rkt")
 "utils.rkt"
 "table-stx.rkt")

(define-syntax (defclass stx)
  (deftable members-db)
  (syntax-case stx ()
    ((defclass class parent . members)

     (with-syntax
         ((meta-dispatch (make-id #'class "dispatch-~a" #'class))
          (class-name #''class)
          )
       #`(define (class)
           (define meta void)
           (define (meta-init)
             (set! meta meta-dispatch)
             meta)
           (define (meta-dispatch meta-msg)
             (case meta-msg
               ((name) class-name)
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (meta-init)
           )))))

(defclass NCard Object
  (public static number-of-cards 0)
  )

((NCard) 'name)
;((NCard) 'foo)