#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table-stx.rkt")
 (for-syntax "check.rkt")
 "utils.rkt"
 "table-stx.rkt")

(define-for-syntax *member-ids*
  '(public-class-field?
    public-class-method?
    private-class-method?))

(define-syntax (defclass stx)
  (deftable members-db)
  (syntax-case stx ()
    ((defclass class parent . members)
     (begin
       (for-each
        (λ (member-id)
          (for-each
           (λ (member)
             (members-db-add+ member-id member))
           (filter 
            (λ (member)
              (parse-member member-id member))
            (syntax->list #'members))))
        *member-ids*)
       (members-db-show))
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