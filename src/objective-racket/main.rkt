#lang racket

(require 
 (for-syntax "utils.rkt")
 (for-syntax "table-stx.rkt")
 (for-syntax "check.rkt")
 "table-stx.rkt"
 "utils.rkt")

(deftable-for-syntax members-db)

(define-for-syntax (gen-member proc key #:wrapper [wrap #'begin])
  #`(#,wrap #,@(members-db-map+ key proc)))

(define-for-syntax (field-dispatcher mo)
  (let ((ref (mo 'caller)))
    #`((#,ref) #,ref)))

(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass class parent . members)
     (for-each 
      (λ (member)
        (define member-object (qualify-member member))
        (members-db-add+ (member-object 'name) member-object))
      (syntax->list #'members))
     (with-syntax
         ((def-public-class-fields
            (gen-member (λ (mo) (mo 'binder)) 'private-class-field))
          (meta-dispatch 
           (make-id #'class "dispatch-~a" #'class))
          (class-name #''class))
       #`(define (class)
           (define meta void)
           (define (meta-init)
             (set! meta meta-dispatch)
             meta)
           def-public-class-fields
           (define (meta-dispatch meta-msg)
             (case meta-msg
               ((name) class-name)
               #,@(members-db-map+ 'private-class-field field-dispatcher)
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (meta-init))))))

(defclass Account Object
  (public static n-accounts 0)
  (public static total-funds 1)
  (public static fun (x) x)
  (private static total-corporate-funds 2))

((Account) 'total-funds)