#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table-stx.rkt")
 (for-syntax "check.rkt")
 "utils.rkt")

(define-syntax (defclass stx)
  (deftable members-db)
  (syntax-case stx ()
    ((defclass class parent . members)
     (begin
       (for-each 
        (λ (member)
          (define member-object (qualify-member member))
          (members-db-add+ (member-object 'name) member-object))
        (syntax->list #'members)))
     (with-syntax
         ((def-public-class-fields
            #`(begin
                #,@(members-db-map+ 
                    'public-class-field 
                    (λ (mo)
                      (mo 'binder)))))
          (meta-dispatch (make-id #'class "dispatch-~a" #'class))
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
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (meta-init)
           )))))

(defclass Account Object
  (public static n-accounts 1)
  (public static total-funds 1)
  (public static fun (x) x))

((Account) 'name)