#lang racket

(require 
 (for-syntax "utils/main.rkt")
 (for-syntax "utils/table-stx.rkt")
 (for-syntax "member-dispatcher.rkt")
 "utils/table-stx.rkt"
 "utils/main.rkt")

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
            (gen-member (λ (mo) (mo 'binder)) 'public-class-field))
          (def-private-class-fields
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
           def-private-class-fields
           (define (meta-dispatch meta-msg)
             (case meta-msg
               ((name) class-name)
               #,@(members-db-map+ 'public-class-field field-dispatcher)
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (meta-init))))))

(defclass Account Object
  (public static field n-accounts 0)
  (private static field total-funds 1)
  (public static method fun (x) x)
  (private static field total-corporate-funds 2))

(define acc (Account))
(acc 'n-accounts)