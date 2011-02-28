#lang racket

(require 
 (for-syntax "utils/main.rkt")
 (for-syntax "utils/table-stx.rkt")
 (for-syntax "member-dispatcher.rkt")
 "utils/table-stx.rkt"
 "utils/main.rkt")

(provide defclass)

(deftable-for-syntax members-db)

(define-for-syntax (gen-member proc key #:wrapper [wrap #'begin])
  #`(#,wrap #,@(members-db-map+ key proc)))

(define-for-syntax (field-dispatcher member-object)
  (let ((ref (member-object 'caller)))
    #`((#,ref) #,ref)))

(define-for-syntax (get-binder member-object)
  (member-object 'binder))

(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass class parent . members)
     (begin
       (for-each 
        (λ (member)
          (define member-object (qualify-member member))
          (members-db-add+ (member-object 'name) member-object))
        (syntax->list #'members)))
     (with-syntax
         ((bind-public-class-fields
            (gen-member get-binder 'public-class-field))
          (bind-private-class-fields
            (gen-member get-binder 'private-class-field))
          (bind-public-class-methods
           (gen-member get-binder 'public-class-method))
          (meta-dispatch 
           (make-id #'class "dispatch-~a" #'class))
          (class-name #''class))
       #`(define (class)
           (define meta void)
           (define (meta-init)
             (set! meta meta-dispatch)
             meta)
           bind-public-class-fields
           bind-private-class-fields
           bind-public-class-methods
           (define (meta-dispatch meta-msg)
             (case meta-msg
               ((name) class-name)
               #,@(members-db-map+ 'public-class-field field-dispatcher)
               #,@(members-db-map+ 'public-class-method field-dispatcher)
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (meta-init))))))