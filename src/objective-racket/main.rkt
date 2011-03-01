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
  #`(#,wrap #,@(reverse (members-db-map+ key proc))))

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
          (members-db-add+ (member-object 'name) member-object)
          (let ((scope (if (member-object 'meta-member?) 'class 'object)))
            (members-db-add+ scope member-object)))
        (syntax->list #'members)))
     (with-syntax
         ((bind-class-members
           (gen-member get-binder 'class))
          (bind-instance-members
           (gen-member get-binder 'object))
          (object 
           (make-id #'class "~a-instance" #'class))
          (object-dispatch
           (make-id #'class "dispatch-~a-instance" #'class))
          (meta-dispatch 
           (make-id #'class "dispatch-~a" #'class))
          (class-name #''class))
       #`(define (class)
           (define meta null)
           (define (init-meta)
             (set! meta meta-dispatch)
             meta)
           bind-class-members
           (define (reify)
             (define self null)
             (define (init-object)
               (set! self object-dispatch)
               self)
             bind-instance-members
             (define (object-dispatch msg)
               (case msg
                 ((meta) meta)
                 #,@(members-db-map+ 
                     'public-instance-field field-dispatcher)
                 #,@(members-db-map+ 
                     'public-instance-method field-dispatcher)
                 (else
                  (error 
                   class-name "unknown message ~a to an instance of ~a" 
                   msg class-name))))
             (init-object))
           (define (meta-dispatch meta-msg)
             (case meta-msg
               ((name) class-name)
               ((reify) (reify))
               #,@(members-db-map+ 'public-class-field field-dispatcher)
               #,@(members-db-map+ 'public-class-method field-dispatcher)
               (else
                (error class-name "unknown message ~a" meta-msg))))
           (init-meta))))))