#lang racket

(require
 (for-syntax "utils.rkt"))

(provide 
 (all-defined-out)
 (for-syntax
  (all-defined-out)))

(define-for-syntax *member-qualifiers* null)
(define-for-syntax *member-qualifier-matchers* null)

(define-for-syntax (add-qualifier! id matcher)
  (set! *member-qualifiers* 
        (cons id *member-qualifiers*))
  (set! *member-qualifier-matchers*
        (cons (cadr (syntax->list matcher))
              *member-qualifier-matchers*)))

(define-syntax (def-member-qualifier stx)
  (syntax-case stx ()
    ((_ id (matcher matcher-expr))
     (with-syntax
         ((qualifier 
           (make-id #'id "~a" #'id))
          (qualifier-dispatcher 
           (make-id #'id "dispatch-~a" #'id))
          (qualifier-name #''id)
          (qualifier-matcher
           (make-id #'id "~a-matcher" #'id)))
       
       #'(begin
           (define-syntax _
             (add-qualifier! #'id #'matcher-expr))
           (define (qualifier member)
             (define (qualifier-dispatcher msg . args)
               (case msg
                 ((name) qualifier-name)
                 (else
                  (error qualifier-name "unknown message (~a) to ~a"
                         msg "to class member qualifier"))))
             qualifier-dispatcher))))))

(def-member-qualifier public-class-field
  (matcher '(public static var-name var-value)))

(def-member-qualifier public-class-method
  (matcher '(public static method-name method-params method-body)))