#lang racket

(require 
 (for-syntax "utils.rkt")
 "utils.rkt")

(provide (all-defined-out))

(define-for-syntax *member-qualifiers* '())

(define-syntax (def-member-qualifier stx)
  (syntax-case stx ()
    ((_ id (op val) ...)
     (with-syntax
         ((qualifier 
           (make-id #'id "~a" #'id))
          (qualifier-dispatcher 
           (make-id #'id "dispatch-~a" #'id))
          (qualifier-name #''id))
       (set! *member-qualifiers* 
             (cons #'id *member-qualifiers*))
       #'(begin
           (define (qualifier member)
             (define (qualifier-dispatcher msg . args)
               (case msg
                 ((name) qualifier-name)
                 ((op) val) ...
                 (else
                  (error qualifier-name "unknown message (~a) to ~a"
                         msg "to class member qualifier"))))
             qualifier-dispatcher)
           )))))

(def-member-qualifier public-class-field
  (matcher '(public static var-name var-value)))

(def-member-qualifier public-class-method
  (matcher '(public static method-name method-params method-body)))

(define-syntax (qualify-member stx)
  (define matcher-id "~a-matcher")
  (syntax-case stx ()
    ((_ member)
     #`(with-handlers 
           ((exn:fail? ; FIXME: too general
             (λ (exn)
               (error 
                'unqualified-class-member 
                "Invalid class member \n ~a" exn))))
         #,@(map
             (λ (q)
               #`(define #,(make-id q matcher-id q) ((#,q member) 'matcher)))
             *member-qualifiers*)
         (syntax-case member (static)
           #,@(map
               (λ (q)
                 #`(#,(make-id q matcher-id q)
                    (#,q member)))
               *member-qualifiers*))))))

(define test-1
  (qualify-member #'(public static test-1 1)))

(define field (pattern (public static var-name var-value)))
(define method (pattern (public static method-name method-params method-body)))

(syntax-case #'(public static n 1) ()
  (method
   (show 'method))
  (field
   (show 'field)))