#lang racket

(require 
 (for-syntax "utils.rkt")
 "utils.rkt")

(provide (all-defined-out))

(define-for-syntax *member-qualifiers* '())
(define-for-syntax *member-qualifier-matchers* '())

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
       
       (set! *member-qualifiers* 
             (cons #'id *member-qualifiers*))
       (set! *member-qualifier-matchers*
             (cons (cadr (syntax->list #'matcher-expr))
                   *member-qualifier-matchers*))
       #'(begin
           (define-syntax (qualifier-matcher stx)
             (syntax-case stx ()
               (qualifier-matcher (datum->syntax stx matcher-expr))))
           (define (qualifier member)
             (define (qualifier-dispatcher msg . args)
               (case msg
                 ((name) qualifier-name)
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
               (show 
                'unqualified-class-member 
                "Invalid class member \n ~a" exn))))
         (syntax-case member ()
           #,@(map
               (λ (qualifier matcher)
                 #`((#,@matcher)
                    (#,qualifier member)))
               *member-qualifiers*
               *member-qualifier-matchers*))))))

(define test-1
  (qualify-member #'(public static a 1)))

(define test-2
  (qualify-member #'(public static b (x) x)))