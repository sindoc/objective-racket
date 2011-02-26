#lang racket

(require 
 (for-syntax "members.rkt")
 (for-syntax "utils.rkt")
 "members.rkt"
 "utils.rkt")

(provide qualify-member)

(define-syntax (qualify-member stx)
  (syntax-case stx ()
    ((_ member)
     #`(with-handlers 
           ((exn:fail? ; FIXME: too general
             (λ (exn)
               (error
                'unqualified-class-member 
                "Invalid class member \n ~a" exn))))
         (show member)
         (syntax-case member (public private static)
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

(define test-3
  (qualify-member #'(private static c 10)))

(list test-1 test-2 test-3)