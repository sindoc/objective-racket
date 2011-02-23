#lang racket

(require 
 (for-syntax "utils.rkt")
 (for-syntax "members.rkt")
 "members.rkt"
 "utils.rkt")

(provide (all-defined-out))

(define-syntax (qualify-member stx)
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