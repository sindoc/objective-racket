#lang racket

(require 
 (for-template racket)
 (for-syntax "members.rkt")
 "members.rkt")

(provide qualify-member)

(define-syntax (qualify-member stx)
  (syntax-case stx ()
    ((qualify-member member)
     #`(with-handlers 
           ((exn:fail?
             (λ (exn)
               (error
                'unqualified-class-member 
                "Invalid class member \n ~a" exn))))
         (syntax-case member #,*member-keywords*
           #,@(map
               (λ (qualifier matcher)
                 #`((#,@matcher)
                    (#,qualifier member)))
               *member-qualifiers*
               *member-qualifier-matchers*))))))