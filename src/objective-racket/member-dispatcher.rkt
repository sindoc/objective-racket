#lang racket

(require 
 (for-template racket)
 (for-syntax "members.rkt")
 (for-syntax "utils.rkt")
 "members.rkt"
 "utils.rkt")

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

;(define test-1
;  (qualify-member 
;   #'(public static field a 1)))
;
;(define test-2
;  (qualify-member 
;   #'(public 
;       static 
;       method
;       b (x)
;       (define y (+ x y))
;       y)))
;
;(define test-3
;  (qualify-member #'(private static field c 10)))
;
;(list test-1 test-2 test-3)