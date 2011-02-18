#lang racket

(require 
 (for-syntax "utils.rkt")
 "utils.rkt")
(provide
 public-class-method?
 public-class-field?
 private-class-method?
 parse-member)

(define (handler exn) #f)
(define-syntax (check stx)
  (syntax-case stx ()
    ((check member conditions ...)
     #'(begin
         (set! member (syntax->datum member))
         (with-handlers ((exn:fail? handler))
           (and conditions ...))))))

(define (get-access-modifier member)
  (car member))

(define (get-scope member)
  (cadr member))
(define (after-scope member)
  (cddr member))
  
(define (public? member)
  (eq? (get-access-modifier member) 'public))

(define (private? member)
  (eq? (get-access-modifier member) 'private))

(define (static? member)
  (eq? (get-scope member) 'static))

(define (field-definition? exp)
  (and 
   (<= (length exp) 2)
   (not (null? (car exp)))
   (not (null? (cadr exp)))))

(define (method-definition? exp)
  (and
   (not (null? (car exp)))
   (not (null? (caddr exp)))))

(define (public-class-field? member)
  (check 
   member
   (public? member)
   (static? member)
   (field-definition? 
    (after-scope member))))

(define (public-class-method? member)
  (check
   member
   (public? member)
   (static? member)
   (method-definition? 
    (after-scope member))))

(define (private-class-method? member)
  (check 
   member
   (private? member)
   (static? member)
   (method-definition?
    (after-scope member))))

(define-syntax (parse-member stx)
  (syntax-case stx ()
    ((parse-member qualifier member)
     (with-syntax
         ((pred 
           (datum->syntax stx #'qualifier)))
       #`(apply (eval pred) (list member))))))

;(public-class-field? #'(public static nr-of 0))
;(public-class-method? #'(public static nr-of 0))
;(public-class-method? #'(public static go () x))
;(private-class-method? #'(private static go (x) x))
;(public-class-field? #'(public static foo (x) x))