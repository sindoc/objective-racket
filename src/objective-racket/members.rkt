#lang racket

(require
 "member-collector.rkt")

(provide
 (for-syntax *member-qualifiers*)
 (for-syntax *member-qualifier-matchers*)
 (for-syntax *member-keywords*))

;(define (common-field-binder member)
;  (let* ((as-list (syntax->datum member))
;         (var-name (cadddr as-list))
;         (var-value (car (cddddr as-list))))
;    `(define ,var-name ,var-value)))

(define (common-field-binder member)
  (syntax-case member ()
    ((_ _ _ var-name var-value)
     #'(define var-name var-value))))

(define (common-field-caller member)
  (syntax-case member ()
    ((_ _ _ var-name _)
     #'var-name)))

(def-member-qualifier 
  public-class-field
  (matcher 
   '(public static field var-name var-value)
   '(public static field))
  (binder common-field-binder)
  (caller common-field-caller))

(def-member-qualifier 
  private-class-field
  (matcher 
   '(private static field var-name var-value)
   '(private static field))
  (binder common-field-binder)
  (caller common-field-caller))

(def-member-qualifier public-class-method
  (matcher 
   '(public static method method-name method-params method-body *ldots*)
   '(public static method)))