#lang racket

(require
 "member-collector.rkt")

(provide
 (for-syntax *member-qualifiers*)
 (for-syntax *member-qualifier-matchers*)
 (for-syntax *member-keywords*))

(define (common-field-binder member)
  (syntax-case member ()
    ((_ _ _ var-name var-value)
     #'(define var-name var-value))))

(define (common-field-caller member)
  (syntax-case member ()
    ((_ _ _ var-name _)
     #'var-name)))

(define (common-method-binder member)
  (syntax-case member ()
    ((_ _ _ method-name method-params method-body ...)
     #'(define method-name 
         (λ method-params
           method-body ...)))))

(define (common-method-caller member)
  (syntax-case member ()
    ((_ _ _ method-name etc ...)
     #'method-name)))

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
   '(public static method))
  (binder common-method-binder)
  (caller common-method-caller))

(def-member-qualifier 
  private-class-method
  (matcher
   '(private static method method-name method-params method-body *ldots*)
   '(private static method))
  (binder common-method-binder)
  (caller common-method-caller))

(def-member-qualifier
  public-instance-field
  (matcher
   '(field var-name)
   '(field))
  (binder 
   (λ (stx)
     (syntax-case stx ()
       ((_ var-name)
        #'(define var-name null)))))
  (caller
   (λ (stx)
     (syntax-case stx ()
       ((_ var-name)
        #'var-name)))))

(def-member-qualifier
  public-instance-method
  (matcher
   '(method name params body *ldots*)
   '(method))
  (caller
   (λ (stx)
     (syntax-case stx ()
       ((_ name _ ...)
        #'name))))
  (binder
   (λ (stx)
     (syntax-case stx ()
       ((_ name params body ...)
        #'(define name
            (λ params
              body ...)))))))