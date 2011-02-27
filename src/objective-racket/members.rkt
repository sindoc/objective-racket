#lang racket

(require
 (for-syntax "utils/main.rkt")
 (for-syntax racket)
 (for-template racket))

(provide 
 (all-defined-out)
 (for-syntax
  (all-defined-out)))

(define-for-syntax *member-qualifiers* null)
(define-for-syntax *member-qualifier-matchers* null)
(define-for-syntax *member-keywords* null)

(define-for-syntax *ldots* '~ldots)

(define-for-syntax (replace-specials stx)
  (map
   (λ (x)
     (case x
       ((*ldots*) '...)
       (else x)))
   (syntax->datum stx)))
  
(define-for-syntax (add-qualifier! id matcher keywords)
  (set! *member-qualifiers* 
        (cons id *member-qualifiers*))
  (set! *member-qualifier-matchers*
        (cons (replace-specials (cadr (syntax->list matcher)))
              *member-qualifier-matchers*))
  (set! *member-keywords*
        (remove-duplicates
         (append 
          (syntax->datum (cadr (syntax->list keywords)))
          *member-keywords*))))

(define-syntax (def-member-qualifier stx)
  (syntax-case stx ()
    ((_ id (matcher matcher-expr keywords) (action-id action) ...)
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
             (add-qualifier! #'id #'matcher-expr #'keywords))
           (define (qualifier member)
             (define (qualifier-dispatcher msg . args)
               (case msg
                 ((name) qualifier-name)
                 ((member) member)
                 ((action-id) 
                  (apply action (list member))) ...
                 (else
                  (error qualifier-name "unknown message (~a) to ~a"
                         msg "to class member qualifier"))))
             qualifier-dispatcher))))))

(define (common-field-binder member)
  (let* ((as-list (syntax->datum member))
         (var-name (cadddr as-list))
         (var-value (car (cddddr as-list))))
    `(define ,var-name ,var-value)))

(define (common-field-caller member)
  (let* ((as-list (syntax->datum member)))
    (cadddr as-list)))

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