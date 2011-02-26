#lang racket

(require
 "table-stx.rkt"
 (for-syntax "utils.rkt"))

(provide 
 (all-defined-out)
 (for-syntax
  (all-defined-out)))

(define-for-syntax *member-qualifiers* null)
(define-for-syntax *member-qualifier-matchers* null)

(deftable-for-syntax member-objects)

(define-for-syntax (add-qualifier! id matcher)
  (set! *member-qualifiers* 
        (cons id *member-qualifiers*))
  (set! *member-qualifier-matchers*
        (cons (cadr (syntax->list matcher))
              *member-qualifier-matchers*)))

(define-syntax (def-member-qualifier stx)
  (syntax-case stx ()
    ((_ id (matcher matcher-expr) (action-id action) ...)
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
             (add-qualifier! #'id #'matcher-expr))
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
         (var-name (caddr as-list))
         (var-value (cadddr as-list)))
    `(define ,var-name ,var-value)))

(define (common-field-caller member)
  (let* ((as-list (syntax->datum member)))
    (caddr as-list)))

(def-member-qualifier public-class-field
  (matcher '(public static var-name var-value))
  (binder common-field-binder)
  (caller common-field-caller))

(def-member-qualifier private-class-field
  (matcher '(private static var-name var-value))
  (binder common-field-binder)
  (caller common-field-caller))
  
(def-member-qualifier public-class-method
  (matcher '(public static method-name method-params method-body)))