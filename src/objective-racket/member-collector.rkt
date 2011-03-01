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

(define-for-syntax (replace-specials lst)
  (map
   (λ (x)
     (case x
       ((*ldots*) '...)
       (else x)))
   lst))
  
(define-for-syntax (add-qualifier! id matcher keywords)
  (set! *member-qualifiers* 
        (cons id *member-qualifiers*))
  (set! *member-qualifier-matchers*
        (cons (replace-specials 
               (syntax->datum (cadr (syntax->list matcher))))
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
                 ((meta-member?) 
                  (eq? (cadr (syntax->datum member)) 'static))
                 ((action-id)
                  (datum->syntax
                   member
                   (syntax->datum (apply action (list member))))) ...
                 (else
                  (error qualifier-name "unknown message (~a) to ~a"
                         msg "to class member qualifier"))))
             qualifier-dispatcher))))))