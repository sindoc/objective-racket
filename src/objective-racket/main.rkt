#lang racket

(require 
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt")
 "utils.rkt"
 "table.rkt")

(define-for-syntax (get-member-qualifier member qualifier-nr)
  (list-ref (syntax->datum member) (- qualifier-nr 1)))

(define-for-syntax (not-null-member-qualifier? member qualifier-nr)
  (not (null? (get-member-qualifier member qualifier-nr))))

(define-for-syntax (public-field? member)
  (eq? (get-member-qualifier member 1) 'public))

(define-for-syntax (public-instance-field? member)
  (and (public-field? member)
       (not-null-member-qualifier? member 2)))

(define-for-syntax (class-field? member)
  (eq? (get-member-qualifier member 2) 'static))

(define-for-syntax (public-class-field? member)
  (and (public-field? member)
       (class-field? member)
       (not-null-member-qualifier? member 3)
       (not-null-member-qualifier? member 4)))

(define-for-syntax (process-members stx members)
  (deftable members-db)
  
  (for-each
   (λ (member)
     (when (public-class-field? member)
       (members-db-add+ 'public-class-fields member)))
   members)
  
  (for-each
   (λ (member)
     (when (public-instance-field? member)
       (members-db-add+ 'public-instance-fields member)))
   members)
  
  (let ((public-class-fields
         (members-db-map+ 
          'public-class-fields 
          (λ (member)
            (syntax-case member (static)
              ((public static var-name var-value)
               #'(define var-name var-value)))))))
    
    (show public-class-fields)
    #''ok))
  
(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass name parent . members)
     #`(define (name)
         (define meta '())
         #,(process-members stx (syntax->list #'members))))))

(defclass NCard Object
  
  ;; Public (Static) Fields
  (public name)
  (public static number-of-cards 0)
  (public static cool-meta 0)
  
  ;; Private (Static) Fields
;  (private status)
;  (private static nice-cards 10)
;  
;  ;; Public (Static) Methods 
;  (public play ()
;          (show "Play the card"))
;  (public static all-names-as-list ()
;          (show "Put the name of all cards in a list"))
;  
;  ;; Private (Static) Methods 
;  (private check ()
;           (show "Checks stuff"))
;  (private static all-statuses-as-list ()
;           (show "Put the status of all cards in a list"))
  )

(NCard)