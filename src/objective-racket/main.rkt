#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt")
 "utils.rkt"
 "table.rkt")

(define-for-syntax (member-matches? pattern member)
  (match (syntax->datum member)
    (pattern #t)))

(define-for-syntax (public-class-field? member)
  (match (syntax->datum member)
    ((list public static var-name var-value) #t)))
  
(define-for-syntax (public-class-method? member)
  (match (syntax->datum member)
    ((list public static method-name method-params method-body) #t)))

(define-for-syntax (process-members stx members)
  (deftable members-db)

  ;; Public Static Fields
  (for-each
   (λ (member)
     (when (public-class-field? member)
       (members-db-add+ 'public-class-fields member)))
   members)
  
  ;; Public Static Methods
  (for-each
   (λ (member)
     (when (public-class-method? member)
       (members-db-add+ 'public-class-method member)))
   members)
  
  (let ((public-class-fields
         (members-db-map+ 
          'public-class-fields 
          (λ (member)
            (syntax-case member (static)
              ((public static var-name var-value)
               #'(define var-name var-value)))))))
    
    (show public-class-fields)
    #`(begin
        #,@public-class-fields
        #''done)))
  
(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass name parent . members)
     #`(define (name)
         (define meta '())
         #,(process-members stx (syntax->list #'members))))))

(defclass NCard Object
  
  ;; Public (Static) Fields
  ; (public name)
  (public static number-of-cards 0)
  (public static cool-meta 0)
  
  ;; Private (Static) Fields
;  (private status)
;  (private static nice-cards 10)
;  
;  ;; Public (Static) Methods 
;  (public play ()
;          (show "Play the card"))
  (public static all-names-as-list ()
          (show "Put the name of all cards in a list"))
;  
;  ;; Private (Static) Methods 
;  (private check ()
;           (show "Checks stuff"))
;  (private static all-statuses-as-list ()
;           (show "Put the status of all cards in a list"))
  )

(NCard)