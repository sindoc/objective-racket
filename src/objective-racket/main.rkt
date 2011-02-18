#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt")
 (for-syntax "check.rkt")
 "utils.rkt"
 "table.rkt")

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
       (members-db-add+ 'public-class-methods member)))
   members)
    
  (with-syntax
      ((public-class-fields
        #`(begin
            #,@(members-db-map+
                'public-class-fields
                (λ (member)
                  (syntax-case member (static)
                    ((public static var-name var-value)
                     #'(define var-name var-value)))))))
       (public-class-methods
        (car (members-db-map+
                'public-class-methods
                (λ (member)
                  (syntax-case member (public static)
                    ((public static method-name (method-param ...)
                             method-body ...)
                     #'(define method-name
                         (λ (method-param ...)
                           method-body ...))))))))
       )
    #`(begin
        public-class-fields
        public-class-methods
        'ok)))

(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass name parent . members)
     #`(define (name)
         (define meta 'meta)
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
  
  (public static all-names-as-list (x)
          (+ 2 3)
          (show "Put the name of all cards in a list"))
  
  ;; Private (Static) Methods 
  ;(private check ()
  ;         (show "Checks stuff"))
  ;(private static all-statuses-as-list ()
  ;         (show "Put the status of all cards in a list"))
  )

(NCard)