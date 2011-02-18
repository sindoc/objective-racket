#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt")
 (for-syntax "check.rkt")
 "utils.rkt"
 "table.rkt")
   
(define-for-syntax (process-members name members)
  (deftable members-db)
  
  (for-each
   (λ (qualifier)
     (for-each
      (λ (member)
        (members-db-add+ qualifier member))
      (filter 
       (λ (member)
         (parse-member qualifier member))
       members)))
   '(public-class-field?
     public-class-method?
     private-class-method?))
  
  (members-db-show)
  
  (with-syntax
      ((public-class-fields
        #`(begin
            #,@(members-db-map+
                'public-class-field?
                (λ (member)
                  (syntax-case member (static)
                    ((public static var-name var-value)
                     #'(define var-name var-value)))))))
       (public-class-methods
        #`(begin
            #,@(members-db-map+
                'public-class-method?
                (λ (member)
                  (syntax-case member (public static)
                    ((public static method-name method-params method-body ...)
                     #'(define method-name
                         (λ method-params
                           method-body ...))))))))
       (class-dispatcher
        (make-id name "dispatch-class-~a" name))
       )
    #`(begin
        public-class-fields
        public-class-methods
        (define (init)
          (set! meta class-dispatcher)
          meta)
        (define (class-dispatcher msg . args)
          (case msg
            ((name) (object-name #,name))
            (else
             (error "Unknown message"))))
        (init)
        )))

(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass name parent . members)
     (with-syntax 
         ((meta #'meta))
       #`(define (name)
           (define meta 'meta)
           #,(process-members #'name (syntax->list #'members)))))))

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

  (public static foo (x) 
          (+ 1 2) x)
  
  (public static all-names-as-list (x) 
          (+ 1 2) 
          (show "Return all names in a list"))
  
  ;; Private (Static) Methods 
  ;(private check ()
  ;         (show "Checks stuff"))
  (private static all-statuses-as-list ()
           (show "Put the status of all cards in a list"))
  )

(NCard)