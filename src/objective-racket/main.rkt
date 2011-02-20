#lang racket

(require 
 (for-syntax racket)
 (for-syntax "utils.rkt")
 (for-syntax "table.rkt")
 (for-syntax "check.rkt")
 "utils.rkt"
 "table.rkt")

(define-for-syntax *member-ids*
  '(public-class-field?
    public-class-method?
    private-class-method?))

(define-for-syntax (bind-public-class-methods member)
  (syntax-case member (public static)
    ((public static method-name method-params method-body ...)
     #'(define method-name
         (λ method-params
           method-body ...)))))

(define-for-syntax (bind-public-class-fields member)
  (syntax-case member (public static)
    ((public static var-name var-value)
     #'(define var-name var-value))))

(define-for-syntax (process-members name members)
  (deftable members-db)
  
  ;; Class member definitions can appear in any order. Here, we order them.
  (for-each
   (λ (member-id)
     (for-each
      (λ (member)
        (members-db-add+ member-id member))
      (filter 
       (λ (member)
         (parse-member member-id member))
       members)))
   *member-ids*)
  
  (let ((def-public-class-fields
          (members-db-map+ 'public-class-field? bind-public-class-fields))
        (def-public-class-methods
          (members-db-map+ 'public-class-method? bind-public-class-methods)))
    (with-syntax
        ((class-dispatcher
          (make-id name "dispatch-class-~a" name)))
      #`(begin
          #,@def-public-class-fields
          #,@def-public-class-methods
          (define (init)
            (set! meta class-dispatcher)
            meta)
          (define (class-dispatcher msg . args)
            (case msg
              ((name) (object-name #,name))
              (else
               (error "Unknown message to class dispatcher" msg))))
          (init)
          'ok
          ))))

(define-syntax (defclass stx)
  (syntax-case stx ()
    ((defclass name parent . members)
     #`(define (name)
         (define meta 'meta)
         #,(process-members #'name (syntax->list #'members))))))

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
          (+ 1 2) 
          x)
  
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