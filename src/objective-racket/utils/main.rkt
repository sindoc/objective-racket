#lang racket

(provide (all-defined-out))

;; Gentext
;; ------------------------------------------------------------------------

(define gentext-println
  (λ stuff
    (map 
     (λ (x)
       (display x)
       (display " "))
     stuff)
    (newline)))

(define (gentext-echo . args)
  (map display args))

(define (gentext-key-val-separator)
  (display ":"))

(define (gentext-newline)
  (newline))

(define (gentext-space)
  (gentext-echo " "))

(define (gentext-fancy-line)
  (gentext-println "********************************"))

(define (gentext-format-key-val-pair key val)
  (gentext-echo key)
  (gentext-key-val-separator)
  (gentext-space)
  (gentext-println val))
  
;; Miscellaneous
;; ------------------------------------------------------------------------

(define (identity x) x)

(define show gentext-println)

(define symbol-append
  (λ symbols
    (string->symbol 
     (apply 
      string-append 
      (map symbol->string symbols)))
    (newline)))

(define (atom? any)
  (not (list? any)))

(define (deep-map proc l)
  (cond ((null? l) null)
        ((atom? l) (proc l))
        (else 
         (cons (deep-map proc (car l))
               (deep-map proc (cdr l))))))

(define (make-id stx template . ids)
  (datum->syntax 
   stx 
   (string->symbol 
    (apply format template (map syntax->datum ids)))))