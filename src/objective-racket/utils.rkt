#lang racket

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

(define (vector-map! v f)
  (define last (- (vector-length v) 1))
  (let loop
    ((i 0))
    (vector-set! v i (f i (vector-ref v i)))
    (unless (>= i last)
      (loop (+ i 1))))
  v)

(define (true? pred)
  (eq? pred #t))
(define (false? pred)
  (eq? pred #f))

(define (identity x) x)

(define (inc n)
  (+ n 1))
(define (dec n)
  (- n 1))

(define (in-range? step start end)
  (cond
    ((and (>= step start)
          (<= step end)) #t)
    (else #f)))

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

;; Exported Abstractions
;; ------------------------------------------------------------------------
(provide
 ;vector-map!
 in-range?
 identity
 inc
 dec
 true?
 ;false?
 gentext-format-key-val-pair
 gentext-newline
 gentext-println
 gentext-fancy-line
 show
 symbol-append
 deep-map
 make-id)
