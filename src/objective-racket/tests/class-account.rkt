#lang racket

(require
 "../main.rkt")

(defclass Account Object
  (public static field n-accounts 0)
  (private static field total-funds 100)
  (public static method access-private-fields (x)
          (+ x total-funds))
  (private static method foo () 'foo)
  (private static field total-corporate-funds 2))

(define acc (Account))
((acc 'access-private-fields) 10)