#lang racket

(require
 "../main.rkt")

(defclass Account Object
  (public static field n-accounts 0)
  (private static field total-funds 1)
  (public static method fun (x) x)
  (private static field total-corporate-funds 2))

(define acc (Account))
(acc 'n-accounts)