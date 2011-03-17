#!/usr/bin/gosh

(load "./util.scm")


;; problem 1. common number
;;-------------------------
(define ans1
  (apply + (collection-if 
            (lambda (x)
              (or (= (mod x 3) 0)
                  (= (mod x 5) 0)))
            (... 1 999))))

;; problem 2. fibonatti
;;-------------------------
; t = a1
; a1 = a1 + a2
; a2 = a1 + a2
(define ans2
  (let ((n 0)
        (t 0)
        (a1 0)
        (a2 1))
    (while (<= a2 4000000)
           ;(format #t "~A ~A~%" a1 a2)
           (if (= (mod a2 2) 0)
               (set! n (+ a2 n)))
           (set! t a2)
           (set! a2 (+ a1 a2))
           (set! a1 t))
    n))

;; problem 3. prime factor
;;-------------------------
; primes output the prime numbers
; > (primes 20)
; -> (2 2 5)
(define ans3
  (car (reverse! (primes 600851475143))))

;; problem 4. largest palindrome
