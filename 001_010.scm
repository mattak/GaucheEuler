#!/usr/local/bin/gosh

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
;;------------------------------
(define ans4
  (car (reverse (collection-if palindromic? 
                               (mapeach * (.. 900 1000)(.. 900 1000))))))


;; problem 5. divisible number
;;-----------------------------
; 16 9 5 7 11 13 17 19
(define ans5
  (apply * '(16 9 5 7 11 13 17 19)))

;; problem 6. sum of square, square of sum
;;----------------------------------------
(define ans6
  (- (** (apply + (... 1 100)) 2)
     (apply + (map (lambda (x) (** x 2)) (... 1 100)))))

;; problem 7. 
;;-----------
(define ans7
  (nth 10001 (eratosthenes (.. 2 200000))))


;; problem 8. product of five consequitive digits
;;------------------------------------------------
(define ans8
  (let* ((n 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)
         (lst (number->list n))
         (t1 0)
         (maxnum 0))
    (dotimes (i (- (length lst) 10))
             (set! t1 (apply * (subseq lst i (+ i 5))))
             (if (> t1 maxnum)
                 (set! maxnum t1)))
    maxnum))

;; problem 9. find pythagorean triplet
;;------------------------------------
; 0 < a < b < c < 1000
; a + b + c = 1000
; a^2 + b^2 = c^2
; a^2 + b^2 = (1000 - a - b)^2
; 2000(a + b) - 2ab = 1000 * 1000
; 1000(a + b) - ab = 500000
(define ans9
  (let ((nums (.. 1 1000))
        (pa 0)
        (pb 0)
        (b 0)
        (anslst '())
        (ok? #f))
    (until (or ok? (null? nums))
           (set! b (car nums))
           (dolist (a (cdr nums))
                   (if (= (- (* (+ a b) 1000) (* a b)) 500000)
                       (begin (set! pa a)
                              (set! pb b)
                              (set! ok? #t))))
           (set! nums (cdr nums)))
    (set! anslst (list pa pb (- 1000 (+ pa pb))))
    (apply * anslst)))

;; problem 10. sum of primes under 2,000,000
;;------------------------------------------
;; it takes a little minuts
(define ans10
  (let ((searchlst (list* 2 3 (collection-if 
                               (lambda (x) 
                                 (let ((x (modulo x 6)))
                                   (if (or (= x 1) (= x 5))
                                       #t
                                       #f)))
                               (... 5 2000000)))))
    (apply + (collection-if almostprime? searchlst))))


