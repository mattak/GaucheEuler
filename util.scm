#!/usr/local/bin/gosh
; here is my document scripting style
; '*' is title
; '~' is description 
; '>' is usage example
; '->' is execution result

; ~ for loop macro 
; > (for i 0 <= 10 (print i))
(define-macro for 
  (lambda (v start test stop . body)
    (let ((gstop (gensym)))
      `(do ((,v ,start (+ ,v 1))
            (,gstop ,stop))
           ((not (,test ,v ,gstop)))
         ,@body))))

; ~ not equal
(define (!= e1 e2)
  (not (= e1 e2)))

; ~ generate integer range
; > (... 1 10)
; ->  (1 2 3 4 5 6 7 8 9 10)
(define (... start end)
  (let ((n '()))
    (for i start <= end
         (push! n i))
    (reverse n)))

; ~ generagte integer range from "start" to before "end"
; > (.. 1 10)
; -> (1 2 3 4 5 6 7 8 9)
(define (.. start end)
  (... start (- end 1)))


; ~ remove if arglst has the element which was not pass testfn.
; > (remove-if odd? '(1 2 3 4 5 6))
; -> (2 4 6)
(define (remove-if testfn arglst :optional (acc '()))
  (cond ((null? arglst) (reverse acc))
        ((testfn (car arglst))
         (remove-if testfn (cdr arglst) acc))
        (else 
         (remove-if testfn (cdr arglst) (cons (car arglst) acc)))))      

; ~ collection if arglst has the element which pass testfn
; > (collection-if even? '(1 2 3 4 5 6))
; -> (2 4 6)
(define (collection-if testfn arglst :optional (acc '()))
  (remove-if (lambda (x) (not (testfn x))) arglst acc))

; ~ nth in element
(define (nth n lst)
  (let loop ((ln n) (llst lst))
    (if (<= ln 1)
        (car llst)
        (loop (- ln 1) (cdr llst)))))

(define (randomint start end)
  (+ start (modulo (sys-random) (++ (- end start)))))

; ~ power
; optimizing thanks for python
(define (bits integer)
  (let ((result 0))
    (until (= integer 0)
           (set! integer (ash integer -1))
           (set! result (++ result)))
    result))

(define (** x n :optional m)
  (let ((iteration (bits n))
        (result 1)
        (firstModulus 0))
    (if (undefined? m)
        (while (>= iteration 0)
               (set! result (* result result))
               (if (= (logand (ash n (- iteration)) 1) 1)
                   (set! result (* result x)))
               (set! iteration (-- iteration)))
        ; modulo
        (begin (set! firstModulus (modulo x m))
               (while (>= iteration 0)
                      (set! result (modulo (* result result) m))
                      (if (= (logand (ash n (- iteration)) 1) 1)
                          (set! result (modulo (* result firstModulus) m)))
                      (set! iteration (-- iteration)))))
    result))

; ~ factorial
(define (! e)
  (if (= e 0)
      1
      (let ((n 1))
        (while (> e 0)
               (set! n (* n e))
               (set! e (- e 1)))
        n)))

; ~ plus one
(define (++ n)
  (+ n 1))

; ~ minus one
(define (-- n)
  (- n 1))

; ~ prime numbers
; > (primes 12)
; -> (2 2 3)
(define (primes n)
  (let ((e n)
        (x 2)
        (lst '()))
    (while (and (< 1 e) (<= x n))
           (while (= (mod e x) 0)
                  (set! e (/ e x))
                  (set! lst (cons x lst)))
           (set! x (+ x 1)))
    (reverse! lst)))


; ~ is palindromic number?
; > (collection-if palindromic? (.. 100 300))
; -> (101 111 121 131 141 151 161 171 181 191 202 212 222 232 242 252 262 272 282 292)
(define (palindromic? x)
  (let ((lst '()))
    (while (> x 0)
           (push! lst (mod x 10))
           (set! x (floor (/ x 10))))
    (equal? lst (reverse lst))))

           
(define (mapeach fn lst1 lst2)
  (let ((retlst '()))
    (dolist (a1 lst1)
            (dolist (a2 lst2)
                    (push! retlst (fn a1 a2))))
    (reverse! retlst)))


(define (delete-duplicate lst)
  (let ((retlst '())
        (tmplst '()))
    (dolist (a lst)
            (set! tmplst retlst)
            (while (and (not (null? tmplst))
                        (not (eq? a (car tmplst))))
                   (set! tmplst (cdr tmplst)))
            (if (null? tmplst)
                (push! retlst a)))
    (reverse! retlst)))

(define (eratosthenes nlst)
  (let loop ((primes '()) (lst nlst))
    (if (null? lst)
        (reverse! primes)
        (loop (cons (car lst) primes)
              (remove-if (lambda (x) (= (mod x (car lst)) 0))
                         (cdr lst))))))

(define (number->list n)
  (let ((lst '()))
    (while (> n 0)
           (push! lst (mod n 10))
           (set! n (floor (/ n 10))))
    lst))

(define (list->number lst)
  (let ((n 0))
    (until (null? lst)
            (set! n (+ (* n 10) (car lst)))
            (set! lst (cdr lst)))
    n))

(define (subseq lst n1 :optional n2)
  (if (undefined? n2)
      (list-tail lst n1)
      (let ((ll (list-tail lst n1))
            (llret '()))
        (dotimes (i (- n2 n1))
                 (push! llret (car ll))
                 (set! ll (cdr ll)))
        (reverse! llret))))

; fermertest
(define (mayprime? q :optional (k 100))
  (if (= q 2)
      #t
      (let ((ok? #t)
            (x q)
            (y 0)
            (t 0)
            (i 0)
            (nums (... 2 k)))
        (while (and ok? (not (null? nums)))
               (set! x q)
               (set! i (car nums))
               (set! y i)
               (set! nums (cdr nums))
               (until (= y 0)
                      (set! t x)
                      (set! x y)
                      (set! y (mod t y)))
               (if (and (= x 1)
                        (!= (mod (** i (-- q)) q) 1))
                   (set! ok? #f)))
        ok?)))


;miller rabin test
(define (almostprime? q :optional (k 50))
  (cond ((or (= q 2) (= q 3) (= q 5)) #t)
        ((or (< q 2) 
             (= (logand q 1) 0)) #f)
        (#t (let ((d (ash (- q 1) -1))
                  (a 0)
                  (y 0)
                  (t 0)
                  (i 0)
                  (ok? #t))
              (while (= (logand d 1) 0)
                     (set! d (ash d -1)))
              (while (and ok? (< i k))
                     (set! i (++ i))
                     (set! a (randomint 1 (-- q)))
                     (set! t d)
                     (set! y (** a t q))
                     ; 0 to s-1
                     ;(format #t "i:~A a:~A t:~A y:~A ok:~A~%" i a t y ok?)
                     (while (and (!= t (-- q)) (!= y 1) (!= y (-- q)))
                            (set! y (** y 2 q))
                            (set! t (ash t 1)))
                     (if (and (!= y (-- q)) (= (logand t 1) 0))
                         (set! ok? #f)))
              ok?))))
