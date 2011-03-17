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

; ~ power
(define (** e1 e2)
  (if (= e2 1)
      e1
      (let ((tmp (** e1 (floor (/ e2 2)))))
        (if (= (mod e2 2) 0)
            (* tmp tmp)
            (* tmp tmp e1)))))

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
  (let ((top 0)
        (bottom 0)
        (digit 1)
        (ok? #t))
    (while (>= x (* 10 digit))
           (set! digit (* digit 10)))
    (while (and ok? (>= x 10))
           (set! top (floor (/ x digit)))
           (set! bottom (mod x 10))
           (if (!= top bottom)
               (set! ok? #f))
           (set! x (floor (/ (- x (* top digit)) 10)))
           (set! digit (floor (/ digit 100))))
    ok?))


           
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


