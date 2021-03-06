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
      `(with-yield
        (do ((,v ,start (+ ,v 1))
             (,gstop ,stop))
            ((not (,test ,v ,gstop)))
          ,@body)))))

; ~ with yield
(define-macro (with-yield . body)
  (let ((acc (gensym)))
   `(let ((,acc '()))
      (letrec ((yield (lambda (x)
                        (push! ,acc x))))
        ,@body
        (reverse! ,acc)))))

(define-macro (with-index v . body)
  `(let ((,v 1))
     ,@body))


(define-macro (aif if-state then-state :optional else-state)
  `(let ((it ,if-state))
     (if it
         ,then-state
         ,else-state)))

(define-macro (awhen if-state . body)
  `(let ((it ,if-state))
     (when ,if-state
           ,@body)))

(define-syntax labels
  (syntax-rules ()
    [(_ ((name (var ...)
               proc-body ...) ...)
        body ...)
     (letrec
         ((name (lambda (var ...)
                  proc-body ...)) ...)
          body ...)]))

; CL
;(defmacro dolists (pairs &body body)
;  (let ((parms (mapcar (lambda (x) (declare (ignore x)) (gensym)) pairs))
;        (f (gensym)))
;    `(labels ((,f ,parms
;                  (when (or ,@parms)
;                        (let ,(mapcar (lambda (p g)
;                                        (list (car p) `(car ,g)))
;                                      pairs
;                                      parms)
;                          ,@body
;                          (,f ,@(mapcar (lambda (p) `(cdr ,p)) parms))))))
;             (,f ,@(mapcar #'second pairs)))))

(define-syntax dolists
  (syntax-rules ()
    [(_ ((var lis) ...)
        proc-body ...)
     (for-each (lambda (var ...) proc-body ...) lis ...)]))

;(define-macro (dolists pairs . body)
;  (let ((f (gensym))
;        (parms (map (lambda (x) (gensym)) pairs)))
;    `(labels ((,f ,parms
;                  (when (or ,@(map (lambda (x) (list 'not (list 'eq? x '()))) parms))
;                        (let ,(map (lambda (p g)
;                                     (list (car p) `(car ,g)))
;                                   pairs
;                                   parms)
;                          ,@body
;                          (,f ,@(map (lambda (p) `(cdr ,p)) parms))))))
;             (,f ,@(map cadr pairs)))))

;(define-syntax for
;  (syntax-rules (by)
;    [(for (i init limit by step) expr ...)
;     (do ((tlimit limit)
;          (i init (+ i step)))
;         ((> i tlimit))
;       expr ...)]
;    [(for (i init limit) expr ...)
;     (do ((tlimit limit)
;          (i init (+ i 1)))
;         ((> i tlimit))
;       expr ...)]))

(define-macro (dolist-with-index i a lst . body)
  `(let ((,i 0))
     (dolist (,a ,lst)
             ,@body
             (set! ,i (+ ,i 1)))))


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

;; list basic operation
;;-------------------------------------------

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

; ~ apply function to each list element
; > (mapeach cons '(1 2 3) '(4 5 6))
; ->((1 . 4) (1 . 5) (1 . 6) (2 . 4) (2 . 5) (2 . 6) (3 . 4) (3 . 5) (3 . 6))
(define (mapeach fn lst1 lst2)
  (let ((retlst '()))
    (dolist (a1 lst1)
            (dolist (a2 lst2)
                    (push! retlst (fn a1 a2))))
    (reverse! retlst)))

; ~ delete duplicate element in list
; > (delete-duplicate '( 1 2 3 2 3 1 4 2))
; ->(1 2 3 4)
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

; ~ subsequence of list
; > (subseq '(1 2 3 4) 2 4)
; -> (3 4)
(define (subseq lst n1 :optional n2)
  (if (undefined? n2)
      (list-tail lst n1)
      (let ((ll (list-tail lst n1))
            (llret '()))
        (dotimes (i (- n2 n1))
                 (push! llret (car ll))
                 (set! ll (cdr ll)))
        (reverse! llret))))

; ~ grouping list
; > (group '(1 2 3 4 5) 2)
; ->((1 2) (3 4) (5))
(define (group arglst n)
  (if (< n 1) (error "group length must be greater than zero!")
      (let rec ((m (length arglst)) (lst arglst) (acc '()))
        (if (<= m n) (reverse! (cons lst acc))
            (rec (- m n) (list-tail lst n) (cons (subseq lst 0 n) acc))))))

; ~ flatten list
; > (flatten '((1) (2 (3)) 4 (5 (6 7))))
; -> (1 2 3 4 5 6 7)
(define (flatten arglst)
;  (let ((acc '()))
;    (letrec ((rec (lambda (lst)
;                    (if (atom? lst) (push! acc lst)
;                        (map rec lst)))))
;      (rec arglst))
;    (reverse! acc))
  (letrec ((rec (lambda (x acc)
                  (cond ((null? x) acc)
                        ((atom? x) (cons x acc))
                        (#t (rec (car x) (rec (cdr x) acc)))))))
    (rec arglst '())))



; ~ nth in element
(define (nth n lst)
;  (let loop ((ln n) (llst lst))
;    (if (<= ln 1)
;        (car llst)
;        (loop (- ln 1) (cdr llst)))))
  (list-ref lst (- n 1)))

(define (last lst)
  (let ((tmp lst))
    (until (null? (cdr tmp))
           (set! tmp (cdr tmp)))
    (car tmp)))

; ~ refer matrix
; > (ref (1 2) '((1 2 3) (4 5 6)))
; -> 2
(define (ref idx mat)
  (if (null? idx)
      mat
      (ref (cdr idx) (nth (car idx) mat))))

; ~ memomize function
(define (memoize fn)
  (let ((cache '()))
    (lambda args
      (let ((hit (assoc args cache)))
	(if hit 
	    (cdr hit)
	    (let ((result (apply fn args)))
	      (set! cache (cons (cons args result) cache))
	      result))))))

(define (member-index e lst :optional (test eq?))
  (if (null? lst)
      (values #f '())
      (let ((a (car lst))
            (retlst #f)
            (index 0))
        (until (null? lst)
               (if (test e (car lst))
                   (begin
                     (set! retlst lst)
                     (set! lst '()))
                   (begin
                     (set! index (++ index))
                     (set! lst (cdr lst)))))
        (if (eq? #f retlst)
            (values #f '())
            (values index retlst)))))

;; algorithm
;;-----------------------------------------------
; ~ binary search
; > (binary-search (list->u32vector '(1 2 3 4)) 4 u32vector-ref u32vector-length (lambda (x y) (- x y)))
; ->3
; > (binary-search (list->u32vector '(1 2 3 4)) -1 u32vector-ref u32vector-length (lambda (x y) (- x y)))
; ->#f
;(use gauche.uvector)
(define (binary-search arr v fn-ref fn-length fn-cmp)
  (let* ((end (-- (fn-length arr)))
         (start 0)
         (half (/ (- end start) 2))
         (t 0)
         (finded #f))
    (while (and (not finded) (<= start end))
           (set! half (floor (/ (+ end start) 2)))
           (set! t (fn-cmp v (fn-ref arr half)))
           ;(print (list start end half t (fn-ref arr half)))
           (cond ((= t 0)
                  (set! finded #t)) 
                 ((> t 0)
                  (set! start (++ half)))
                 ((< t 0)
                  (set! end (-- half)))))
    (if finded
        half
        #f)))


;; mathmatics
;;-----------------------------------------------
; ~ plus one
(define (++ n)
  (+ n 1))

; ~ minus one
(define (-- n)
  (- n 1))

; ~ count bits
(define (bits integer)
  (let ((result 0))
    (until (= integer 0)
           (set! integer (ash integer -1))
           (set! result (++ result)))
    result))

; ~ power and residual powers
; optimizing thanks for python
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

; ~ generate random integer
; > (randomint 10 15)
; -> 15
(define (randomint start end)
  (+ start (modulo (sys-random) (++ (- end start)))))

; ~ convert number to list
; > (number->list 12345)
; ->(1 2 3 4 5)
(define (number->list n)
  (let ((lst '()))
    (while (> n 0)
           (push! lst (modulo n 10))
           (set! n (floor (/ n 10))))
    lst))

; ~ convert list to number
; > (list->number '(1 2 3 4 5))
; ->12345
(define (list->number lst)
  (let ((n 0))
    (until (null? lst)
            (set! n (+ (* n 10) (car lst)))
            (set! lst (cdr lst)))
    n))

; ~ is palindromic number?
; > (collection-if palindromic? (.. 100 300))
; -> (101 111 121 131 141 151 161 171 181 191 202 212 222 232 242 252 262 272 282 292)
(define (palindromic? x)
  (let ((lst '()))
    (while (> x 0)
           (push! lst (mod x 10))
           (set! x (floor (/ x 10))))
    (equal? lst (reverse lst))))

; ~ factor of number
; > (factors 12)
; -> (1 3 2 6 4 12)
(define (factors n)
  (let ((i 1)
        (ps (primes n))
        (pre 0)
        (tmp '(1)))
;    (for i 1 <= n
;         (if (= (mod n i) 0)
;             (yield i)))))
    (seq-apply 
     (lambda (x y) (mapeach * x y))
     (with-yield
      (until (null? ps)
             (if (!= pre (car ps))
                 (begin
                   (yield (reverse tmp))
                   (set! tmp '(1))))
             (set! pre (car ps))
             (push! tmp (* (car tmp) pre))
             (set! ps (cdr ps)))
      (yield (reverse tmp))))))

; ~ store result in sequence
; > (seq-apply (lambda (x y) (mapeach * x y)) '((1 2 4) (1 3 9) (1 5)))
; ->(1 5 3 15 9 45 2 10 6 30 18 90 4 20 12 60 36 180)
(define (seq-apply fn rest)
  (let ((result (car rest))
        (lst (cdr rest)))
    (until (null? lst)
           (set! result (fn result (car lst)))
           (set! lst (cdr lst)))
    result))

; ~ amicable numbers
; > (amicable (amicable 220))
; -> 220
(define (amicable n)
  (- (apply + (factors n)) n))


(define (triangle-numbers n)
  (let ((sum 0)
        (i 0))
     (for i 1 <= n
          (set! sum (+ sum i))
          (yield sum))))

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


(define (eratosthenes nlst)
  (let loop ((primes '()) (lst nlst))
    (if (null? lst)
        (reverse! primes)
        (loop (cons (car lst) primes)
              (remove-if (lambda (x) (= (mod x (car lst)) 0))
                         (cdr lst))))))

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


; ~ calculate fraction list. last list mean recurring cycle in its fraction part.
; >(fraction-recur 1 13)
; ->(0 (0 7 6 9 2 3))
; >(fraction-recur 1 6)
; ->(0 1 (6))
; >(fraction-recur 1 8)
; ->(0 1 2 5)
(define (fraction-recur n m :optional (k 100))
  (let ((i 0)
        (restlst '())
        (result '()))
    (while (and (< i k) (!= n 0))
           (if (>= n m)
               (push! result (floor->exact (/ n m)))
               (push! result 0))
           (set! n (mod n m))
           (push! restlst n)
           (set! n (* 10 n))
           (awhen (member-index (car restlst) (cdr restlst))
                  (set! result 
                        (cons (reverse (subseq result 0 (++ it))) 
                                       (list-tail result (++ it))))
                  (set! n 0))
           (set! i (+ i 1)))
    (if (< i k)
        (reverse! result)
        #f)))

; ~ max value and its index
; > (max&index 4 0 2)
; ->(4 1)
(define (max&index . args)
  (let ((i 1)
        (maxidx 1)
        (maxit (car args)))
    (dolist (a args)
            (if (> a  maxit)
	(begin (set! maxit a)
	       (set! maxidx i)))
           (inc! i))
    (list maxit maxidx)))

;; mathmatics -- comination
;;--------------------------------

; ~ permitation
(define (perm n k)
  (apply * (... (++ (- n k)) n)))

; ~ combination
(define (comb n k)
  (/ (perm n k) (! k)))

; ~ nth perm - it begin from index 0 to nPn - 1
; > (nth-perm (... 0 3) 1)
; ->(0 1 3 2)
(define (nth-perm lst n)
  (let ((i n)
        (t 0)
        (order (length lst))
        (tlst lst)
        (result '())
        (tmp 0))
    (if (or (< n 0)
            (>= n (perm order order)))
        #f
        (begin
          (while (> order 1)
                 (set! order (-- order))
                 (set! tmp (perm order order))
                 (set! t (floor (/ i tmp)))
                 (set! i (- i (* tmp t)))
                                        ; pick up 
                 (push! result (list-ref tlst t))
                 (set! tlst (append (subseq tlst 0 t) (list-tail tlst (++ t)))))
          (push! result (car tlst))
          (reverse! result)))))

(define (fib n)
  (if (<= n 2)
      1
      (let ((a1 1)
            (a2 1)
            (tmp 0)
            (t (- n 2)))
        (while (> t 0)
               (set! tmp a2)
               (set! a2 (+ a2 a1))
               (set! a1 tmp)
               (set! t (-- t)))
        a2)))

;(define (fraction-recurring n)
;  (let (())
;    ))

;; mathmatics -- set theory
;;---------------------------------
; ~ different set
; > (complement-set (... 1 9) (... 2 4))
; ->(1 5 6 7 8 9)
(define (complement-set set1 set2)
  (let ((none #t))
    (with-yield
     (dolist (a1 set1)
             (set! none #t)
             (dolist (a2 set2)
                     (if (eq? a1 a2)
                         (set! none #f)))
             (if none
                 (yield a1))))))







