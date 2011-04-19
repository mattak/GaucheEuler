#!/usr/local/bin/gosh

;; problem 21. amicable numbers
;;--------------------------------
(define ans21
  (let ((nums (... 2 10000))
        (ami 0)
        (pairs '()))
    (until (null? nums)
           (set! ami (amicable (car nums)))
           (if (and (!= ami (car nums))
                    (= (car nums) (amicable ami)))
               (push! pairs (car nums)))
           (set! nums (cdr nums)))
    (apply + pairs)))

;; problem 22. alphabet scores
;;--------------------------------
(define ans22
  (let* ((text (with-input-from-file "names.txt"
                                (lambda () (read-line))))
         (words (sort (map (lambda (x) 
                             (substring x 1 (-- (string-length x))))
                           (string-split text ","))))
         (dict (map (lambda (str)
                      (apply +
                             (map (lambda (ch)
                                    (- (char->integer ch) 64))
                                  (string->list str))))
                    words))
         (i 1)
         (sums 0))
    (apply + (map * dict (... 1 (length dict))))))


;; problem 23. abundant number
;;--------------------------------

(define ans23
  (let* ((abundants
          (collection-if (lambda (x) (if (> (amicable x) x) #t #f)) (... 1 28123)))
         (abundants-arr (list->u32vector (sort abundants)))
         (lst abundants)
         (slst abundants)
         (tmp 1)
         (nums '())
         (finded #f)
         (continue1 #t)
         (continue2 #t))
    (dolist (a (... 1 28123))
            (set! lst abundants)
            (set! tmp (- a (car lst)))
            (set! continue1 #t)
            (set! finded #f)
            (while (and continue1 (not (null? lst)))
                   (set! slst abundants)
                   (set! tmp (- a (car lst)))
                   (if (<= tmp 0)
                       (set! continue1 #f)
                       (set! finded
                             (binary-search abundants-arr tmp u32vector-ref u32vector-length (lambda (x y) (- x y)))))
                   (if finded (set! continue1 #f))
                   ;(while (and continue2 (not (null? slst)))
                   ;       (cond ((= (car slst) tmp)
                   ;              (set! continue1 #f)
                   ;              (set! continue2 #f)
                   ;              (set! finded #t))
                   ;             ((> (car slst) tmp)
                   ;              (set! continue2 #f))
                   ;             (#t #f))
                   ;       (set! slst (cdr slst)))
                   (set! lst (cdr lst)))
            (if (not finded) (push! nums a)))
    (apply + nums)))


;; problem 24. permitaion number
;;--------------------------------
(define ans24
  (list->number (nth-perm (... 0 9) (-- 1000000))))

;; problem 25. fibonatti sequence
;;--------------------------------------
(define ans25
  (let ((i 1))
    (while (< (length (number->list (fib i))) 1000)
           (set! i (++ i)))
    i))


;; problem 26. 
