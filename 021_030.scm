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

