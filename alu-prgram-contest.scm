;; Problem 18: reg

;; regular packets

;; To test a network, packets: 1, 2, 3, ..., n are sequentially transmitted to the destination three times in a row. A packet is called to be regular  if there exists another packet which reaches the destination earlier in all three trials.  Having the results of the three trials, find the sum of all regular  packets.

;; Input: (standard input)

;; +Line 1 contains a number n, 1 <= n  <= 100000, where n denotes a number of packets.

;; +Lines 2-4 contain results of each trial i.e.: n space-separated permutation of 1, 2, 3, ..., n.

;; Output: (standard output)

;; Your program should print out the sum of all regular packets.


;; Example:
;; For sample input:
;; 5
;; 4 3 2 5 1
;; 4 2 5 1 3
;; 5 1 2 4 3

;; a correct output format:
;; 4

;; Hint:  Packets 1 and 3 are regular because 5 was always faster than 1, and 4 was always faster than 3. Thus 1 + 3 = 4.
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
               (member? a (cdr lat)))))))

(define intersection
  (lambda (l1 l2)
    (cond
     ((or (null? l1)  (null? l2)) '())
     (else
      (cond
       ((member? (car l1) l2) (cons (car l1)
                                     (intersection (cdr l1) l2)))
       (else (intersection (cdr l1) l2)))))))

(define l-remove-first
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cdr l)))))

(define l-after-a
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) a) (cdr lat))
     (else
      (l-after-a a (cdr lat))))))

(define intersect-after-a
  (lambda (l1 l2)
    (cond
     ((null? l1) '())
     (else
      (intersection (cdr l1) (l-after-a (car l1) l2))))))

(define find-seq-packet-Col
  (lambda (seq-l col)
    ((null? seq-l) (col '() '())
    ((null? (cdr seq-l) (col (car seq-l) (car seq-l))))
    (else
     (find-seq-packet-Col
      (cdr seq-l) (lambda (newl)
                    (col newl (car (cdr seq-l))))))))
                                 

(define find-seq-packet-Col-x
  (lambda (nb-packet seq-l col)
    (cond
     ; parameters check
     (cond
      ((or (>= 1 nb-packet) (<= 100000 nb-packet)) '())
      ((not (list? (car seq-l))) '())
;      ((not (= (length (car seq-l) nb-packet))) '())
;      ((null? (car seq-l)) (col (car seq-l)))
      ((null? (cdr (car seq-l)) (find-seq-packet-Col nb-packet seq-l
      (else
       (intersection (cdr (car seq-l)) (car seq-l)
       
                                

 