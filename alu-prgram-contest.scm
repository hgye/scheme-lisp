;; Problem 18: reg



;; regular packets
 
;; To test a network, packets: 1, 2, 3, ..., n are sequentially transmitted to the destination three times in a row. A packet is called to be regular  if there exists another packet which reaches the destination earlier in all three trials.  Having the results of the three trials, find the sum of all regular  packets.
;; Input: (standard input)
;; +Line 1 contains a number n, 1 ≦ n  ≦ 100000, where n denotes a number of packets.
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
     ((null? l1) '())
     ((member? (car l1) l2) (cons (car l1)
                                  (intersection (cdr l1) l2)))
     (else
      (intersection (cdr l1) l2)))))

(define union
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     ((not (member? (car l1) l2)) (cons (car l1)
                                        (union (cdr l1) l2)))
     (else
      (union (cdr l1) l2)))))

;(intersection '(1 2 3) '(1 5 4))

(define l-after-a
  (lambda (a l)
    (cond
     ((null? l) '())
     ((eq? (car l) a) (cdr l))
     (else
      (l-after-a a (cdr l))))))

;; (l-after-a 'a '(b c a b c))

(define sumofl
  (lambda (l)
    (cond
     ((null? (car l)) 0)
     ((null? (cdr l)) (car l))
     (else (+ (car l) (sumofl (cdr l)))))))

(define lol-intersect-a
  (lambda (a l)
    (cond
     ((null? l) '())
     ((null? (cdr l)) (l-after-a a (car l)))
     (else
      (intersection (l-after-a a (car l))
                    (lol-intersect-a a (cdr l)))))))

;(lol-intersect-a '1 '((1 2 3) ( 2 4 1 2 5) (1 2 7 6)))

(define l1-intersect-rest
  (lambda (lat lol)
    (cond
     ((null? lat) '())
     ((null? (cdr lat)) (lol-intersect-a (car lat) lol))
     ((null? (lol-intersect-a (car lat) lol))
      (l1-intersect-rest (cdr lat) lol))
     (else
      (union (lol-intersect-a (car lat) lol)
             (l1-intersect-rest (cdr lat) lol))))))

;;(lat-intersect-rest '(4 3 2 5 1) '((4 2 5 1 3) (5 1 2 4 3)))



(define find-reg-packet
  (lambda (nb-packet seql)
    ;;parameter check
    (sumofl (l1-intersect-rest (car seql) seql))))

(find-reg-packet '5 '((4 3 2 5 1)
                      (4 2 5 1 3)
                      (5 1 2 4 3)))


(find-reg-packet '6 '((4 3 2 6 5 1)
                      (4 2 5 6 1 3)
                      (5 1 2 6 4 3)))

(find-reg-packet '6 '((4 3 2 6 5 1)
                      (4 2 5 6 1 3)
                      (5 1 6 2 4 3)))

;; version two, use a natural recursive way
(define intersect-lol
  (lambda (lat lol)
    (cond
     ((null? lat) '())
     ((null? (cdr lat)) lat)
     (else
      (letrec ((func (lambda (a)
                    (lambda (intersection-internal)
                      (intersection (l-after-a a (car lol))
                                    (intersect-internal a (cdr lol)))))))
            (union (func (car lat)) (intersect-lol (cdr lat) lol)))
      ))))

(intersect-lol '(4 3 2 5 1) '((4 3 2 5 1)
                              (4 2 5 1 3)
                              (5 1 2 4 3)))

(define find-reg-packet-v2
  (lambda (seqlol)
    (cond
     ((null? seqlol) '())
     ((list? (car seqlol))
      (union (intersect-internal (car seqlol) seqlol) (find-seq-packet-v2 (cdr seqlol))))
     (else
      ;;      (find-reg-packet-v2 (cdr seqlol))))))
      '()))))

(find-reg-packet-v2 '((4 3 2 5 1)
                      (4 2 5 1 3)
                      (5 1 2 4 3)))

(find-reg-packet-v2 '((4 3 2 6 5 1)
                      (4 2 5 6 1 3)
                      (5 1 2 6 4 3)))

(find-reg-packet-v2 '((4 3 2 6 5 1)
                      (4 2 5 6 1 3)
                      (5 1 6 2 4 3)))

;; use map function
(define intersection-lol
  (lambda (lol)
    (cond
     ((null? lol) '())
     ((null? (cdr lol)) (car lol))
     (else
      (intersection (car lol) (intersection-lol (cdr lol)))))))

(define union-lol
  (lambda (lol)
    (cond
     ((null? lol) '())
     (else
      (union (car lol) (union-lol (cdr lol)))))))

(define find-reg-packet-map-v3
  (lambda (seqlol)
        ;;    (map (lambda (a) (lol-intersect-a a seqlol)) (car seqlol))))
    (union-lol
     (map (lambda (a)
            (intersection-lol
             (map (lambda (l)
                       (l-after-a a l)) seqlol)
                )) (car seqlol)))))

(find-reg-packet-map-v3 '((1 2 3 4 5)
                          (1 2 3 4 5)
                          (1 2 3 4 5)))

(find-reg-packet-map-v3 '((4 3 2 6 5 1)
                          (4 2 5 6 1 3)
                          (5 1 6 2 4 3)))