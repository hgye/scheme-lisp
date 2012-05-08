(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(atom? 'abc)
(atom? '(1 2))
(atom? (car '(1 2 3)))


(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(lat? '(a b))
(lat? (list))
(lat? '(a (b c) d))
(or #f #f #f #t)

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eqan? (car lat) a)
               (member? a (cdr lat)))))))

(member? 'a '())
(member? 'a '(a b c))
(member? 'a '(b c))


(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (cons (car lat)
                 (rember a (cdr lat)))))))

(rember 'a '(a b c))
(rember 'a '(a b a c))
(rember 'a '(b c a c))
(rember 'a '( ))

(define build
  (lambda (s1 s2)
    (cond
     (else (cons s1
                 (cons s2 (quote ())))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cond
       ((or (not (list? (car l))) (null? (car l)))
        (firsts (cdr l)))
       (else (cons
              (car (car l))
              (firsts (cdr l)))))))))

(firsts '((1 2) (3 4)))
(firsts '(((1 2) 3) (4 5)))
(firsts '( ( 1 2) (( 3 4) 5)))
(firsts '(1 (2 3)))
(firsts '(() (1 2)))
(firsts '(()))
(firsts '())

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) old) (cons old (cons new (cdr lat))))
     (else (cons (car lat) (insertR new old (cdr lat)))))))

(insertR 'b 'a '(a d))
(insertR 'b 'a '(c d a))
(insertR '(a b) 'a '(c d a))
(insertR '(a b) '(a b) '(c d '(a b))) ; eq? can't used for list, only for atom
(insertR 'x '(a b) '(c d '(a b))) ; eq? can't used for list

(define insertL
    (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) old) (cons new lat))
     (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'b 'a '(a d))
(insertL 'b 'a '(c d a))
(insertL '(a b) 'a '(c d a))
(insertL '(a b) '(a b) '(c d '(a b))) ; eq? can't used for list, only for atom
(insertL 'x '(a b) '(c d '(a b))) ; eq? can't used for list

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'b 'a '(a d))
(subst 'b 'a '(c d a))
(subst '(a b) 'a '(c d a))
(subst '(a c) '(a b) '(c d '(a b))) ; eq? can't used for list, only for atom
(subst 'x '(a c) '(c d '(a b))) ; eq? can't used for list

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) lat)
     ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'x 'a1 'a2 '(a1 a2 b c))
(subst2 'x 'a1 'a2 '(a2 a1 b c))
(subst2 'x 'a1 'a2 '(e f a1 a2 b c))

(define multirmber
  (lambda (a lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) a) (multirmber a (cdr lat)))
     (else (cons (car lat) (multirmber a (cdr lat)))))))

(multirmber 'a '( a b c d))
(multirmber 'a '( a b a c a d))
(multirmber 'a '( a b a c (a) d))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'x 'a '(a c a d a))
(multiinsertR '(x1 x2) 'a '(a c a d a))
(multiinsertR 'x 'a '( c  d))
(multiinsertR 'x 'a '((a)))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(multiinsertL 'x 'a '(a c a d))
(multiinsertL '(x1 x2) 'a '(a c a d a))
(multiinsertL 'x 'a '( c  d))
(multiinsertL 'x 'a '((a)))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) lat)
     ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
     (else (cons (car lat) (multisubst new old (cdr lat)))))))

(multisubst 'x 'a '(a b c d))
(multisubst 'x 'a '(a b a c a d))
(multisubst 'x 'a '(b c d))

(define my+
  (lambda (n1 n2)
    (cond
     ((zero? n1) n2)
     (else (add1 (my+ (sub1 n1)  n2))))))

(my+ 2 3)
(my+ 0 1)
(my+ 1 2 3) ; wrong, only take two arguments, but 3

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (+ (car tup) (addtup (cdr tup)))))))

(addtup '(2 3 4 5))

(define my*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (my* n (sub1 m)))))))

(my* 1 2)
(my* 5 8)

(define tup+
  (lambda (tup1 tup2)
    (cond
;     ((and (null? tup1) (null? tup2)) '())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2) '(3 4))
(tup+ '(1 2) '(1 3 4))
(tup+ '(1 2 3) '(4 5))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2)) #f)
     (else
      (eq? a1 a2)))))

(eqan? 4 4)
(eqan? 4 'a)
(eqan? 'a 'b)

(define rember*
  (lambda (a l)
    (cond
     ((null? l) l)
     ((atom? (car l))
      (cond
       ((eqan? a (car l))
        (rember* a (cdr l)))
       (else
        (cons (car l) (rember* a (cdr l))))))
     (else
        (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'a '(a b))
(rember* 'a '((a b) a b))
(rember* 'x '(((x)) (x a) (a b) x a))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) l)
     ((atom? (car l))
      (cond
       ((eqan? old (car l))
        (cons old (cons new (cdr l))))
       (else
        (cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'x 'a '(a b))
(insertR* 'x 'a '((a) b a c))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eqan? a (car l))
        (add1 (occur* a (cdr l))))
       (else
        (occur* a(cdr l)))))
     (else
      (+ (occur* a (car l)) (occur* a (cdr l)))))))


(occur* 'a '((a) a (a b) b))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) l)
     ((atom? (car l))
      (cond
       ((eqan? old (car l))
        (cons new (subst* new old (cdr l))))
       (else
        (cons old (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'x 'a '(((a)) a (a b) b))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((or (null? l1) (null? l2))
      (cond
       ((and (null? l1) (null? l2)))
       (else #f)))
     ((atom? (car l1))
      (cond
       ((atom? (car l2))
        (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
       (else #f)))
     (else
      (cond
       ((atom? (car l2)) #f)
       (else
        (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))
       
(eqlist? '(a b) '(a b))
(eqlist? '(a) '(b))
(eqlist? '((a) b (c d)) '((a) b (c d)))
(eqlist? '((a) b (c d)) ' '((a) b (c d) e))
(eqlist? '() '())

(define equal?
  (lambda (a1 a2)
    (cond
     ((and (atom? a1) (atom? a2))
      (eqan? a1 a2))
     ((or (atom? a1) (atom? a2))
      #f)
     (eqlist? a1 a2))))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     ((member? (car lat) (cdr lat)) #t)
     (else
      (set? (cdr lat))))))

(set? '(a b))
(set? '( a b c d a ))
(set? '(a b c d 1 e 1))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) lat)
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else
      (cons (car lat) (makeset (cdr lat)))))))

(makeset '())
(makeset '(a b c d a b))
(makeset '(a b c d a e))
(makeset '(b c f x x x y y))

(define makeset-v2
  (lambda (lat)
    (cond
     ((null? lat) lat)
     (else
;      (cons (car lat) (multirmber (car lat) (makeset-v2 (cdr lat))))))))
      (cons (car lat) (makeset-v2 (multirmber (car lat) (cdr lat))))))))

(makeset-v2 '())
(makeset-v2 '(a b c d a b))
(makeset-v2 '(a b c d a e))
(makeset-v2 '(b c f x x x y x y))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     ((member? (car set1) set2) (subset? (cdr set1) set2))
     (else #f))))

(subset? '(a) '(a b))
(subset? '(c) '(a b))
(subset? '() '(a b))
(subset? '(a b) '(c d e a b))
(subset? '(a x b) '(c d e a b))


(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eqset? '(a b) '(a b))
(eqset? '(a 1 3) '(a 1 3 ))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((and (null? set1) (null? set2)) #t)
     ((or (null? set1) (null? set2)) #f)
     (else
      (or (member? (car set1) set2)
          (member? (car set2) set1))))))

(intersect? '(a b c) '(x y z a))
(intersect? '(a b c) '(x y z))
(intersect? '(a b c) '(x b y z a))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     (else
      (cond
       ((member? (car set1) set2)
        (cons (car set1) (intersect (cdr set1) set2)))
       (else (intersect (cdr set1) set2)))))))

(intersect '(a b c) '(a x y))
(intersect '(a c d) '(a c xy y))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else
      (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((a b) (a c d) (a x y)))
(intersectall '((a b) (a b c d) (a x y b)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

(define rember-f-1
  (lambda (test? a lat)
    (cond
     ((null? lat) '())
     ((test? (car lat) a) (cdr lat))
     (else (cons (car lat) (rember-f test? a (cdr lat)))))))

(rember-f eq? 'a '(a b c d))
(rember-f equal? '1 '((1) 2 3 1))

(define rember-f
  (lambda (test?)
    (lambda ( a lat)
      (cond
       ((null? lat) '())
       ((test? (car lat) a) (cdr lat))
       (else (cons (car lat) ((rember-f test?) a (cdr lat))))))))

((rember-f eq?) 'a '(a b c))
((rember-f eq?) 'a '( b c a))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) lat)
       ((test? (car lat) old) (cons new lat))
       (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

((insertL-f eq?) 'a 'b '( b c d))
((insertL-f eq?) 'a 'b '(c b d))

(define insertR-f
  (lambda (test?)
    (lambda (new old lat)
      (cond
       ((null? lat) lat)
       ((test? (car lat) old) (cons old (cons new (cdr lat))))
       (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

((insertR-f eq?) 'a 'b '(b c d))

(define insert-g
  (lambda (cons-f)
    (lambda (new old lat)
      (cond
       ((null? lat) '())
       ((eq? old (car lat)) (cons-f new old (cdr lat)))
       (else (cons (car lat) ((insert-g cons-f) new old (cdr lat))))))))
        
(define cons-ab
  (lambda (a b lat)
    (cons a (cons b lat))))
(define cons-ba
  (lambda (a b lat)
    (cons b (cons a lat))))
  
((insert-g cons-ab) 'a 'b '(b c d))
((insert-g cons-ba) 'a 'b '(b c d))
((insert-g cons-ab) 'a 'b '(c b d))
((insert-g cons-ba) 'a 'b '(c b d))

(define insertL
  (insert-g
   (lambda (new old lat)
     (cons new (cons old lat)))))

(define seqS
  (lambda ( new old lat)
    (cons new (cdr lat))))

(define subst
  (insert-g seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(yyy 'a '(a b))

(define multirmberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat)) (multirmberT test? (cdr lat)))
     (else (cons (car lat) (multirmberT test? (cdr lat)))))))

(define eq-a?
   (eq? 'a))

(define multirmberT!
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? a (car lat)) (multirmberT! a (cdr lat)
                                      (lambda (newlat seen)
                                        (col newlat (cons (car lat) seen)))))
     (else (multirmberT! a (cdr lat)
                         (lambda (newlat seen)
                           (col (cons (car lat) newlat) seen)))))))

(define a-friend?
  (lambda (x y) (null? y)))

(multirmberT! 'a '(a b c) a-friend?)
(multirmberT! 'a '(a b a c) a-friend?)
(multirmberT! 'a '(b c d) a-friend?)

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'a 'x 'y '( b c x d e y f))
(multiinsertLR 'a 'x 'x '( b c x d e y f))

(define multiinsertLRCol
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '()) 0 0)
     ((eq? oldL (car lat)) (multiinsertLRCol new oldL oldR (cdr lat)
                                             (lambda (newlat L R)
                                               (col (cons new (cons oldL newlat)) (add1 L) R))))
     ((eq? oldR (car lat)) (multiinsertLRCol new oldL oldR (cdr lat)
                                             (lambda (newlat L R)
                                               (col (cons new (cons oldR newlat) L (add1 R))))))
     (else (multiinsertLRCol new oldL oldR (cdr lat)
                             (lambda (newlat L R)
                               (col (cons (car lat) newlat) L R)))))))

(define evens-only*Col
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond
       ((even? (car l))
        (evens-only*Col (cdr l)
                       (lambda (newl p s)
                         (col (cons (car l) newl) (* p (car l)) s))))
       (else
        (evens-only*Col (cdr l)
                       (lambda (newl p s)
                         (col (cons (car l) newl) p (+ (car l) s)))))))
     (else
      (evens-only*Col (car l)
                      (lambda (al ap as)
                        (evens-only*Col (cdr l)
                                        (lambda (dl dp ds)
                                          (col (cons al dl) (* ap dp) (+ as ds))))))))))

(define the-last-friend
  (lambda (newl product sum)
    (cons product (cons sum newl))))


(evens-only*Col '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)


(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking-old
  (lambda (a loc lat)
    (cond
     ((null? lat) #f)
     ((eq? (pick loc lat) a) #t)
     (else
      (cond
       ((number? (pick loc lat))
        (cond
         ((> (pick loc lat) (length lat)) #f)
         (else (keep-look-old a (pick loc lat) lat)))))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? (pick sorn lat))
      (keep-looking a (pick sorn lat) lat))
     (else
      (eq? (pick sorn lat) a)))))

(define pick
  (lambda (loc lat)
    (cond
     ((= loc 1) (car lat))
     (else
      (pick (- loc 1) (cdr lat))))))

(pick 1 '(a b c))
(looking 'a '(3 5 4 a))

(looking 'a '(3 4 5))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(align '(a b))
(align '(a b c))
(align '((a b) c))
(align '(a (b c) d))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(revpair '(a b))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

(shuffle '(a b))
(shuffle '(a b c))
(shuffle '((a b) c))
(shuffle '(a (c d)))


; can't define in language, turning, The Halting Problem
(define will-stoping?
  (lambda (f)
    (...)))

(define eternity
  (lambda (x)
    (eternity x)))

(define turing-stop
  (lambda (x)
    (and (will-stoping? turing-stop)
         (eternity x))))


((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (addl (length (cdr l))))))))


((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 5)

(define fact-Ycombinator
  (lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1))))))))

(fact-Ycombinator 10)

(define length-YC
  (lambda (l)
    ((lambda (length-iter)
       (length-iter length-iter l))
     (lambda (lg l)
       (cond
        ((null? l) 0)
        (else
         (add1 (lg lg (cdr l)))))))))

(length-YC '(1 (2) 3))

(define y-combinator
  (lambda (f)
    ((lambda (f)
       (f f))
     (f (lambda (x) (x x))))))

(define fact
  (lambda (n)
    (y-combinator
     (lambda (x)
       ((lambda (f) (f f m))
        (x (lambda (x m)
             (if (= m 0)
                 1
                 (* (x x (- m 1)))))))))
    n))


(fact 3)
                 

