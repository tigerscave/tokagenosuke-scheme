(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;(print (atom? 'a))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

;(print (lat? '(a b c)))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

;(print (member? 'f '(a k a n e)))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat)  (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

;(print (rember 'a '(a k a n e)))

(define firsts
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((cons (car (car lat)) (firsts (cdr lat)))))))

;(print (firsts '((a b) (b c) (c d))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

;(print (insertR 'new 'old '(a b c old d e f)))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat)  (quote ()))
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

;(print (insertL 'new 'old '(a b c old d e f)))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

;(print (subst 'new 'old '(a b c old d e f)))

(define multirember
  (lambda (old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (multirember old (cdr lat)))
      (else (cons (car lat) (multirember old (cdr lat)))))))

;(print (multirember 'old '(a b c old old d old e f old g old h)))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat)  (quote ()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

;(print (multiinsertR 'new 'old '(a b c old d e old f old g old h)))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat)  (quote ()))
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

;(print (multiinsertL 'new 'old '(a b c old d e old f old g old h)))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
;(print (multisubst 'new 'old '(a b c old d e old f g old h)))

(define add1
  (lambda (x)
    (+ x 1)))

;(print (add 1))

(define sub1
  (lambda (x)
    (- x 1)))

;(print (sub1 3))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
;(print (o+ 5 2))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
;(print (o- 10 4))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      ((o+ (car tup) (addtup (cdr tup)))))))
;(print (addtup '(1 2 3 4 5 6)))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))
;(print (x 4 3))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

;(print (tup+ '(1 2 3) '(1 2 3 8 10)))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

;(print (> 2 3))
;(print (< 5 2))

(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))

;(print (= 1 1))
;(print (= 1 2))
;(print (= 2 1))

(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (expt n (sub1 m)))))))
;(print (expt 2 5))

(define quotient
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quotient (o- n m) m))))))
;(print (quotient 30 3))


(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
;(print (length '(1 2 3 4 5)))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
;(print (pick 3 '(1 2 3 4 5)))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;(print (rempick 3 '(1 2 3 4 5)))


(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)  (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
;(print (no-nums '(a 1 b 2 c 3 d 4)))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))
;(print (all-nums '(1 a 2 b 3 c 4 d)))

(define equan?
  (lambda (n m)
    (cond
      ((and (number? n) (number? m)) (= n m))
      ((or (number? n) (number? m)) #f)
      ((eq? n m) #t)
      (else #f))))
;(print (equan? '1 '1))
;(print (equan? '1 'a))
;(print (equan? 'a 'a))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))
;(print (occur 'a '(a k a n e)))


(define one?
  (lambda (x)
    (= x 1)))
;(print (one? 12))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
;(print (rempick 3 '(1 2 3 4 5)))

(define rember_star
  (lambda (a lat)
    (cond
      ((null? lat)  (quote ()))
      ((atom? (car lat))
      (cond
        ((eq? (car lat) a) (rember_star a (cdr lat)))
        (else (cons (car lat) (rember_star a (cdr lat))))))
      (else (cons (rember_star a (car lat)) (rember_star a (cdr lat)))))))

(print (rember_star 'a '(a b (c a s (a d)))))
