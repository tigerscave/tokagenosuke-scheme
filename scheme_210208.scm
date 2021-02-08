(print '[atom?])
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(print (atom? 'akane))

;--------------------------
(print '[lat?])
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(print (lat? '(a b c)))

;--------------------------
(print '[member?])
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))
(print (member? 'd '(a b c d e)))

;--------------------------
(print '[rember?])
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (rember a (cdr lat)))
      (else (cons (car lat) (rember a (cdr lat)))))))
(print (rember 'c '(a b c d e)))

;--------------------------
(print '[firsts])
(define firsts
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car (car lat)) (firsts (cdr lat)))))))
(print (firsts '((a b) (b c) (c d))))

;--------------------------
(print '[insertR])
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
(print (insertR 'new 'old '(a a a old a a a)))

;--------------------------
(print '[insertL])
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
(print (insertL 'new 'old '(a a a old a a a)))

;--------------------------
(print '[subst])
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
(print (subst 'new 'old '(a a a old a a a)))

;--------------------------
(print '[multirember])
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
(print (multirember 'a '(a b c a b c a b c)))

;--------------------------
(print '[multiinsertR])
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
(print (multiinsertR 'new 'old '(a a old old a a old a a a)))


;--------------------------
(print '[multiinsertL])
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
(print (multiinsertL 'new 'old '(a a old old a a old a a a)))

;--------------------------
(print '[multisubst])
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
(print (multisubst 'new 'old '(a a old old a a old a a a)))


;--------------------------
(print '[add1])
(define add1
  (lambda (n)
    (+ n 1)))

;--------------------------
(print '[sub1])
(define sub1
  (lambda (n)
    (- n 1)))

;--------------------------
(print '[plus])
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))
(print (o+ 1 2))


;--------------------------
(print '[minus])
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))
(print (o- 32 5))

;--------------------------
(print '[addtup])
(define addtup
  (lambda (lat)
    (cond
      ((null? lat) 0)
      ((o+ (car lat) (addtup (cdr lat)))))))
(print (addtup '(1 2 3 4 5)))

;--------------------------
(print '[x])
(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))
(print (x 21 3))

;--------------------------
(print '[tup+])
(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(print (tup+ '(1 2 3) '(1 2)))

;--------------------------
(print '[>])
(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))
(print (> 1 2))


;--------------------------
(print '[<])
(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))
(print (< 1 2))

;--------------------------
(print '[=])
(define =
  (lambda (n m)
    (cond
      ((> n m) #f)
      ((< n m) #f)
      (else #t))))
(print (= 1 2))


;--------------------------
(print '[expt])
(define expt
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (expt n (sub1 m)))))))
(print (expt 2 3))


;--------------------------
(print '[quotient])
(define quotient
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quotient (o- n m) m))))))
(print (quotient 10 3))

;--------------------------
(print '[length])
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
(print (length '(1 2 3 4 5)))

;--------------------------
(print '[pick])
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(print (pick '3 '(1 2 3 4 5)))

;--------------------------
(print '[rempick])
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(print (rempick '3 '(1 2 3 4 5)))


;--------------------------
(print '[no-nums])
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
(print (no-nums '(1 a 2 b 3 c 4 d 5 e)))

;--------------------------
(print '[all-nums])
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))
(print (all-nums '(1 a 2 b 3 c 4 d 5 e)))

;--------------------------
(print '[eqan?])
(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))
(print (eqan? '1 '1))

;--------------------------
(print '[occur])
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))
(print (occur 'a '(a b c a b c)))

;--------------------------
(print '[one?])
(define one?
  (lambda (n)
    (cond
      ((= n 1) #t)
      (else #f))))
(print (one? 1))
(print (one? 0))

;--------------------------
(print '[rempick2])
(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))
(print (rempick2 3 '(1 2 3 4 5)))
