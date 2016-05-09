(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define list1 '(this is a simple list of atoms))
(define list2 '(this (is a list of) (lists) (not simple)))

(atom? 'hello)
(atom? list1)


; list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(lat? list1)
(lat? list2)

; atom in list of atoms?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(member? 'simple list1)
(member? 'ham list1)


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(rember 'word '(some word))
(rember 'word '(some word word))
(rember 'word '(some words))


(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))


(define fruits '((apple peach pear) (orange plum cherry) (banana bloobs mango)))
(define things '((Sasha says she hates) (likes (to eat)) (apples)))

(firsts fruits)
(firsts things)


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))))

(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'kinda 'is '(this is hard))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(insertL 'fucking 'fudge '(ice cream with fudge for dessert))
(insertL 'shit 'is '(this is hard))


(define subst
  (lambda (new old lat)
    (cond
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'great 'bad '(this feels bad))
(subst 'breathe 'sneeze '(dont forget to sneeze deeply))

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((or (eq? (car lat) old1) (eq? (car lat) old2)) (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))))

(subst2 'vanilla 'chocolate 'banana '(banannnna ice cream with chocolate))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((eq? (car lat) a) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat)))))))))

(multirember 'wooo '(wooo where am i in the wooo planet earth wooo))
