(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define list1 '(this is a simple list of atoms))
(define list2 '(this (is a list of) (lists) (not simple)))

(write "atom?")
(atom? 'hello)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(display "list of atoms")
(lat? list1)
(lat? list2)


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a) (member? a (cdr lat)))))))

(write "member?")
(member? 'simple list1)
(member? 'ham list1)


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(display "remove member from list of atoms")
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

(display "pick first word from all lats")
(firsts fruits)
(firsts things)

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
        ((eq? (car lat) old) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat)))))))))

(display "insert the new word to the right of the old word")
(insertR 'topping 'fudge '(ice cream with fudge for dessert))
(insertR 'kinda 'is '(this is hard))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((eq? (car lat) old) (cons new lat))
        (else (cons (car lat) (insertL new old (cdr lat)))))))))

(display "insert the new word to the left of the old word")
(insertL 'fucking 'fudge '(ice cream with fudge for dessert))
(insertL 'shit 'is '(this is hard))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     ((eq? (car lat) old) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(display "substitute new word for the old")
(subst 'great 'bad '(this feels bad))
(subst 'breathe 'sneeze '(dont forget to sneeze deeply))

(define subst2
  (lambda (new old1 old2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
        ((or (eq? (car lat) old1) (eq? (car lat) old2)) (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))))

(display "substitute the first occurance of old1 or old1 with the new word")
(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate))
(subst2 'vanilla 'chocolate 'banana '(bananarama ice cream with chocolate))

(define multirember
  (lambda (a lat)
    (cond
    ((null? lat) (quote()))
    (else (cond
           ((eq? (car lat) a) (multirember a (cdr lat)))
           (else (cons (car lat) (multirember a (cdr lat)))))))))

(display "remove all occurances of the word")
(multirember 'wooo '(wooo where am i in the wooo planet earth wooo))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons (car lat)
          (cons new (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
              (multiinsertR new old (cdr lat)))))))))

(display "insert new word to the right of all occurances of the old word")
(multiinsertR 'funny 'a '(There is a place and a time for a thing that is a different thing))


(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old)
        (cons new
              (cons old (multiinsertL new old (cdr lat)))))
       (else (cons (car lat)
                   (multiinsertL new old (cdr lat)))))))))

(display "insert new word to the left of all occuracnes of old word")
(multiinsertL 'loony 'a '(A place where a dog lives in a house.))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote()))
     (else (cond
            ((eq? (car lat) old)
             (cons new (multisubst new old (cdr lat))))
            (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(display "substitute all occurances of old word with the new word")
(multisubst 'cute 'loony '(loony the loony dog lives in a loony house in a loony place.))


(define add1
  (lambda (n)
    (+ n 1 )))
(add1 67)

(define sub1
  (lambda (n)
    (- n 1)))
(sub1 67)

(define plus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (plus n (sub1 m)))))))

(plus 5 2)
(+ 5 2)

(define -
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))


(- 5 2)
