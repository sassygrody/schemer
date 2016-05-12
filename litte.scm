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
(multirember 'wooo '((wooo where ami) in the wooo (planet wooo) earth))

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

(define minus
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (minus n (sub1 m)))))))


(minus 5 2)
(- 5 2)


(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else
      (+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4))


(define multiply
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (+ n (multiply n (sub1 m)))))))

(multiply 3 5)

(define tupplus
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)))
     (quote ())
     (else
      (cons (plus (car tup1) (car tup2))
            (tupplus (cdr tup1) (cdr tup2)))))))


(define tupplus
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons ( + (car tup1) (car tup2))
            (tupplus (cdr tup1) (cdr tup2)))))))

(tupplus '(2 3 1) '(2 3 9))
(tupplus '(1 5) '(5 4 9 1))
(tupplus '() '())

(define greaterThan
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (greaterThan (sub1 n) (sub1 m))))))

(greaterThan '5 '90)
(greaterThan '80 '2)
(greaterThan '3 '3)
(> '500 '90)

(define lessThan
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (lessThan (sub1 n) (sub1 m))))))

(lessThan '5 '90)
(lessThan '800 '10)
(lessThan '8 '8)


(define equalTo
  (lambda (n m)
    (cond
     ((zero? m) (zero? n))
     ((zero? n) #f)
     (else (equalTo (sub1 n) (sub1 m))))))

(equalTo '3 '3)
(equalTo '3 '8)

(define equalToTo
  (lambda (n m)
    (cond
     ((> n m) #f)
     ((< n m) #f)
     (else #t))))

(equalToTo '3 '3)
(equalToTo '3 '8)


(define powerOf
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (multiply n (powerOf n (sub1 m)))))))

(powerOf 1 1)
(powerOf 2 3)
(powerOf 5 3)

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (length (cdr lat)))))))

(length '(this is a lat))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick '4 '(Pick the number four please))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick '4 '(Remove the number four from the list please))

(define noNums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((number? (car lat)) (noNums (cdr lat)))
            (else (cons (car lat) (noNums (cdr lat)))))))))

(noNums '(5 there are 3 numbers in this 9 list.))

(define allNums
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((number? (car lat))
             (cons (car lat)
                   (allNums (cdr lat))))
            (else (allNums (cdr lat))))))))

(allNums '(4 There are 4 words 8))

(define oddsList
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
            ((odd? (car lat))
             (cons (car lat) (oddsList (cdr lat))))
            (else
             (oddsList (cdr lat))))))))

(oddsList '(1 2 3 4 5))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a1))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

(eqan? 'atom 'atom)
(eqan? 'atom 'atomm)
(eqan? 1 1)

(eq? 'atom 'atom)
(= 4 4)
(= '4 '4)

(define occurs
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? (car lat) a)
        (add1 (occurs a (cdr lat))))
       (else (occurs a (cdr lat))))))))

(occurs 'times '(How many times does times appear?))

(define onelongest?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(define onelong?
  (lambda n
    (cond
     (= n 1))))

(define one?
  (lambda n
    (= n 1)))

(one? '1)
(one? '3)

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick '3 '(List of three things))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else (cons (car l)
                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))
(multirember 'cup '((coffee) cup ((tea) cup) (and (hick)) cup))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else
      (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))


(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l))
         (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else
      ( + (occur* a (car l)) (occur* a (cdr l)))))))

;;; what??
(occur* 'banana '(banana (sundays are ((full) of ) ((banana)s))))


(define subst*
  (lambda (new old l)
    (cond
     ((null? l)
      (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (subst* new old (cdr l))))
       (else
        (cons (car l) (subst* new old (cdr l))))))
     (else
      (cons (subst* new old (car l)) (subst* new old (cdr l)))))))


(subst* 'orange 'banana '((banana) split (banana ice) (cream (banana)) sherbert (banana) (bread) (banana brandy)))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old (cdr l)))))
       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'happy 'puppies '((In a world) (full) of puppies, (there are (puppies) (being) (puppies))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) (quote ()))
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old (cdr l)))))
       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'puppies 'happy '((In a world) (full) of happy, (there are (happy) (being) (happy))))


(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a) (member* a (cdr l))))
     (else
      (or (member* a (car l)) (member* a (cdr l)))))))

(member? 'is '(This (is) a member?))
(member* 'is '(This (is) a member?))
(member* 'is '(This (issss) a member?))


(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else
      (leftmost (cdr l))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((anf (null? l1) (atom? (car l2))) #f)
     ((null? l1) #f)
     ((and (atom? (car l1)) (null? l1)) #f)
     ((and (atom? (car l1)) (atom? (car l2))
           (and (eqan? )))))))


(eqlist? '(this is a list) '(this is a list))
(eqlist? '((this) is a (list)) '((this) is a (list)))
(eqlist? '(this (is a list)) '(THINGS (THINGS (THINGS))))
