#lang racket
; S-expression = {list, atom}
; {car, cdr} for non-empty list, (cons S '(L)) = '(S L)
; null? for list only / practice #f for everything except '()
; atom? for any S-expression
; eq? for two non-numeric  atoms / practice two lists available
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; lat? #t when each s-expression in list are atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
; (define ...) give it a name
; (lambda ...) create a function
; (cond ...) ask questions  (question value)
; or ask two question
; member?
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
; #1-always ask null? as the first in question

; rember to remove item once(or zero) from a list
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))
(define pppp 'pppp)
(define lati '(ppp pppp pp))
; #2-use cons to build lists
; firsts takes empty-list or list that contains non-empty list.
; builds another list composed of first S-expression of each internal list.
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))
; #3-when building a list, describe the first typical element, and then
; cons it onto the natural recursion.
; insertR new old lat. new insert to the right of the first old in lat.
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (cdr lat))))
              (else (cons (car lat) (insertR new old (cdr lat)))))))))
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new lat))
              (else (cons (car lat) (insertL new old (cdr lat)))))))))

; subst new old lat. replace
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons new (cdr lat)))
              (else (cons (car lat) (subst new old (cdr lat)))))))))
; subst2 new o1 o2 lat
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
              (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))
; multirember
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? a (car lat)) (multirember a (cdr lat)))
              (else (cons (car lat) (multirember a (cdr lat)))))))))
; multiinsertR
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
              (else (cons (car lat) (multiinsertR new old (cdr lat)))))))))
; multiinsertL
(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
              (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))
; #-4 always change at least one argument while recurring. close to termination.
; when using cdr, test termination with null?
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else (cond
              ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
              (else (cons (car lat) (multisubst new old (cdr lat)))))))))

; consider non-negative integers
; add1
(define add1
  (lambda (n)
    (+ n 1)))
; sub1
(define sub1
  (lambda (n)
    (- n 1)))
; zero? seen as null?
(define add
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (add (add1 n1) (sub1 n2))))))
(define sub
  (lambda (n1 n2)
    (cond
      ((zero? n2) n1)
      (else (sub (sub1 n1) (sub1 n2))))))
; tup a list of numbers
(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (add (car tup) (addtup (cdr tup)))))))
; #1'-when recur list, ask null? or zero? first.
(define mul
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (add n (mul n (sub1 m)))))))
; #4'-cdr sub1 test null? zero?
; #5-when building a value with +, always use 0 for the value of the terminating line.
; *, use 1.
; cons, use ().
; (tup+ tup1 tup2) build tup by adding.
(define tup+
  (lambda (tup1 tup2)
    (cond
      ;((and (null? tup1) (null? tup2)) '())
      ((null? tup1) tup2) ;order doesn't matter
      ((null? tup2) tup1)
      (else (cons (add (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
; (> n m)
(define bigger
  (lambda (n m)
    (cond
      ((zero? n) #f) ;order matters!
      ((zero? m) #t)
      (else (bigger (sub1 n) (sub1 m))))))
(define smaller
  (lambda (n m)
    (cond
      ((zero? m) #f) ;false formmer
      ((zero? n) #t)
      (else (smaller (sub1 n) (sub1 m))))))
(define equal1
  (lambda (n m)
    (cond
      ((and (zero? n) (zero? m)) #t)
      ((or (zero? n) (zero? m)) #f)
      (else (equal1 (sub1 n) (sub1 m))))))
(define equal2
  (lambda (n m)
    (cond
     ((bigger n m) #f)
     ((smaller n m) #f)
     (else #t))))
; ^|  expt
(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (mul n (^ n (sub1 m)))))))
(define ??? ;div .quotient
  (lambda (n m)
    (cond
      ((smaller n m ) 0)
      (else (add1 (??? (sub n m) m))))))
; (div 15 4) = 1 + (div 11 4) = 1 + 1 + (div 7 4) ...
;length
(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
; pick n lat
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
; rempick. pick and remove
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (rempick (sub1 n) (cdr lat))))))
; number? primitive function. can't rewrite.(add1, sub1, zero?, car, cdr, cons, null?, eq? atom?)
; no-nums remove number in lat.
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
; all0nums
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (cons (car lat) (all-nums (cdr lat)))))
      (else (all-nums (cdr lat))))))
; eqan? equal for number and atoms
; and num? n num? m equal1 n m
; or num? n num? m #f
; eq? n m
(define eqan? ;replace eq? with eqan? in all func except eqan?
  (lambda (n m)
    (cond
      ((and (number? n) (number? m) (equal2 n m)))
      ((and (atom? n) (atom? m)) (eq? n m))
      (else #f))))
; occur number of times atom apear in lat
(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else (cond
              ((eq? a (car lat)) (add1 (occur a (cdr lat))))
              (else (occur a (cdr lat))))))))
(define one?
  (lambda (n)
    (eqan? n 1))) ;atom can not necessarily be sub-ed.
      
