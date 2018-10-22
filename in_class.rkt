#lang racket

(define ten 10)

(define (add x y) (+ x y))

(define (test-map lst) (map (lambda (x) (+ x 2)) lst))

(define (sumlist lst) (foldl (lambda (x y) (+ x y)) 0 lst))
(define sumlist2 (lambda (x) (if (null? x) 0
                   (+ (car x) (sumlist2 (cdr x))))))

;(define (sumlist3 xs)
 ; (if (null? xs) 0
  ;    (if (list? xs) 
   ;   (if (number? (first xs))
    ;      (+ (first xs) (sumlist3 (rest xs)))
     ;     (+ (sumlist3 (first xs)) (sumlist3 (rest xs))))))

(define fact 
  (lambda (n)
    (if (= n 0) 1
        (* n (fact (- n 1))))))

(define (fact2 n)
  (if (= n 0) 1
      (* n (fact2 (- n 1)))))

;doesn't work, racket thinks (1) should be run like a function
;which doesn't make sense
;(define (fact3 n)
 ; (if (= n 0)
  ;    (1)
   ;   (* n (fact3 (- n 1)))))

(define (cube n) ( * n (* n n)))
(define (cube2 n) (* n n n))

(define foo
  (lambda () (1 2)))


;checks to see if a list is empty
(null? null)

;add given element to the front of given list
(cons 1 '(2 3))
(cons 1 (cons 2 (cons 3 '())))
;lists can contain more than one type
(cons 'a '(1 2 3))
(cons '(1 2) '(3 4 5))

; 'foo -> 'foo (this is the symbol foo, not the string containing "'foo")
; "foo" is the string "foo"