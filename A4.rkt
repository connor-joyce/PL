#lang racket

(require 2htdp/planetcute)
(require test-engine/racket-tests)

;downseries takes in a list(expected to be numbers, where step is >0)
;at every step the function appends the current high to the recursive
;call of the function itself, where high is now (high - step)
;if high is ever lower than low, return an empty list
;this means that the return will be a list, and it will have
;the given original high in the 0th position, original high - step in 1st
;etc
(define downseries (lambda (step high low)
                     (if (< high low) '()
                         (cons high (downseries step (- high step) low)))))


;define an anonymous function that appends the string "meow"
;to whatever is passed into it. Call that anonymous function
;in the map function, which passes every string in a given
;list to the anonymous function. This appends the current value from the list to
;the value "meow", then does this for every value in the list. 
(define (meow-string-map lst)
  (map (lambda (x) (string-append x "meow")) lst))


;function first checks to see if n < 0, if it is it throws an error
;then checks to see if lst is null, if it is then the function throws an error
;once those have cleared then use list-ref to get the element of lst
;at n/length-of-lst
(define list-ref-div
  (lambda (lst n) (if (< n 0) (error "list-ref-div:negative number")
                      (if (null? lst) (error "list-ref-div: empty list")
                          (list-ref lst (quotient n (length lst)))))))


;takes a stream and a number, checks if the number is 0, if so return the empty list
;otherwise cons the head of the stream 
;to the recursive call on the rest of the stream, decrementing k by one.
;Since no parentheses are arround the stream as it is passed in, I have to put them around
;the s in car to actually access the list, but not around the (cdr (s)) because
;it gets handled by the parentheses in the car call
(define (next-k-items s k)
  (if (= 0 k) '()
      (cons (car (s)) (next-k-items (cdr (s)) (- k 1)))))

;works the same way as the above function, except it doesn't return a list.
;iterate through the stream until k = 1 (not 0 this time because we want to return it, not add it to a list and don't
;need the empty list returned). I still assume there are no parentheses around the stream call when the function is called like
;I did above, which informs my parentheses placement in the cdr and car functions. 
(define (kth-item s k)
  (if (= 1 k) (car (s))
      (kth-item (cdr (s)) (- k 1))))


;uses thunks so that we don't get infinite recursion when we
;run this. The thunks only get evaluated when they need to be
;the first if statement checks to see if x is divisible by two,
;if so then just recurse with the next number, same for 5 in the
;else statement (if (= 0 (remainder x 5)). If neither of those
;things are true then cons the current x to the next call.
;finally call the function with a starting value of 1. 
(define negate-2-and-5
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 2))
                    (cons (* -1 x) (lambda () (f (+ x 1))))
                    (if (= 0 (remainder x 5))
                        (cons (* -1 x) (lambda () (f (+ x 1))))
                        (cons x (lambda () (f (+ x 1)))))))])
    (lambda () (f 1))))


;uses a recursive definition of f to define the stream
;the anonymous function cons a heart to the stream if x = 0,
;and then passes x = 1, if x = 1 a key is cons'ed to the stream,
;and x=2 is passed into the function, if x doesn't equal 1 or 0,
;a yellow-star is consed to the list and x is set to 0.
;The assignment/Jeff said to go in the order heart -> key -> star even though the name is
;not in that order

(define key-heart-star
  (letrec ([f (lambda ()
                (cons heart (lambda () (cons key
                                             (lambda () (cons yellow-star f))))))])
    (lambda () (f))))


;construct a new stream from a given stream, using lambda in the cons to prevent an infinite recursion
;imperfectly cons'es the head of the given list with the number 2, then cons'es that to the recursive call of
;the function. 
(define (two-pairs-stream s)
  (letrec ([f (lambda (x)
                (cons
                 (cons 2 (car (x))) (lambda () (f (cdr (x)))))
                )])
    (lambda () (f s))))


;put the helper call in a thunk so that the resulting stream is a procedure that can be applied
;like the rest of the streams I've made
(define (spin-stream xs ys)
  (lambda () (helper xs ys 0)))

;append the element of each list at n%(either list length) together in an imperfect pair
;then append that pair to the following call, where n = n + 1. Modulo means the elements will
;keep looping back through the lists forever, there is no base case the stream is infinite
;the recursive call is in a thunk so that I don't get caught up in the infinite recursion
(define (helper xs ys n)
   (cons (cons (list-ref xs (modulo n (length xs)))
        (list-ref ys (modulo n (length ys)))) (lambda () (helper xs ys (add1 n)))))

;took this from class notes to do stream testing
(define nats (letrec ([f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))])
               (lambda () (f 1))))


;the function takes a vector of pairs and a number
;the then creates an anonymous function that takes a number, I'm using this
;to reference specific elements of the vector. This anonymous function is f
;f checks to see if the given 'counter' x is the same as the length of the vector,
;if it is the function has checked every element and found nothing, return false
;then it checks to see if the next value in the vector is a pair, if it isn't
;skip it by recursing. If it is a pair then check the car of the pair with the
;original given value, if they are the same return that pair, otherwise
;recurse and increment x by one. 
(define (kvpv-lookup v vec)
  (letrec ([f (lambda (x)
                (if (>= x (vector-length vec)) #f
                (if (pair? (vector-ref vec x))
                    (if (= v (car (vector-ref vec x)))
                        (vector-ref vec x)
                        (f (add1 x)))
                    (f (add1 x)))))])
    (f 0)))

;(cached-lookup is called and given a list (which isn't used) and a number
;a vector of size n is created, the function returns another function that takes the list (which is used)
;and a value. I'm expecting cached-lookup to be bound to some binding, which makes the vector persistent.
;If the function is not bound, then the vector gets re-created at every call. For example,
;calling it like ((cached-lookup lst 10) lst 5) twice say the value wasn't in the cache on the second call,
;because calling it like this re-sets the vector. Calling it like (define foo (cached-lookup lst 10))
;(foo lst 5) (foo lst 6) works because the vector is always the same on foo calls.
;the returned function uses kvpv lookup to determine if the given value is in the cache, if it is then return
;(#t . p) where p is the kvpv-lookup returned value. If it isn't in the cache, assoc over the list to see if its there
;if it is return (#f . p) where p is the assoc returned value. Then add it to the vector at cache-slot, which increments
;and gets modulo'd by the vector length so that its circularly going through the vector. If the value isn't in the cache
;or the list, then return #f. 
(define (cached-lookup list n)
  (letrec ([cache (make-vector n #f)]
           [cache-slot 0]
           [f (lambda (v lst)
                (if (not (kvpv-lookup v cache))
                    (if (not (assoc v lst))
                        #f
                        (let ([next-spot (remainder cache-slot (vector-length cache))])
                          (vector-set! cache next-spot (assoc v lst))
                          ;(printf "Vector Set at: ~a : ~a" next-spot (vector-ref cache next-spot))
                          (set! cache-slot (add1 cache-slot))
                          (cons #f (assoc v lst))))
                    (cons #t (kvpv-lookup v cache))))])
    f))
                    

;tests
; 1)
; notice, the first argument to check-expect is our test, the second argument is the expected result
(check-expect (downseries 2 11 3) '(11 9 7 5 3))

; 2)
(check-expect (meow-string-map '("hi" "hello" "there")) '("himeow" "hellomeow" "theremeow"))

; 3)
; you should probably have one that exceeds the bounds of your list though, like 6
(check-expect (list-ref-div (list 1 2 3) 0) 1)
(check-expect (list-ref-div (list 1 2 3) 3) 2)

; 4)
; this assumes you have nats defined
(check-expect (next-k-items nats 3) '(1 2 3))

; 5)
(check-expect (kth-item nats 3) 3)

; 6)
(check-expect (next-k-items negate-2-and-5 7) '(1 -2 3 -4 -5 -6 7))

; 7)
(check-expect (next-k-items key-heart-star 2) (list heart key))

; 8)
(check-expect (kth-item (two-pairs-stream nats) 2) '(2 . 2))

; 9)
(check-expect (next-k-items (spin-stream '(1 2 3) '("a" "b")) 6)
              '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b")))
(check-expect (next-k-items (spin-stream '(1 2 3 4 5) '("z")) 10)
              '((1 . "z") (2 . "z") (3 . "z") (4 . "z") (5 . "z") (1 . "z") (2 . "z") (3 . "z") (4 . "z") (5 . "z")))

; 10)
(check-expect (kvpv-lookup 2 '#((1 . 1) (2 . 1))) '(2 . 1))

; 11)
; this will test if you're getting the expected results from the cached-lookup
(define tstlst '((1 . "a") (2 . "b") (3 . "c") (4 . "d") (5 . "e")))
(define cl-fun (cached-lookup tstlst 3))
(check-expect (cl-fun 6 tstlst) #f)
(check-expect (cl-fun 1 tstlst) '(#f 1 . "a"))
(check-expect (cl-fun 2 tstlst) '(#f 2 . "b"))
(check-expect (cl-fun 3 tstlst) '(#f 3 . "c"))
(check-expect (cl-fun 1 tstlst) '(#t 1 . "a"))
(check-expect (cl-fun 4 tstlst) '(#f 4 . "d"))
(check-expect (cl-fun 1 tstlst) '(#f 1 . "a"))
(check-expect (cl-fun 1 tstlst) '(#t 1 . "a"))

(test)



