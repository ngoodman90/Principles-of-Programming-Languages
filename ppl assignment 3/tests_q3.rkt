#lang racket

(require "utils.rkt")
(require "q3.rkt")

(define newton
  (lambda (f x0 eps)
    (if (< (abs (f x0)) eps)
        x0
        (let ((x1 (- x0 (/ (f x0) 
                           ((derive f) x0)))))
          ;(display x0)
          ;(newline)
          (newton f x1 eps)))))
    
(define f
  (lambda (x)
    (+ (expt x 3) (expt x 2))))

(define g
  (lambda (x)
    (+ (- (expt x 3) (* 2 x)) 2)))

(define y
  (lambda (x)
    (+ (* 5 x) 5)))


;; q3 tests
(define (q3-tests)
  (run-tests
   
   (test (let ((lznewton (lazy-function-root g -1)))
           (< (abs (- (nth lznewton 10) (newton g -1 0.001))) 0.1))
         => #t)
   
   (test (let ((lznewton (lazy-function-root f -0.6)))
           (< (abs (- (nth lznewton 10) (newton f -0.6 0.001))) 0.1))
         => #t)
   
   (test (let ((lznewton (lazy-function-root y 3)))
           (< (abs (- (nth lznewton 5) (newton y 3 0.001))) 0.1))
         => #t)
   
  ))

(display "q3-tests:\t\t")
(q3-tests)


