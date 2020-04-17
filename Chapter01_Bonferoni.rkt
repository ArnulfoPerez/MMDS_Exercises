#lang racket

  
; This question generalizes the example of "evil-doers" visiting hotels, as in Section 1.2.3.
; Suppose as before that there are a billion people being monitored for 1000 days.
; Each person has a 1% probability of visiting a hotel on any given day, and hotels hold 100 people each, so there are 100,000 hotels.
; However, our test for evil-doers is different. We consider a group of p people evil-doers if they all stayed at the same hotel on d different days.
; Derive the formula for the expected number of sets of p people that will be suspected of evil-doing ("false accusations"),
; assuming that in fact there are no evil-doers, but all people behave at random,
; following the conditions stated in this problem (1% probability of visiting a hotel, etc.).
; Then, find in the list below, the choice that is the best approximation to the correct value for some d and p.
;
; Note: You may assume that d and p are sufficiently small that (1000 choose d) can be approximated as 1000d/d!, and similarly for p.
; Also note: all choices are either powers of 10 or 3 times a power of 10.
; We are looking for the choice that is the best approximation to the expected value for the given d and p.
;
;    d  p   f
;    2  2  2.5e5
;    2  3  0.833
;    3  2  10e-1
;    3  3  3e-14

;factorial
(define factt
  (lambda (n acc)
    (if (= n 0) 
        acc
        (factt (- n 1)  (* acc n )))))
(define fact
  (lambda(n)
    (factt n 1)))

; x^y
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))
;constants
(define people 1e9)
(define days 1000)
(define pr 1e-02)
(define capacity-hotel 1e2)
(define hotels 1e5)

;functions

(define two-in-hotel (/ (* pr pr) hotels))

(define (two-in-d x)
  (pow two-in-hotel x))
; One in p chooses hotel at random, the rest have to pick same hotel
(define (p-in-hotel p) (* (pow (/ pr hotels) p) hotels))

; assumes that the probability is independent of day
(define pr-activity
  (lambda(p d)
    (pow (p-in-hotel p) d)))

(define days-choose-d
  (lambda (d)
    (/ (pow days d) (fact d))))

(define people-choose-p
  (lambda (p)
    (/ (pow people p) (fact p))))

(define random-cases
  (lambda(p d)
    (* (days-choose-d d)(people-choose-p p))))

; (probability of p going to same hotel) * (choose p in people) * (choose d in days)
(define suspects
  (lambda(p d)
    (* (pr-activity p d) (random-cases p d))))


(define d 2)
(define p 3)
(suspects p d)