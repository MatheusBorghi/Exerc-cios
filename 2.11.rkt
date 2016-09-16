#lang racket
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (sub-interval a b)
  (make-interval
   (- (lower-bound a)
      (lower-bound b))
   (- (upper-bound a)
      (upper-bound b))))

(define (mul-int x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval a b)
  (if (and
       (or (> (lower-bound a) 0)
           (> (lower-bound b) 0))           
       (> (upper-bound a)
          (upper-bound b) 0))
      (make-interval
       (* (lower-bound a)
          (lower-bound b))
       (* (upper-bound a)
          (upper-bound b)))
      (if (or
           (and (< (lower-bound a)
                   (upper-bound a) 0)
                (> (lower-bound b)
                   (upper-bound b) 0))
           (and (< (lower-bound b)
                   (upper-bound b) 0)
                (> (lower-bound a)
                   (upper-bound a) 0))
           (and (< (lower-bound a) 0)
                (> (upper-bound a)
                   (lower-bound b)
                   (upper-bound b) 0))
           (and (< (lower-bound b) 0)
                (> (upper-bound a)
                   (lower-bound a)
                   (upper-bound a) 0))
           (< (lower-bound a)
              (upper-bound a)
              (lower-bound b)
              (upper-bound b)
              0))
          
          (make-interval
           (* (upper-bound a)
              (upper-bound b))
           (* (lower-bound a)
              (lower-bound b)))
          (mul-int a b))))
           
           