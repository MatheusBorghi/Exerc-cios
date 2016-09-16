#lang racket
(define a (list 9 0 8 9))

(define b (list 5 6 7 5))

(define (append a b)
  (if (empty? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (last-pair list)
  (define (aux list last)
    (if (empty? list)
        last
        (aux (cdr list) (car list))))
  (aux list empty))

