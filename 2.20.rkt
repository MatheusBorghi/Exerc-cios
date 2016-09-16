#lang racket
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (same-parity a . b)
  (define (aux list b)
    (if (empty? b)
        list
        (if (or
             (and (even? a) (even? (car b)))
             (and (odd? a) (odd? (car b))))
            (aux (append list (cons (car b) empty)) (cdr b))
            (aux list (cdr b)))))
  (aux (list a) b))

(define (append a b)
  (if (empty? a)
      b
      (cons (car a) (append (cdr a) b))))