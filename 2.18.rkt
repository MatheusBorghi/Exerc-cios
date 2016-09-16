#lang racket
(define (reverse list)
  (define (aux list1 list2)
    (if (empty? list1)
        list2        
        (aux (cdr list1)
             (cons (car list1) list2))))
    (aux list empty))