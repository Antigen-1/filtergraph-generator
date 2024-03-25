#lang racket/base
(require racket/contract)
(provide (contract-out (parse-argument-list (-> (listof (not/c (cons/c keyword? any/c))) any))))

;; Positional arguments: any/c
;; Keyword arguments: keyword? any/c
;; The values of positional arguments must not be (cons/c keyword? any/c)
(define (parse-argument-list l)
  (let loop ((original l) (resolved null))
    (cond ((null? original) (reverse resolved))
          ((keyword? (car original))
           (and (not (null? (cdr original)))
                (loop (cddr original) (cons (cons (car original) (cadr original)) resolved))))
          (else (loop (cdr original) (cons (car original) resolved))))))

(module+ test
  (require rackunit)
  (check-false (parse-argument-list '(a #:a)))
  (check-equal? (parse-argument-list '(#:a #:a))
                (list (cons '#:a '#:a)))
  (check-equal? (parse-argument-list '(#:a a b))
                (list (cons '#:a 'a) 'b))
  (check-equal? (parse-argument-list '(a b))
                (list 'a 'b)))
