#lang racket/base
(require racket/contract racket/match)
(provide (contract-out (parse-argument-list (-> list? (or/c list? #f)))))

;; Positional arguments: any/c
;; Keyword arguments: keyword? any/c
;; The values of positional arguments must not be (cons/c keyword? any/c)
(define (parse-argument-list l)
  (let loop ((original l) (resolved null))
    (cond ((null? original) (reverse resolved))
          ((keyword? (car original))
           (and (not (null? (cdr original)))
                (loop (cddr original) (cons (cons (car original) (cadr original)) resolved))))
          ((match (car original)
             (`(,(? keyword?) . ,_)
              #t)
             (_ #f))
           #f)
          (else (loop (cdr original) (cons (car original) resolved))))))

(module+ test
  (require rackunit)
  (check-false (parse-argument-list '(a #:a)))
  (check-equal? (parse-argument-list '(#:a #:a))
                (list (cons '#:a '#:a)))
  (check-equal? (parse-argument-list '(#:a a b))
                (list (cons '#:a 'a) 'b))
  (check-false (parse-argument-list '((#:a . b))))
  (check-equal? (parse-argument-list '(a b))
                (list 'a 'b)))
