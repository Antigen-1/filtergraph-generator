#lang racket/base
(require racket/contract racket/set racket/string)
(provide (contract-out (escape (-> (set/c char? #:kind 'dont-care) string? char? string?))))

(define (escape set str esc)
  (string-append*
   (reverse
    (for/fold ((str-list null))
              ((char (in-string str)))
      (cons
       (if (set-member? set char)
           (string esc char)
           (string char))
       str-list)))))

(module* test racket/base
  (require rackunit (submod ".."))
  (check-equal? (escape (list #\\ #\' #\]) "\\\"'" #\\)
                "\\\\\"\\'"))
