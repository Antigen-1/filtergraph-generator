#lang racket/base
(require racket/contract)
(provide (contract-out (complex? (parameter/c boolean? boolean?))))
(define complex? (make-parameter #f))
