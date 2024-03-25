#lang racket/base
(require racket/string racket/match racket/contract "keyword.rkt" (for-syntax racket/base))
(provide (contract-out (stream-specifier? (-> any/c any))
                       (render-stream-speficier (-> stream-specifier? string?))))

(define sep ":")

;; stream_index
(define-match-expander stream-index
  (syntax-rules ()
    ((_ index)
     `(#:stream-index . ,(? exact-nonnegative-integer? index)))))
(define (stream-index? v)
  (match v
    ((stream-index _) #t)
    (_ #f)))
(define (render-stream-index i)
  (match i
    ((stream-index ind) (format "~a" ind))))

;; stream_type[:additional_stream_specifier]
(define-match-expander stream-type
  (syntax-rules ()
    ((_ stream-type)
     `(#:stream-type . ,(and stream-type (or 'V 'v 'a 's 't 'd))))))
(define (stream-type? v)
  (match v
    ((stream-type _) #t)
    (_ #f)))
(define (render-stream-type v)
  (match v
    ((stream-type type)
     (format "~a" type))))

;; g:group_specifier[:additional_stream_specifier]
(define-match-expander stream-group
  (syntax-rules ()
    ((_ type value)
     `(#:stream-group
       .
       ,(?
         (lambda (v) (or (exact-nonnegative-integer? v) (string? v)))
         (app (lambda (v)
                (if (exact-nonnegative-integer? v)
                    (cons 'index v)
                    (cons 'id v)))
              (cons type value))))
     )))
(define (stream-group? v)
  (match v
    ((stream-group _ _) #t)
    (_ #f)))
(define (render-stream-group v)
  (match v
    ((stream-group type value)
     (string-append
      "g:"
      (cond
        ((eq? 'index type) (format "~a" value))
        ((eq? 'id type) (format "i:~a" value)))))))

;; p:program_id[:additional_stream_specifier]
(define-match-expander program
  (syntax-rules ()
    ((_ id)
     `(#:program . ,(and id (? string?))))))
(define (program? v)
  (match v
    ((program _) #t)
    (_ #f)))
(define (render-program v)
  (match v
    ((program id)
     (string-append "p:" (format "~a" id)))))

;; #stream_id or i:stream_id
(define-match-expander stream-id
  (syntax-rules ()
    ((_ stream-id)
     `(#:stream-id . ,(and stream-id (? string?))))))
(define (stream-id? v)
  (match v
    ((stream-id _) #t)
    (_ #f)))
(define (render-stream-id v)
  (match v
    ((stream-id id) (format "i:~a" id))))

;; m:key[:value]
(define-match-expander metadata
  (syntax-rules ()
    ((_ key value)
     `(#:metadata
       .
       ,(or `(,(? string? key) . ,(? string? value))
            (? string? (app (lambda (v) (list v #f)) (list key value))))))))
(define (metadata? v)
  (match v
    ((metadata _ _) #t)
    (_ #f)))
(define (render-metadata v)
  (match v
    ((metadata key value)
     (string-append* `("m:" ,key ,@(if value (list ":" value) null))))))

;; u
(define-match-expander u
  (syntax-rules ()
    ((_ u)
     `(#:usable-configuration . ,(? string? u)))))
(define (usable-configuration? v)
  (match v
    ((u _) #t)
    (_ #f)))
(define (render-usable-configuration v) (match v ((u u) u)))

(define (stream-specifier? ov)
  (and (list? ov) (not (null? ov))
       (let ((parsed (parse-argument-list ov)))
         (and parsed
              (letrec ((specifier?
                        (lambda (v)
                          (or (and (stream-type? (car v)) (specifier/null? (cdr v)))
                              (and (stream-group? (car v)) (specifier/null? (cdr v)))
                              (and (program? (car v)) (specifier/null? (cdr v)))
                              (and (stream-index? (car v)) (null? (cdr v)))
                              (and (stream-id? (car v)) (null? (cdr v)))
                              (and (metadata? (car v)) (null? (cdr v)))
                              (and (usable-configuration? (car v)) (null? (cdr v))))))
                       (specifier/null? (lambda (v) (or (null? v) (specifier? v)))))
                (specifier? parsed))))))
(define (render-stream-speficier ov)
  (define (append-specifier p v)
    (if (null? v)
        p
        (string-append p sep (render v))))
  (define (render v)
    (cond ((stream-type? (car v)) (append-specifier (render-stream-type (car v)) (cdr v)))
          ((stream-group? (car v)) (append-specifier (render-stream-group (car v)) (cdr v)))
          ((program? (car v)) (append-specifier (render-program (car v)) (cdr v)))
          ((stream-index? (car v))
           (render-stream-index (car v)))
          ((stream-id? (car v))
           (render-stream-id (car v)))
          ((metadata? (car v))
           (render-metadata (car v)))
          ((usable-configuration? (car v))
           (render-usable-configuration (car v)))))
  (define parsed (parse-argument-list ov))
  (render parsed))

(module* test racket/base
  (require (submod "..") rackunit)
  (check-true (stream-specifier? `(#:stream-index 0)))
  (check-true (stream-specifier? `(#:stream-type V)))
  (check-true (stream-specifier? `(#:stream-type V #:stream-index 0)))
  (check-true (stream-specifier? `(#:stream-group "a" #:stream-index 0)))
  (check-true (stream-specifier? `(#:stream-id "a")))
  (check-true (stream-specifier? `(#:metadata ("a" . "b"))))
  (check-true (stream-specifier? `(#:metadata "a")))
  (check-true (stream-specifier? `(#:usable-configuration "b")))
  (check-false (stream-specifier? `(#:stream-index 0 #:stream-type V)))
  (check-false (stream-specifier? `(#:stream-index 0 #:stream-group "a")))
  (check-equal? (render-stream-speficier `(#:stream-index 0)) "0")
  (check-equal? (render-stream-speficier `(#:stream-type v)) "v")
  (check-equal? (render-stream-speficier `(#:stream-type V #:stream-index 0)) "V:0")
  (check-equal? (render-stream-speficier `(#:stream-type V #:stream-group "a" #:stream-index 0)) "V:g:i:a:0")
  (check-equal? (render-stream-speficier `(#:stream-type V #:program "a1" #:stream-index 0)) "V:p:a1:0")
  (check-equal? (render-stream-speficier `(#:stream-type V #:stream-id "a")) "V:i:a")
  (check-equal? (render-stream-speficier `(#:stream-type V #:metadata  "a")) "V:m:a")
  (check-equal? (render-stream-speficier `(#:stream-type V #:metadata ("s" . "a"))) "V:m:s:a")
  (check-equal? (render-stream-speficier `(#:stream-type V #:usable-configuration "a")) "V:a")
  )
