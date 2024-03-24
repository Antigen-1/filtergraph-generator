#lang racket/base
(require racket/string racket/match racket/contract (for-syntax racket/base))
(provide (contract-out (stream-specifier? (-> any/c any))
                       (render-stream-speficier (-> stream-specifier? string?))))

(define sep ":")

;; stream_index
(define stream-index? exact-nonnegative-integer?)
(define (render-stream-index i) (format "~a" i))

;; stream_type[:additional_stream_specifier]
(define-match-expander stream-type-and-others
  (syntax-rules ()
    ((_ stream-type others)
     `((#:stream-type . ,(and stream-type (or 'V 'v 'a 's 't 'd)))
       ,@(and others (? stream-specifier?))))))
(define (stream-type-and-others? v)
  (match v
    ((stream-type-and-others _ _) #t)
    (_ #f)))
(define (render-stream-type-and-others v)
  (match v
    ((stream-type-and-others type others)
     (string-append (format "~a" type) sep (render-stream-speficier others)))))

;; g:group_specifier[:additional_stream_specifier]
(define-match-expander stream-group-and-others
  (syntax-rules ()
    ((_ info others)
     `((#:stream-group
        .
        ,(?
          (lambda (v) (or (exact-nonnegative-integer? v) (symbol? v)))
          (app (lambda (v)
                 (if (exact-nonnegative-integer? v)
                     (cons 'index v)
                     (cons 'id v)))
               info)))
       ,@(and others (? stream-specifier?))))))
(define (stream-group-and-others? v)
  (match v
    ((stream-group-and-others _ _) #t)
    (_ #f)))
(define (render-stream-group-and-others v)
  (match v
    ((stream-group-and-others info others)
     (string-append
      "g:"
      (match info
        ((cons 'index ind) (format "~a" ind))
        ((cons 'id id) (format "i:~a" id)))
      sep
      (render-stream-speficier others)))))

;; p:program_id[:additional_stream_specifier]
(define-match-expander program-and-others
  (syntax-rules ()
    ((_ id others)
     `((#:program . ,(and id (? symbol?)))
       ,@(and others (? stream-specifier?))))))
(define (program-and-others? v)
  (match v
    ((program-and-others _ _) #t)
    (_ #f)))
(define (render-program-and-others v)
  (match v
    ((program-and-others id others)
     (string-append "p:" (format "~a" id) sep (render-stream-speficier others)))))

;; #stream_id or i:stream_id
(define-match-expander stream-id
  (syntax-rules ()
    ((_ stream-id)
     `(#:stream-id . ,(and stream-id (? symbol?))))))
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

(define (stream-specifier/null? v)
  (or (stream-specifier? v) (null? v)))
(define (stream-specifier? v)
  (and (list? v) (not (null? v))
       (cond ((stream-type-and-others? v))
             ((stream-group-and-others? v))
             ((program-and-others? v))
             ((and (stream-index? (car v)) (stream-specifier/null? (cdr v))))
             ((and (stream-id? (car v)) (stream-specifier/null? (cdr v))))
             ((and (metadata? (car v)) (stream-specifier/null? (cdr v))))
             ((and (usable-configuration? (car v)) (stream-specifier/null? (cdr v))))
             (else #f))))
(define (render-stream-speficier v)
  (define (append part rest)
    (string-append* `(,part ,@(if (null? rest) null (list ":" (render-stream-speficier rest))))))
  (cond ((stream-type-and-others? v) (render-stream-type-and-others v))
        ((stream-group-and-others? v) (render-stream-group-and-others v))
        ((program-and-others? v) (render-program-and-others v))
        ((and (stream-index? (car v)) (stream-specifier/null? (cdr v)))
         (render-stream-index (car v)))
        ((and (stream-id? (car v)) (stream-specifier/null? (cdr v)))
         (render-stream-id (car v)))
        ((and (metadata? (car v)) (stream-specifier/null? (cdr v)))
         (render-metadata (car v)))
        ((and (usable-configuration? (car v)) (stream-specifier/null? (cdr v)))
         (render-usable-configuration (car v)))))

(module* test racket/base
  (require (submod "..") rackunit)
  (check-true (stream-specifier? `(0)))
  (check-true (stream-specifier? `((#:stream-type . V) 0)))
  (check-true (stream-specifier? `((#:stream-group . a) 0)))
  (check-true (stream-specifier? `((#:stream-id . a))))
  (check-true (stream-specifier? `((#:metadata "a" . "b"))))
  (check-true (stream-specifier? `((#:metadata . "a"))))
  (check-true (stream-specifier? `((#:usable-configuration . "b"))))
  (check-false (stream-specifier? `(0 (#:stream-type . V))))
  (check-false (stream-specifier? `(0 (#:stream-group . a))))
  (check-equal? (render-stream-speficier `(0)) "0")
  (check-equal? (render-stream-speficier `((#:stream-type . V) 0)) "V:0")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:stream-group . a) 0)) "V:g:i:a:0")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:program . a1) 0)) "V:p:a1:0")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:stream-id . a))) "V:i:a")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:metadata . "a"))) "V:m:a")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:metadata "s" . "a"))) "V:m:s:a")
  (check-equal? (render-stream-speficier `((#:stream-type . V) (#:usable-configuration . "a"))) "V:a")
  )
