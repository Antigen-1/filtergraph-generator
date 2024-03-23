#lang racket/base
(require racket/set racket/list racket/match racket/string racket/contract
         uuid
         "escape.rkt"
         (for-syntax racket/base))
(provide (contract-out (filtergraph? (-> any/c any))
                       (render-filtergraph (-> filtergraph? string?))))

(module+ test
  (require rackunit))

;; NAME ::= sequence of alphanumeric characters and '_'
(define (name? v)
  (define allowed-chars
    (list->set
     (cons #\_
           (map integer->char
                (append (range 65 (+ 65 26))
                        (range 97 (+ 97 26))
                        (range 48 (+ 48 10)))))))
  (and (symbol? v)
       (andmap (lambda (c) (set-member? allowed-chars c))
               ((compose1 string->list symbol->string) v))))
(define render-name symbol->string)
(define (generate-name)
  (string->symbol (string-replace (uuid-string) #rx"-" "_")))
(define (call-with-random-names nums proc)
  (apply proc (map (lambda (_) (generate-name)) (range 0 nums))))

(module+ test
  (check-true (name? (generate-name)))
  (check-false (name? 'a-b123))
  (call-with-random-names
   1
   (lambda (name) (check-equal? (render-name name) (symbol->string name)))))

;; FILTER_NAME ::= NAME["@"NAME]
(define-match-expander filter-name
  (syntax-rules ()
    ((_ name-list)
     (or
      (and name-list `(,(? name?) ,(? name?)))
      (? name? (app list name-list))))))
(define (filter-name? v)
  (match v
    ((filter-name names) #t)
    (_ #f)))
(define (render-filter-name v)
  (match v
    ((filter-name names)
     (string-join (map render-name names) "@"))))

(module+ test
  (check-true (filter-name? (generate-name)))
  (check-true (filter-name? `(,(generate-name) ,(generate-name))))
  (check-false (filter-name? 'a-b123))
  (call-with-random-names
   2
   (lambda (name1 name2)
     (check-equal? (render-filter-name (list name1 name2))
                   (string-append (render-name name1) "@" (render-name name2)))))
  (call-with-random-names
   1
   (lambda (name) (check-equal? (render-filter-name name) (render-name name)))))

;; LINKLABEL ::= "[" NAME "]"
;; LINKLABELS ::= LINKLABEL [LINKLABELS]
(define (link-labels? v)
  (and (list? v) (not (null? v)) (andmap name? v)))
(define (render-link-labels v)
  (string-append* (map (lambda (nm) (string-append "[" (render-name nm) "]")) v)))

(module+ test
  (check-true (link-labels? (list (generate-name))))
  (check-false (link-labels? null))
  (check-false (link-labels? '(a-b123)))
  (call-with-random-names
   2
   (lambda (name1 name2)
     (check-equal? (render-link-labels (list name1 name2))
                   (string-append "[" (render-name name1) "]"
                                  "[" (render-name name2) "]")))))

;; FILTER_ARGUMENTS ::= sequence of chars (possibly quoted)
(define-match-expander filter-argument
  (syntax-rules ()
    ((_ kw str)
     (or (? string? (app (lambda (v) (cons #f v)) (cons kw str)))
         `(,(? keyword? kw) . ,(? string? str))))))
(define (filter-arguments? v)
  (and (list? v)
       (andmap
        (lambda (a)
          (match a
            ((filter-argument _ _) #t)
            (_ #f)))
        v)))
(define (render-filter-arguments v)
  (define (escape-argument v)
    (escape (set #\: #\' #\\) v #\\))
  (define (render-keyword-argument v)
    (escape-argument (string-append (keyword->string (car v)) "=" (cdr v))))
  (define (render-argument v)
    (escape-argument v))
  (string-join
   (map
    (lambda (a)
      (match a
        ((and all (filter-argument kw str))
         (if kw (render-keyword-argument all) (render-argument str)))))
    v)
   ":"))

(module+ test
  (check-true (filter-arguments? (list "a" (cons '#:a "a"))))
  (check-false (filter-arguments? "a"))
  (let ((id (uuid-string))) (check-equal? (render-filter-arguments (list id)) id))
  (let ((id (uuid-string))
        (kw (string->keyword (uuid-string))))
    (check-equal? (render-filter-arguments (list (cons kw id)))
                  (string-append (keyword->string kw) "=" id)))
  (let ((id (uuid-string)))
    (check-equal? (render-filter-arguments (list (string-replace id #rx"-" ":")))
                  (string-replace id #rx"-" "\\:"))))

;; FILTER ::= [LINKLABELS] FILTER_NAME ["=" FILTER_ARGUMENTS] [LINKLABELS]
(define-match-expander filter
  (syntax-rules ()
    ((_ filter-name filter-arguments in-labels out-labels)
     `((,(? filter-name? filter-name) ,@(? filter-arguments? filter-arguments))
       :
       ,(or (? null? in-labels)
            (? link-labels? in-labels))
       ->
       ,(or (? null? out-labels)
            (? link-labels? out-labels))))))
(define (filter? v)
  (match v
    ((filter _ _ _ _) #t)
    (_ #f)))
(define (render-filter v)
  (match v
    ((filter name args in out)
     (string-append
      (render-link-labels in)
      (escape
       (set #\[ #\] #\, #\; #\' #\\)
       (string-append
        (render-name name)
        "="
        (render-filter-arguments args))
       #\\)
      (render-link-labels out)))))

(module+ test
  (define name (generate-name))
  (define kw (string->keyword (uuid-string)))
  (define str (uuid-string))
  (define filter `((,name (,kw . ,str) ,str) : (,name) -> (,name)))
  (check-true (filter? filter))
  (check-true (filter? `((,name) : () -> ())))
  (check-false (filter? null))
  (check-equal?
   (render-filter
    `((drawtext (#:text . "this is a 'string': may contain one, or more, special characters"))
      :
      () -> ()))
   "drawtext=text=this is a \\\\\\'string\\\\\\'\\\\: may contain one\\, or more\\, special characters")
  (check-equal? (render-filter filter)
                (string-append (render-link-labels (list name))
                               (render-filter-name name)
                               "="
                               (render-filter-arguments (list (cons kw str) str))
                               (render-link-labels (list name)))))

;; FILTERCHAIN ::= FILTER [,FILTERCHAIN]
(define-match-expander filterchain
  (syntax-rules ()
    ((_ filters)
     `(>>> ,@(? (lambda (l)
                  (and (not (null? l))
                       (andmap filter? l)))
                filters)))))
(define (filterchain? v)
  (match v
    ((filterchain _) #t)
    (_ #f)))
(define (render-filterchain v)
  (match v
    ((filterchain fs)
     (string-append* (map render-filter fs)))
    ))

(module+ test
  (check-true (filterchain? `(>>> ,filter ,filter)))
  (check-false (filterchain? `(>>>)))
  (check-equal? (render-filterchain `(>>> ,filter ,filter))
                (string-append (render-filter filter) (render-filter filter))))

;; FILTERGRAPH ::= [sws_flags=flags;] FILTERCHAIN [;FILTERGRAPH]
(define-match-expander filtergraph
  (syntax-rules ()
    ((_ sws-flags filterchains)
     `(,(or (and sws-flags `())
            (list-no-order `(#:sws-flags . ,(? string? sws-flags))))
       ,@(? (lambda (l)
              (and (not (null? l))
                   (andmap filterchain? l)))
            filterchains)))))
(define (filtergraph? v)
  (match v
    ((filtergraph _ _) #t)
    (_ #f)))
(define (render-filtergraph v)
  (match v
    ((filtergraph fls chains)
     ((if (null? fls)
          (lambda (str) str)
          (lambda (str) (string-append "sws_flags=" fls ";" str)))
      (string-join (map render-filterchain chains) ";")))))

(module+ test
  (define graph1 `(((#:sws-flags . "a"))
                  (>>> ,filter)
                  (>>> ,filter)))
  (define graph2 `(()
                   (>>> ,filter)
                   (>>> ,filter)))
  (check-true (filtergraph? graph1))
  (check-true (filtergraph? graph2))
  (check-false (filtergraph? `((>>> ,filter) (>>> ,filter))))
  (check-equal? (render-filtergraph graph1)
                (string-append "sws_flags=a;" (render-filter filter) ";" (render-filter filter)))
  (check-equal? (render-filtergraph graph2)
                (string-append (render-filter filter) ";" (render-filter filter))))
