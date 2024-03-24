#lang racket/base

(require "private/data.rkt" "private/config.rkt")
(provide (all-from-out "private/data.rkt")
         (all-from-out "private/config.rkt"))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract racket/file raco/command-name)
  (define where (box #f))
  (define complex (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-p" "--path") path "Specify the file path" (set-box! where path)]
    [("-c" "--complex") "Generate complex filtergraph" (set-box! complex #t)]
    #:args ()
    (define/contract path string? (unbox where))
    (parameterize ((complex? (unbox complex)))
      (display (render-filtergraph (file->value path))))))
