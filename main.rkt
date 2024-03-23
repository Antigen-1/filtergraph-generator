#lang racket/base

(require "private/syntax.rkt")
(provide (all-from-out "private/syntax.rkt"))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract racket/file raco/command-name)
  (define where (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-p" "--path") path "Specify the file path" (set-box! where path)]
    #:args ()
    (define/contract path string? (unbox where))
    (display (render-filtergraph (file->value path)))))
