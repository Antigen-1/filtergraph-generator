#lang scribble/manual
@require[@for-label[filtergraph-generator
                    (except-in racket/base complex?)]
         scribble/bnf]

@title{filtergraph-generator}
@author{zhanghao}

@defmodule[filtergraph-generator]

A ffmpeg filtergraph renderer.

@section{Filtergraph Representation}

@(let ([open @litchar{(}]
       [close @litchar{)}]
       [dot @litchar{.}])
   @BNF[(list @nonterm{maybe-sws-flags}
              @BNF-seq[@optional[open @litchar{#:sws-flags} dot @nonterm{string} close]])
        (list @nonterm{name}
              @elem{sequence of alphanumeric characters and @litchar{_}})
        (list @nonterm{label}
              @nonterm{name})
        (list @nonterm{complex input label}
              @BNF-seq[open @nonterm{file index(exact nonnegative integer)}
                            @nonterm{stream specifier(exact nonnegative integer)} close]
              @BNF-seq[open
                       @litchar{dec}
                       @nonterm{the index of loopback decoder(exact nonnegative integer)}
                       close])
        (list @nonterm{input label}
              @BNF-alt[@nonterm{label} @nonterm{complex input label}])
        (list @nonterm{filter argument}
              @BNF-alt[
                       @BNF-seq[open @nonterm{keyword} dot @nonterm{string} close]
                       @nonterm{string}
                       ])
        (list @nonterm{filter}
              @BNF-seq[open
                       open @nonterm{name} @kleenestar[@nonterm{filter argument}] close
                       @litchar{:}
                       open @kleenestar[@nonterm{input label}] close
                       @litchar{->}
                       open @kleenestar[@nonterm{label}] close
                       close])
        (list @nonterm{filterchain}
              @BNF-seq[open
                       @litchar{>>>}
                       @kleeneplus[@nonterm{filter}]
                       close])
        (list @nonterm{filtergraph}
              @BNF-seq[open
                       open @nonterm{maybe-sws-flags} close
                       @kleeneplus[@nonterm{filterchain}]
                       close])])

@section{Exported Functions}

@defproc[#:kind "predicate"
         (filtergraph? (value any/c))
         boolean?]
@defproc[#:kind "renderer"
         (render-filtergraph (value filtergraph?))
         string?]
@defparam[complex? flag boolean? #:value #f]
