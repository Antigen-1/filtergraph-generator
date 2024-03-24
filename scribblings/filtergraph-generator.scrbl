#lang scribble/manual
@require[@for-label[filtergraph-generator
                    (except-in racket/base complex?)]
         scribble/bnf]

@title{filtergraph-generator}
@author{zhanghao}

@defmodule[filtergraph-generator]

A ffmpeg filtergraph renderer.

@section{Usage}

@(let ([open @litchar{(}]
       [close @litchar{)}]
       [dot @litchar{.}])
   @BNF[(list @nonterm{maybe-sws-flags}
              @BNF-seq[@optional[open @litchar{#:sws-flags} dot @nonterm{string} close]])
        (list @nonterm{name}
              @elem{sequence of alphanumeric characters and @litchar{_}})
        (list @nonterm{label}
              @nonterm{name})
        (list @nonterm{filter argument}
              @BNF-alt[
                       @elem[open @nonterm{keyword} dot @nonterm{string} close]
                       @nonterm{string}
                       ])
        (list @nonterm{filter}
              @BNF-seq[open
                       open @nonterm{name} @kleenestar[@nonterm{filter argument}] close
                       @litchar{:}
                       open @kleenestar[@nonterm{label}] close
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
