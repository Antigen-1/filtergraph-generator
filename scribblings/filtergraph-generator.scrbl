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
   @BNF[(list @nonterm{stream type}
              @BNF-alt[@litchar{V} @litchar{v} @litchar{a} @litchar{s} @litchar{t} @litchar{d}])
        (list @nonterm{stream index}
              @nonterm{exact nonnegative integer})
        (list @nonterm{stream group id}
              @nonterm{string})
        (list @nonterm{program id}
              @nonterm{string})
        (list @nonterm{stream id}
              @nonterm{string})
        (list @nonterm{usable configuration}
              @nonterm{string})
        (list @nonterm{file index}
              @nonterm{exact nonnegative integer})
        (list @nonterm{the index of loopback decoder}
              @nonterm{exact nonnegative integer})
        (list @nonterm{stream specifier}
              @BNF-seq[@litchar{#:stream-index} @nonterm{stream index}]
              @BNF-seq[@litchar{#:stream-type} @nonterm{stream type}]
              @BNF-seq[@litchar{#:stream-group} @nonterm{stream group id}]
              @BNF-seq[@litchar{#:program} @nonterm{program id}]
              @BNF-seq[@litchar{#:stream-id} @nonterm{stream id}]
              @BNF-seq[@litchar{#:metadata} @nonterm{metadata key or key-value pair}]
              @BNF-seq[@litchar{#:usable-configuration} @nonterm{usable configuration}]
              )
        (list @nonterm{maybe-sws-flags}
              @BNF-seq[@optional[open @litchar{#:sws-flags} dot @nonterm{string} close]])
        (list @nonterm{name}
              @elem{sequence of alphanumeric characters and @litchar{_}})
        (list @nonterm{label}
              @nonterm{name})
        (list @nonterm{complex input label}
              @BNF-seq[open @nonterm{file index}
                            @nonterm{stream specifier} close]
              @BNF-seq[open
                       @litchar{dec}
                       @nonterm{the index of loopback decoder}
                       close]
              )
        (list @nonterm{input label}
              @BNF-alt[@nonterm{label} @nonterm{complex input label}])
        (list @nonterm{filter argument}
              @BNF-alt[
                       @BNF-seq[@nonterm{keyword} @nonterm{string}]
                       @nonterm{string}
                       ])
        (list @nonterm{filter}
              @BNF-seq-lines[@list[open
                                   open @nonterm{name} @kleenestar[@nonterm{filter argument}] close
                                   @litchar{:}]
                                  @list[open @kleenestar[@nonterm{input label}] close]
                                  @list[@litchar{->}]
                                  @list[open @kleenestar[@nonterm{label}] close close]])
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
