#lang scribble/manual

@require[@for-label[kinda-ferpy
                    racket/base
                    racket/contract
                    racket/undefined]]

@title{Expressive Functional Reactive Programming (Kinda)}
@defmodule[kinda-ferpy]

This module provides a convenient way to write programs using a
spreadsheet metaphor.

The underlying model is based on
@hyperlink["https://github.com/MaiaVictor/PureState"]{PureState}.

@section{Reading and Writing Values}
To create a cell, wrap @racket[stateful-cell] around a single Racket value.

@racketblock[
(define x (stateful-cell 1))
]

To get a value in a cell, apply it to no arguments.

@racketblock[
(x) (code:comment "1")
]

To set a new value, apply the cell to that value.

@racketblock[
(x 2) (code:comment "2")
]

@section{Computing Dependent Values}
If a cell changes, then the cells that depend on that cell also
change. If you've used Excel, then you know how this works.

Here, @racket[stateful-cell] represents optionally-dependent
computations. The @racket[1]s and @racket[(+ (x) (y))] each act as a
@deftech{stateful cell body}.

@racketblock[
(require kinda-ferpy)

(define x (stateful-cell 1))
(define y (stateful-cell 1))
(define sum (stateful-cell (+ (x) (y)))) (code:comment "*")

(displayln (sum)) (code:comment "2")
(y 8) (code:comment "*")
(displayln (sum)) (code:comment "9")
]

Normally Racket would evaluate @racket[(+ (x) (y))] every time you
apply @racket[sum]. But here, @racket[(+ (x) (y))] actually runs on
the lines marked with @litchar{*}. As already stated, @racket[(sum)]
merely retrieves a value that was already computed. @racket[sum]
depends on @racket[x] and @racket[y] by virtue of use, and
@racketmodname[kinda-ferpy] will keep all cells in sync for you. This
is an example of reactive programming.

If it was not already obvious, this will change how you write
code. For example, you might find reason to represent errors as values
without raising an exception. A spreadsheet application handles a
division by zero by
@hyperlink["https://filedn.com/lI3m84JVCumjvoKMPcGOsVp/images/spreadsheet%20error.png"]{showing
a special error value} instead of crashing.

Unlike other libraries and languages like FrTime, signals and events
are not explicitly declared in @racketmodname[kinda-ferpy]. So this is
not a full interface for reactive programming, it's just a nice way to
model dependency relationships using procedures. That way, when I say
"evaluating the cell body" or "applying the cell", I mean the same
thing. However, a cell's behavior is not fully equivalent to a
procedure because the expression @racket[(+ (x) (y))] does not always
run when you apply @racket[sum]. To understand why this is, we need to
cover a cell's lifecycle.

@section{Cell Lifecycle}
When you create a stateful cell, it starts life with no dependencies
and a value of @racket[undefined]. The cell then goes through a
@deftech[#:key "discovery"]{discovery phase} to find dependencies.
When evaluating expressions during this phase, I'll say that they do
so at @deftech{discovery time}. You can opt-out of this phase by using
explicit dependencies as per @secref{explicit-implicit}.

@racketmodname[kinda-ferpy] evaluates a cell body once if carrying out
a discovery phase. Whether it does this or not, it will still evaluate
the cell body to compute its initial value. Meaning that by the time a
@racket[stateful-cell] is done evaluating, the cell body ran either
once or twice.

@section[#:tag "explicit-implicit"]{Explicit vs. Implicit Dependencies}
A cell like @racket[(stateful-cell (+ (x) (y)))] uses
@deftech{implicit dependencies}, which are stateful cells encountered
while evaluating the body of another cell at discovery time.
There are blind spots that can later result in incorrect values:

@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum (stateful-cell (+ (x) (if (switch) 1 (y)))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "DANGER: Won't update sum's cell!")
(sum) (code:comment "Still 0. Should be 1.")
]

From the way @racket[if] works, @racket[(y)] is not evaluated at
discovery time. It will not be recognized as a dependency of
@racket[sum].

You can address this by either writing expressions where they will
always be evaluated, or listing dependencies explicitly.

For the former case, you can move dependencies that might not be
evaluated out of the @racket[if].

@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum
  (stateful-cell
    (let ([x-val (x)] [y-val (y)])
      (+ x-val (if (switch) 1 y-val)))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "Will update the cell now")
(sum) (code:comment "1")
]

If that seems like a bad precedent to you, then you can list
dependencies for your cells explicitly. Doing so will skip evaluation
of the cell bodies in the @tech[#:key "discovery"]{discovery phase}.
This approach also allows you to bind the values of other cells by name.

@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum
  (stateful-cell
    #:dependency x-val x
    #:dependency y-val y
    #:dependency s-val switch
    (+ x-val (if s-val 1 y-val))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "Will update the cell now")
(sum) (code:comment "1")
]

Take care to list @italic{every dependency} in this case. This
approach just means you have to worry about your own blind spots
instead of the system's. If you forget to list @racket[y] as a
dependency you'll still produce incorrect data.

If you don't want to use explicit dependencies and/or still want to
leverage the discovery phase, then you can check
@racket[(discovery-phase?)] within a stateful cell body. It will tell
you if the cell is being evaluated at discovery time. This gives you a
hybrid approach where you can list dependencies for discovery, and
express a relatively expensive computation for the times you need to
compute a value.

@racketblock[
(define c
  (stateful-cell
    (if (discovery-phase?)
        (begin (file-path)
               (file-proc)
               (void))
        (call-with-input-file (file-path)
                              (unbox (file-proc))))))]

The value you return in a cell body in a discovery phase
(@racket[(void)], in this case), won't matter because it won't be
stored as the value of the cell. In general, it's a good idea to avoid
additional side-effects at discovery time. After all, encountering
dependencies @bold{is} the intended side-effect.

@section{Reference}
@defform[(stateful-cell maybe-dependency ... body ...+)
         #:grammar
         [(maybe-dependency (code:line)
                            (code:line #:dependency id existing-cell-id))]]{
Creates a stateful cell. The @racket[body] is placed inside of a new
procedure as-is. If no dependencies are explicitly defined using
@racket[#:dependency], then the procedure containing @racket[body]
will evaluate immediately to discover dependencies. Any stateful cell
accessed in @racket[body] will be flagged as a dependency of the
containing cell. @racket[body] will then be evaluated again to compute
the initial value of the cell.

If at least one dependency is defined using @racket[#:dependency], the
discovery phase will simply use the dependencies you provide instead
of evaluating @racket[body]. Each @racket[id] is an identifier that
will be bound to the value of the cell with a corresponding
@racket[existing-cell-id]. In this case, @racket[body] will only be
used to compute the value of the cell.

This macro expands to an application of @racket[make-stateful-cell], which
returns a procedure to interact with the cell. See @racket[make-stateful-cell]
for more details.

@racketblock[
(define first-operand (stateful-cell 1))
(define second-operand (stateful-cell 2))
(define sum
  (stateful-cell #:dependency a first-operand
                 #:dependency b second-operand
                 (+ a b)))

(define square (stateful-cell (* (sum) (sum))))
]

For comparison, here's an equivalent code block that shows expansions
of @racket[stateful-cell].

@racketblock[
(define first-operand (make-stateful-cell (lambda () 1)))
(define second-operand (make-stateful-cell (lambda () 2)))
(define sum
  (make-stateful-cell #:dependencies (list first-operand second-operand)
    (lambda ()
      (let ([a (first-operand)]
            [b (second-operand)]
           (+ a b))))))

(define square (make-stateful-cell (lambda () (* (sum) (sum)))))
]}

@defproc[(make-stateful-cell [#:dependencies explicit-dependencies (listof stateful-cell?) '()]
                             [managed (if/c procedure? (-> any/c) any/c)])
                             stateful-cell?]{
This is a procedure form for @racket[stateful-cell]. It returns a
stateful cell @racket[P] that, when applied, returns the latest
correct version of a Racket value in terms of dependencies.

The behavior of @racket[P] and @racket[make-stateful-cell] both depend
on @racket[managed] and @racket[explicit-dependencies].

If @racket[managed] is not a procedure, then @racket[(P)] will return
@racket[managed].

If @racket[managed] is a procedure, then @racket[stateful-cell] will
@bold{immediately} apply @racket[managed] once or twice according to
the value of @racket[explicit-dependencies]:

@itemlist[
@item{If @racket[explicit-dependencies] is an empty list, then
@racket[make-stateful-cell] assumes that you would rather let it discover
dependencies for you. In this case, @racket[managed] will be applied
in a parameterization where @racket[discovery-phase?] is
@racket[#t]. Any other stateful cells applied in the body of the
procedure bound to @racket[managed] will be captured as dependencies
of the stateful cell holding @racket[managed]. @racket[managed] is
then applied once more (where @racket[discovery-phase?] is
@racket[#f]) to store its initial value. @bold{Any stateful cells that
are not evaluated in the body of @racket[managed] are not captured as
dependencies.}}

@item{If @racket[explicit-dependencies] is not empty, then
@racket[stateful-cell] assumes that you know what you want and no
discovery phase is necessary.  @racket[managed] will be applied once
(with @racket[discovery-phase?] set to @racket[#f]) to initialize its
cell value. @racket[explicit-dependencies] are used as-is to construct
relationships to other cells.}
]
}

Given the above, @racket[(P)] will return a cached reference to the
value last returned from @racket[managed].

@racket[(P new-managed)] will update the stored value of @racket[P], and
will synchronously update all dependent cells. Be warned that setting
@racket[new-managed] to a different procedure will NOT initialize a new
dependency discovery phase, nor will it change the existing dependency
relationships of @racket[P]. If you want to express new dependency
relationships, then create a new cell.

@defthing[※ stateful-cell]{
For those who love single-character aliases and reconfiguring their editor.
}

@defthing[% stateful-cell]{
For those who love single-character aliases but hate it when people make them reconfigure their editor.

For the rest, there's @racket[rename-in].
}

@defproc[(stateful-cell? [v any/c]) boolean?]{
Return @racket[#t] if @racket[v] is a value constructed with @racket[stateful-cell] or @racket[make-stateful-cell].
}

@defproc[(stateful-cell-dependencies [cell stateful-cell?]) (listof stateful-cell?)]{
Returns a list of @racket[cell]'s dependencies.
}

@defproc[(stateful-cell-dependents [cell stateful-cell?]) (listof stateful-cell?)]{
Returns a list of @racket[cell]'s dependents.
}

@defproc[(discovery-phase?) boolean?]{
Returns @racket[#t] if the library is currently looking for implicit dependencies.

You can use this to avoid potentially expensive operations within the body of a stateful
cell body. If you do not use the @racket[#:dependencies] argument in @racket[stateful-cell],
you can use the following pattern to make dependencies visible and avoid unnecessary work.

@racketblock[
(define c
  (stateful-cell
    (if (discovery-phase?)
        (begin (file-path)
               (file-proc)
               (void))
        (call-with-input-file (file-path)
                              (unbox (file-proc))))))]

}
