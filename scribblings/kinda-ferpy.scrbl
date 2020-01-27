#lang scribble/manual

@require[@for-label[kinda-ferpy
                    racket/base
                    racket/contract
                    racket/undefined]]

@title{Expressive Functional Reactive Programming (Kinda)}
@defmodule[kinda-ferpy]

This module provides a convenient reactive programming model that
favors a particular mode of writing. Based on
@hyperlink["https://github.com/MaiaVictor/PureState"]{PureState}.

@bold{Warning}: The interface is currently unstable. Expect
@racket[stateful-cell] to change it's name to
@racket[make-stateful-cell].  @racket[stateful-cell] will become a
macro.

@section{Reading and Writing Values}
To create a cell, apply @racket[stateful-cell] to a single Racket value.

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

@section{Spreadsheet Metaphor}
If you've used Excel, you know how this works: If a cell changes, then the
cells that depend on that cell also change.

@racketblock[
(require kinda-ferpy)

(define x (stateful-cell 1))
(define y (stateful-cell 1))
(define sum (stateful-cell (λ _ (+ (x) (y)))))

(displayln (sum)) (code:comment "2")
(y 8)
(displayln (sum)) (code:comment "9")
]

Here, @racket[stateful-cell] (or @racket[stateful-cell]) represents
optionally-dependent computations. It can apply to any one Racket
value. If that value is a procedure, then it will be hooked up to any
other cells that it uses. @racket[sum] depends on @racket[x] and
@racket[y] by virtue of use. Signals and events are not explicitly
declared, meaning that this is not a full interface for functional reactive
programming. It's just a nice way to model dependency relationships.

When a cell is initialized with a procedure, that procedure will be
applied in a @italic{discovery phase} where the use of other cells
flags them as dependencies. It will then be applied once more to
initialize the cell. All of that only happens if you want this library
to think of your dependencies as @italic{implicit} dependencies.
We'll cover more about the implications and how to handle explicit
dependencies in a moment.

All values are synchronized on the current thread when initializing or
updating a stateful cell. In the above example, the body of
@racket[sum] evaluates when it is first declared, and as a consequence
of evaluating @racket[(y 8)]. When evaluating @racket[(sum)], you are
@italic{only accessing that cell's value}. The library will keep data
in sync for you while doing only minimal work.

@section{Fair Warnings}

@itemlist[
@item{Cycles create infinite loops. @racket[current-hard-walk-limit] is your empirical protection.}
@item{You will find reason to represent errors as values without raising an exception. For example,
a spreadsheet application will handle a division by zero by @hyperlink["https://filedn.com/lI3m84JVCumjvoKMPcGOsVp/images/spreadsheet%20error.png"]{showing a special error value} instead of crashing.}
@item{Every cell will be evaluated immediately to discover dependencies.}
@item{It's possible to produce incorrect values as a result of a stateful cell not being visible during discovery.
@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum (stateful-cell (λ _ (+ (x) (if (switch) 1 (y))))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "DANGER: Won't update sum's cell!")
(sum) (code:comment "Still 0. Should be 1.")
]

From the way @racket[if] works, @racket[(y)] is not evaluated at
discovery time. It therefore will not be recognized as a dependency of
@racket[sum].}]

Let's cover some ways to address this.

@section{Adjusting Your Writing}
You can address dependency discovery blind spots by either
writing expressions where they will always be evaluated,
or listing dependencies explicitly.

For the former case, you can move dependencies out of the @racket[if].

@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum (stateful-cell (λ _
  (let ([x-val (x)]
        [y-val (y)]
    (+ x-val (if (switch) 1 y-val)))))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "Will update the cell now")
(sum) (code:comment "1")
]

If that seems like a bad precedent to you, then list dependencies
explicitly using the optional @racket[#:dependencies] argument to @racket[stateful-cell].
This will modify @racket[stateful-cell] such that it will visit the cited
nodes, but will not evaluate any procedure you provide in the cell
for dependency discovery purposes.

@racketblock[
(define switch (stateful-cell #t))
(define x (stateful-cell 1))
(define y (stateful-cell -1))
(define sum
  (stateful-cell #:dependencies (list switch x y)
    (λ _ (+ (x) (if (switch) 1 (y))))))

(sum) (code:comment "2")
(switch #f)
(sum) (code:comment "0")
(y 0) (code:comment "Will update the cell now")
(sum) (code:comment "1")
]

Take care to list @italic{every dependency} in this case.

This addresses both blind spots in dependency discovery by making
missing dependencies obvious, while allowing you to skip potentially
expensive operations during discovery. So why support implicit
dependencies at all? Because then you save time avoiding boilerplate
if your state graph is simple or planned.

If you don't want to use @racket[#:dependencies] but are still worried
about expensive computations or side-effects kicking off too early,
then you can check @racket[(discovery-phase?)] within a stateful cell
body. It will tell you if the cell is being evaluated for discovery
purposes. This gives you a hybrid approach where you can list
dependencies for discovery, and express a relatively expensive
computation for the times you need to compute a value.

@racketblock[
(define c
  (% (lambda ()
    (if (discovery-phase?)
        (begin (file-path)
               (file-proc)
               (void))
        (call-with-input-file (file-path)
                              (unbox (file-proc)))))))]

This does of course mean that any dependent cells may need to
understand and react to conditions created during discovery. This
example uses @racket[(void)], and that will matter to any downstream
cells checking for, say, @racket[undefined] during the discovery phase.
That would be a problem if you wanted to do something interesting like
choose a new set of dependencies based on upstream behavior during the
discovery phase.

@section{Reference}
@defproc[(stateful-cell [#:dependencies explicit-dependencies (listof stateful-cell?) '()]
                        [managed (if/c procedure? (-> any/c) any/c)])
                        stateful-cell?]{
Returns a stateful cell @racket[P] that, when applied, returns the latest correct version of
a Racket value in terms of dependencies.

The behavior of @racket[P] and @racket[stateful-cell] both depend on @racket[managed]
and @racket[explicit-dependencies].

If @racket[managed] is not a procedure, then @racket[(P)] will return @racket[managed].

If @racket[managed] is a procedure, then @racket[stateful-cell] will
apply @racket[managed] once or twice according to the value of
@racket[explicit-dependencies]:

@itemlist[
@item{If @racket[explicit-dependencies] is an empty list, then
@racket[stateful-cell] assumes that you would rather let it discover
dependencies for you. In this case, @racket[managed] will be applied
in a parameterization where @racket[discovery-phase?] is
@racket[#t]. Any other stateful cells applied in the body of the
procedure bound to @racket[managed] will be captured as dependencies
of the stateful cell holding @racket[managed]. @racket[managed] is
then applied once more (where @racket[discovery-phase?] is
@racket[#f]) to store its initial value. @bold{Any stateful cells that
are not evaluated in the body of @racket[managed] are not captured as
dependencies.}}

@item{If @racket[explicit-dependencies] is not empty, then @racket[stateful-cell] assumes that
you know what you want and no discovery phase is necessary.  @racket[managed] will be applied once
(with @racket[discovery-phase?] set to @racket[#f]) to initialize its cell value. @racket[explicit-dependencies]
are used as-is to construct relationships to other cells.}
]
}

@racket[(P new-managed)] will update the stored value of @racket[P], and
will synchronously update all dependent cells. Be warned that setting
@racket[new-managed] to a different procedure will NOT initialize a new
dependency discovery phase, nor will it change the existing dependency
relationships of @racket[P]. If you want a new discovery phase, create
a new stateful cell.

@defthing[※ procedure? #:value stateful-cell]{
For those who love single-character aliases and reconfiguring their editor.
}

@defthing[% procedure? #:value stateful-cell]{
For those who love single-character aliases but hate it when people make them reconfigure their editor.

For the rest, there's @racket[rename-in].
}

@defproc[(stateful-cell? [v any/c]) boolean?]{
Return @racket[#t] if @racket[v] is a value constructed with @racket[stateful-cell].
}

@defproc[(discovery-phase?) boolean?]{
Returns @racket[#t] if the library is currently looking for implicit dependencies.

You can use this to avoid potentially expensive operations within the body of a stateful
cell. If you do not use the @racket[#:dependencies] argument in @racket[stateful-cell],
you can use the following pattern to make dependencies visible and avoid unnecessary work.

@racketblock[
(define c
  (% (lambda ()
    (if (discovery-phase?)
        (begin (file-path)
               (file-proc)
               (void))
        (call-with-input-file (file-path)
                              (unbox (file-proc)))))))]

}

@defthing[current-hard-walk-limit (parameter/c exact-positive-integer?) #:value 10000]{
A parameter that sets the hard maximum for the number of times change can propogate to
dependencies. This is a simple, empirical way to respond to cycles.
}
