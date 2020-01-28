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

@section{Computing Dependent Values Using the Spreadsheet Metaphor}
If you've used Excel, you know how this works: If a cell changes, then the
cells that depend on that cell also change.

Here, @racket[stateful-cell] represents optionally-dependent
computations. The @racket[1]s and @racket[(+ (x) (y))] each act as a
@deftech{stateful cell body}.

@racketblock[
(require kinda-ferpy)

(define x (stateful-cell 1))
(define y (stateful-cell 1))
(define sum (stateful-cell (+ (x) (y))))
(code:comment "NOTE: By this line, (+ 1 1) already ran.")

(displayln (sum)) (code:comment "2")
(y 8) (code:comment "<-- This updates sum too")
(displayln (sum)) (code:comment "9")
]

When defined, a stateful cell body is evaluated first to discover its
depdencencies and then to compute its value. The runtime is said to be
in a @deftech{discovery phase} when evaluating the cell body for the
purpose of finding dependencies. When evaluating expressions during this
phase, I'll say that they do so at @deftech{discovery time}.

@racket[sum] depends on @racket[x] and @racket[y] by virtue of
use. Signals and events are not explicitly declared, meaning that this
is not a full interface for functional reactive programming. It's just
a nice way to model dependency relationships because it looks so much
like normal procedure application. But don't be fooled; they are not
the same. Evaluating @racket[(sum)] merely returns a value and does
not run @racket[(+ (x) (y))] again.

Cell bodies are evaluated to synchronize values @italic{on the current
thread when initializing or updating} a stateful cell. In the above
example, the body of @racket[sum] evaluates when it is first defined,
and as a consequence of evaluating @racket[(y 8)]. When evaluating
@racket[(sum)], you are only accessing that cell's value. This is how
library keeps data in sync while doing only minimal work; change
propogates to dependents when change happens.

@section{Fair Warnings}
@itemlist[
@item{Cycles create infinite loops. @racket[current-hard-walk-limit] is your empirical protection.}
@item{You will find reason to represent errors as values without raising an exception. For example,
a spreadsheet application will handle a division by zero by @hyperlink["https://filedn.com/lI3m84JVCumjvoKMPcGOsVp/images/spreadsheet%20error.png"]{showing a special error value} instead of crashing.}
@item{Every cell will be evaluated immediately. When defining a cell, don't write any code that isn't "ready."}
@item{It's possible to produce incorrect values as a result of a stateful cell not being visible during discovery.
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
discovery time. It therefore will not be recognized as a dependency of
@racket[sum].}]

Let's cover some ways to address this.

@section{Explicit vs. Implicit Dependencies}
The examples above use @deftech{implicit dependencies}, which are
stateful cells encountered in the body of another cell at discovery
time. Above, we showed that a discovery phase will not detect a cell
unless it actually encounters it.

You can address these blind spots by either writing expressions where
they will always be evaluated, or listing dependencies explicitly.

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
dependencies explicitly. Doing so will cause @racket[stateful-cell] to
only apply its body to compute a value, not to discover
dependencies. This approach allows you to bind the values
of other cells by name, while avoiding unecessary indentation.

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
approach just changes the blind spot problem into a matter of whether
or not you remembered to list a dependency.

Why support implicit dependencies at all? Because they help you avoid
boilerplate and save time writing.

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

In general, it's a good idea to avoid side-effects at discovery time.

@section{Reference}
@defform[(stateful-cell maybe-dependency ... body ...+)
         #:grammar
         [(maybe-dependency (code:line)
                            (code:line #:dependency id existing-cell-id))]]{
Creates a stateful cell. The @racket[body] is placed inside of a
procedure as-is. If no dependencies are explicitly defined using @racket[#:dependency],
then the procedure containing @racket[body] will evaluate immediately to discover
dependencies. Any stateful cell accessed in @racket[body] will be flagged
as a dependency of the containing cell. @racket[body] will then be evaluated
again to compute the initial value of the cell.

If at least one dependency is defined using @racket[#:dependency], the
discovery phase is skipped. Each @racket[id] is an identifier that
will be bound to the value of the cell with a corresponding
@racket[existing-cell-id]. In this case, @racket[body] will only be
used to compute the value of the cell.

This macro expands to an application of @racket[make-stateful-cell], which
returns a procedure to interact with the cell. See @racket[make-stateful-cell]
for details on this procedure.

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
This is a procedure form for @racket[stateful-cell]. It returns a stateful cell @racket[P]
that, when applied, returns the latest correct version of a Racket value in terms of dependencies.

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

@item{If @racket[explicit-dependencies] is not empty, then @racket[stateful-cell] assumes that
you know what you want and no discovery phase is necessary.  @racket[managed] will be applied once
(with @racket[discovery-phase?] set to @racket[#f]) to initialize its cell value. @racket[explicit-dependencies]
are used as-is to construct relationships to other cells.}
]
}

Given the above, @racket[(P)] will return a cached reference to the
value last returned from @racket[managed].

@racket[(P new-managed)] will update the stored value of @racket[P], and
will synchronously update all dependent cells. Be warned that setting
@racket[new-managed] to a different procedure will NOT initialize a new
dependency discovery phase, nor will it change the existing dependency
relationships of @racket[P]. If you want a new discovery phase, create
a new stateful cell.

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

@defthing[current-hard-walk-limit (parameter/c exact-positive-integer?) #:value 10000]{
A parameter that sets the hard maximum for the number of times change can propogate to
dependencies. This is a simple, empirical way to respond to cycles.
}
