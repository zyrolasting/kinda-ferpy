#lang racket/base

;; Inspired by https://github.com/MaiaVictor/PureState
;; See tests and docs for usage details.

(provide (rename-out [stateful-cell %]
                     [stateful-cell ※])
         current-cell-value
         stateful-cell
         make-stateful-cell
         stateful-cell-dependencies
         stateful-cell-dependents
         stateful-cell?
         discovery-phase?)

(require racket/undefined
         racket/list
         racket/set
         racket/sequence
         (for-syntax racket/base
                     syntax/parse))

#| CELL CONSTRUCTION

Stateful cells are like spreadsheet cells.

(define a (stateful-cell 100))
(define b (stateful-cell 2))
(define product (stateful-cell (* (a) (b))))
|#

(define-syntax (stateful-cell stx)
  (define-splicing-syntax-class explicit-dependency
    #:description "an explicit dependency binding"
    (pattern (~seq #:dependency e:id)))

  (syntax-parse stx #:context #'stateful-cell
    [(stateful-cell d:explicit-dependency ... body:expr ...+)
     #'(make-stateful-cell #:dependencies (list d.e ...)
                           (λ () body ...))]

    [(stateful-cell d:explicit-dependency ...)
     (raise-syntax-error 'stateful-cell
                         "Expected body after dependency bindings"
                         stx)]))

(define (make-stateful-cell compute #:dependencies [explicit-dependencies '()])
  (define cell (node (normalize compute) undefined (seteq) (seteq)))
  (define dependencies (discover-dependencies cell explicit-dependencies))
  (set-node-dependencies! cell dependencies)
  (for ([dependency (in-set dependencies)])
    (set-node-dependents! dependency
                          (set-add (node-dependents dependency)
                                   cell)))
  (update! cell)
  cell)


#| MODEL

Cells are implemented in terms of a global graph, represented as nodes
with adjacency information.

A node can be applied like a procedure to get/set an associated
value. A node always represents data using a procedure to do this,
hence 'normalize'.
|#

(define (normalize compute)
  (if (procedure? compute)
      compute
      (λ _ compute)))

(struct node (compute value dependencies dependents)
  #:mutable #:property prop:procedure
  (λ (self [new-compute undefined])
    (when (discovery-phase?)
      ((capture-dependency-handler) self))
    (unless (eq? new-compute undefined)
      (set-node-compute! self (normalize new-compute))
      (update-graph! self))
    (node-value self)))

(define stateful-cell?
  (procedure-rename node? 'stateful-cell?))
(define stateful-cell-dependencies
  (procedure-rename node-dependencies 'stateful-cell-dependencies))
(define stateful-cell-dependents
  (procedure-rename node-dependents 'stateful-cell-dependents))



#| REFRESHING DATA

When a node updates, all of its dependents must update exactly
once. This is tricky because no affected cell should update
before its affected dependencies. This section deals in making
sure nodes have the correct values in response to change.
|#

; Some cells may need to clean up their old values, but there's
; already user feedback asking not to have formals for the cell
; body. A parameter acts as a compromise.
(define not-in-cell (gensym))
(define current-cell-value (make-parameter not-in-cell))

(define (update! n)
  (parameterize ([current-cell-value (node-value n)])
    (set-node-value! n ((node-compute n)))))

(define (update-graph! start)
  (define affected (mutable-seteq))

  (define (gather-affected! cell)
    (set-add! affected cell)
    (sequence-for-each gather-affected!
                       (in-set (node-dependents cell))))

  (define (update-affected! cell)
    (sequence-for-each
     (λ (dependency)
       (when (set-member? affected dependency)
         (update-affected! dependency)))
     (in-set (node-dependencies cell)))
    (update! cell)
    (set-remove! affected cell))

  (define (propogate-change! cell)
    (update-affected! cell)
    (for ([dependent (node-dependents cell)])
      #:break (= 0 (set-count affected))
      (propogate-change! dependent)))

  (gather-affected! start)
  (propogate-change! start))


#| DEPENDENCY DISCOVERY

The runtime globally enters a discovery phase during cell
construction if the user does not specify dependencies themselves.
This can be helpful, but the process has blind spots.
|#
(define capture-dependency-handler (make-parameter #f))

(define (discovery-phase?)
  (procedure? (capture-dependency-handler)))

(define (discover-dependencies cell explicit-dependencies)
  (apply seteq
         (if (> (length explicit-dependencies) 0)
             explicit-dependencies
             (let ([captured-deps '()])
               (parameterize ([capture-dependency-handler
                               (λ (discovered)
                                 (set! captured-deps
                                       (cons discovered captured-deps)))]
                              [current-cell-value (node-value cell)])
                 ((node-compute cell)))
               captured-deps))))


(module+ test
  (require rackunit)
  (define (with-call-counter proc)
    (let ([num-calls 0])
      (values (λ A
                (set! num-calls (add1 num-calls))
                (apply proc A))
              (λ _ num-calls))))

  (test-true "Can identify instances"
             (and (stateful-cell? (make-stateful-cell 1))
                  (stateful-cell? (stateful-cell 1))))

  (test-case "make-stateful-cell represents data or other procedures trivially"
    (let ([x 10])
      (check-equal? ((make-stateful-cell x)) x)
      (check-equal? ((make-stateful-cell (λ () x))) x)))

  (test-case "stateful-cell uses the body for a new procedure, even if it means returning another procedure."
    (let ([x 10])
      (check-equal? ((stateful-cell x)) x)
      (check-pred procedure? ((stateful-cell (λ () x))))))

  (test-case "Racket values can form dependency relationships"
    (let* ([a (stateful-cell 1)]
           [b (stateful-cell 1)]
           [c (stateful-cell (+ (a) (b)))])
      (check-equal? (c) 2)
      (a 2)
      (check-equal? (c) 3)))

  (test-case "Dependencies are not recomputed unless necessary"
    (define-values (a a-count) (with-call-counter (λ _ 1)))

    ; Sanity check the counter
    (test-equal? "a was not yet called" (a-count) 0)

    (define stateful-a (make-stateful-cell a))
    (test-equal? "a is called twice; Once for discovery and once for initialization."
                 (a-count) 2)

    (define-values (b b-count) (with-call-counter (λ _ 1)))
    (define stateful-b (make-stateful-cell b))
    (define-values (c c-count) (with-call-counter (λ _ (+ (stateful-a) (stateful-b)))))
    (define stateful-c (make-stateful-cell c))

    (define (check-counts expectation
                          before
                          expected-a-count
                          expected-b-count
                          expected-c-count)
      (before)
      (test-case expectation
        (test-equal? "a count matches" (a-count) expected-a-count)
        (test-equal? "b count matches" (b-count) expected-b-count)
        (test-equal? "c count matches" (c-count) expected-c-count)))

    (check-counts "Nothing was called an additional time because nothing changed."
                  stateful-c
                  2 2 2)
    (check-counts "Updating one dependency updates dependents"
                  (λ _
                    (stateful-a a)
                    (stateful-c))
                  3 2 3))

  (test-case "Dependencies are detected only when called during discovery."
    ; Ported example from https://github.com/MaiaVictor/PureState/issues/4
    (define x (make-stateful-cell #t))
    (define y (make-stateful-cell 2))

    ; y won't be marked as a dependency of z
    ; because (y) does not evaluate during discovery.
    (define z (make-stateful-cell (λ _ (if (x) 1 (y)))))

    ; (y) does evaluate here.
    (x #f)
    (check-equal? (z) 2)

    (y 3) ; <-- The problem is that this won't update z because it doesn't know about z.
    (check-equal? (z) 2))


  (test-case "Explicit dependencies skip discovery and address blind spots."
    (define x (make-stateful-cell #t))
    (define y (make-stateful-cell 2))
    (define z (make-stateful-cell #:dependencies (list x y)
                 (λ _
                   (when (discovery-phase?) (error "should not get here"))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3) ; <-- This will update z because dependencies are listed.
    (check-equal? (z) 3))


  (test-case "Explicit dependencies can be combined with discovery."
    (define x (make-stateful-cell #t))
    (define y (make-stateful-cell 2))
    (define z (make-stateful-cell (λ _
                   (when (discovery-phase?)
                     (values (x) (y)))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3)
    (check-equal? (z) 3))

  (test-case "Explicit dependencies can be declared using stateful-cell"
    (define x (stateful-cell #t))
    (define y (stateful-cell 2))
    (define z (stateful-cell #:dependency x
                             #:dependency y
                             (when (discovery-phase?) (error "should not get here"))
                             (if (x) 1 (y))))
    (x #f)
    (check-equal? (z) 2)
    (y 3)
    (check-equal? (z) 3))

  (test-case "All cells affected by an update apply exactly once."
    (define counter 0)
    (define x (stateful-cell 1))
    (define y (stateful-cell (x)))
    (define z (stateful-cell (x) (y) (set! counter (add1 counter))))
    (check-equal? counter 2)
    (x 2)
    (check-equal? counter 3))

  (test-case "You can dynamically access a cell's current value"
    (define call-count 0)
    (define x
      (stateful-cell
       (set! call-count (add1 call-count))
       (if (< call-count 2)
           (test-eq? "A cell's value is undefined during the discovery phase and initial compute."
                     undefined (current-cell-value))
           1)))

    (test-eq? "A symbol distinguishes between being in or out of a cell body"
              not-in-cell
              (current-cell-value))

    (define (spawn-useless-thread)
      (thread (λ _ (let loop () (loop)))))

    (define threads '())
    (define (add-thread! th)
      (set! threads (cons th threads))
      th)

    (define %signal (stateful-cell 0))
    (define %spawner
      (stateful-cell #:dependency %signal
                     (when (thread? (current-cell-value))
                       (kill-thread (current-cell-value)))
                     (add-thread! (spawn-useless-thread))))

    ; Spawns some threads.
    (for ([i (in-range 10)]) (%signal i))

    (test-equal? "Cells can clean up their old values" 1 (count thread-running? threads))
    (void (map kill-thread threads))))
