#lang racket/base

(require racket/undefined
         racket/list
         (for-syntax racket/base
                     syntax/parse))

(provide (rename-out [stateful-cell %]
                     [stateful-cell ※])
         stateful-cell
         make-stateful-cell
         stateful-cell-dependencies
         stateful-cell-dependents
         current-hard-walk-limit
         stateful-cell?
         discovery-phase?)

;; Based on https://github.com/MaiaVictor/PureState
;; See tests and docs for usage details.

(define captured-deps '())
(define capture? (make-parameter #f))

; Do not allow users to change parameter.
(define (discovery-phase?) (capture?))

(define current-hard-walk-limit (make-parameter 10000))

(define (normalize compute)
  (if (procedure? compute)
      compute
      (λ _ compute)))

(struct node (dependencies dependents compute value)
  #:mutable #:property prop:procedure
  (λ (self [new-compute undefined])
     (when (capture?)
       (set! captured-deps (cons self captured-deps)))
     (unless (eq? new-compute undefined)
       (set-node-compute! self (normalize new-compute))
       (refresh! self))
     (node-value self)))

(define stateful-cell? (procedure-rename node? 'stateful-cell?))
(define stateful-cell-dependencies (procedure-rename node-dependencies 'stateful-cell-dependencies))
(define stateful-cell-dependents (procedure-rename node-dependents 'stateful-cell-dependents))

(define (refresh! n [steps-walked 0])
  (when (> steps-walked (current-hard-walk-limit))
    (error 'stateful-cell "Exceeded hard walk limit of ~a. Your dependencies might be circular."
           (current-hard-walk-limit)))
  (set-node-value! n ((node-compute n)))
  (for ([dependent (node-dependents n)])
    (refresh! dependent (add1 steps-walked))))


(define (make-stateful-cell compute #:dependencies [explicit-dependencies '()])
  (define n (node '() '() (normalize compute) undefined))
  (define dependencies
    (if (> (length explicit-dependencies) 0)
        explicit-dependencies
        (begin
          (set! captured-deps '())
          (parameterize ([capture? #t])
            ((node-compute n)))
          (set-node-dependencies! n (remove-duplicates captured-deps eq?))
          captured-deps)))
  (for ([dependency dependencies])
    (set-node-dependents! dependency
                          (remove-duplicates
                           (cons n (node-dependents dependency))
                           eq?)))
  (refresh! n)
  n)

(define-syntax (stateful-cell stx)
  (define-splicing-syntax-class explicit-dependency
    #:description "an explicit dependency binding"
    (pattern (~seq #:dependency u:id e:id)))

  (syntax-parse stx #:context #'stateful-cell
    [(stateful-cell d:explicit-dependency ...+ body:expr ...+)
     #'(make-stateful-cell #:dependencies (list d.e ...)
                           (λ () (let ([d.u (d.e)] ...) body ...)))]

    [(stateful-cell body:expr ...+)
     #'(make-stateful-cell (λ () body ...))]

    [(stateful-cell d:explicit-dependency ...)
     (raise-syntax-error 'stateful-cell
                         "Expected body after dependency bindings"
                         stx)]))


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
                   (when (capture?) (error "should not get here"))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3) ; <-- This will update z because dependencies are listed.
    (check-equal? (z) 3))


  (test-case "Explicit dependencies can be combined with discovery."
    (define x (make-stateful-cell #t))
    (define y (make-stateful-cell 2))
    (define z (make-stateful-cell (λ _
                   (when (capture?)
                     (values (x) (y)))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3)
    (check-equal? (z) 3))

  (test-case "Explicit dependencies can be bound to new names using stateful-cell"
    (define x (stateful-cell #t))
    (define y (stateful-cell 2))
    (define z (stateful-cell #:dependency a x
                             #:dependency b y
                             (when (capture?) (error "should not get here"))
                             (if a 1 b)))
    (x #f)
    (check-equal? (z) 2)
    (y 3)
    (check-equal? (z) 3))

  (test-case "Dependencies and dependents are available to those who ask"
    (define a (stateful-cell 1))
    (define b (stateful-cell (a)))
    (define c (stateful-cell (b)))
    (define (check-edges n dependencies dependents)
      (check-equal? (stateful-cell-dependencies n)
                    dependencies)
      (check-equal? (stateful-cell-dependents n)
                    dependents))
    (check-edges a (list) (list b))
    (check-edges b (list a) (list c))
    (check-edges c (list b) (list)))

  (test-case "Each dependency and dependent appears at most once"
    (define a (stateful-cell 1))
    (define count 0)
    (define b (stateful-cell (set! count (add1 count)) (a) (a) (a)))
    (check-equal? count 2) ; 2 calls expected; Once for discovery, once for compute
    (a 2)

    ; If the b appears as a dependent multiple times for a, it will be called more
    ; than once during an update. Make sure it's only called once more here.
    (check-equal? count 3)
    (check-equal? (length (filter (λ (d) (eq? d b))(stateful-cell-dependents a))) 1)
    (check-equal? (length (filter (λ (d) (eq? d a))(stateful-cell-dependencies b))) 1)))
