#lang racket/base

(require racket/undefined)
(provide stateful-cell % ※
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

(define (refresh! n [steps-walked 0])
  (when (> steps-walked (current-hard-walk-limit))
    (error 'stateful-cell "Exceeded hard walk limit of ~a. Your dependencies might be circular."
           (current-hard-walk-limit)))
  (set-node-value! n
                   (apply (node-compute n)
                          (map node-value (node-dependencies n))))
  (for ([dependent (node-dependents n)])
    (refresh! dependent (add1 steps-walked))))

(define (stateful-cell compute #:dependencies [explicit-dependencies '()])
  (define n (node '() '() (normalize compute) undefined))

  (define dependencies
    (if (> (length explicit-dependencies) 0)
        explicit-dependencies
        (begin
          (set! captured-deps '())
          (parameterize ([capture? #t])
            ((node-compute n)))
          (set-node-dependencies! n captured-deps)
          captured-deps)))

  (for ([dependency dependencies])
    (set-node-dependents! dependency
                          (cons n (node-dependents dependency))))
  (refresh! n)
  n)

(define ※ stateful-cell)
(define % ※)

(module+ test
  (require rackunit)

  (define (with-call-counter proc)
    (let ([num-calls 0])
      (values (λ A
                (set! num-calls (add1 num-calls))
                (apply proc A))
              (λ _ num-calls))))

  (test-true "Can identify instances"
    (stateful-cell? (% 1)))

  (test-case "State graph procedures represent data or other procedures trivially"
    (let ([x 10])
      (check-equal? ((% x)) x)
      (check-equal? ((% (λ _ x))) x)))

  (test-case "Racket values can form dependency relationships"
    (let* ([a (% 1)]
           [b (% 1)]
           [c (% (λ _ (+ (a) (b))))])
      (check-equal? (c) 2)
      (a 2)
      (check-equal? (c) 3)))

  (test-case "Dependencies are not recomputed unless necessary"
    (define-values (a a-count) (with-call-counter (λ _ 1)))

    ; Sanity check the counter
    (test-equal? "a was not yet called" (a-count) 0)

    (define stateful-a (% a))
    (test-equal? "a is called twice; Once for discovery and once for initialization."
                 (a-count) 2)

    (define-values (b b-count) (with-call-counter (λ _ 1)))
    (define stateful-b (% b))
    (define-values (c c-count) (with-call-counter (λ _ (+ (stateful-a) (stateful-b)))))
    (define stateful-c (% c))

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
    (define x (% #t))
    (define y (% 2))

    ; y won't be marked as a dependency of z
    ; because (y) does not evaluate during discovery.
    (define z (% (λ _ (if (x) 1 (y)))))

    ; (y) does evaluate here.
    (x #f)
    (check-equal? (z) 2)

    (y 3) ; <-- The problem is that this won't update z because it doesn't know about z.
    (check-equal? (z) 2))


  (test-case "Explicit dependencies skip discovery and address blind spots."
    (define x (% #t))
    (define y (% 2))
    (define z (% #:dependencies (list x y)
                 (λ _
                   (when (capture?) (error "should not get here"))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3) ; <-- This will update z because dependencies are listed.
    (check-equal? (z) 3))


  (test-case "Explicit dependencies can be combined with discovery."
    (define x (% #t))
    (define y (% 2))
    (define z (% (λ _
                   (when (capture?)
                     (values (x) (y)))
                   (if (x) 1 (y)))))
    (x #f)
    (check-equal? (z) 2)
    (y 3)
    (check-equal? (z) 3)))
