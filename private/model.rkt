#lang racket/base

(require racket/class
         racket/list
         racket/set
         graph)
(provide (all-defined-out))

(define state-graph%
  (class object% (super-new)
    (define responsibility-graph (directed-graph null))
    (define tsorted null)
    (define dirty #f)

    (define/public (clone-graph)
      (graph-copy responsibility-graph))

    (define/public (get-graph/unsafe)
      responsibility-graph)

    (define/public (set-dependencies! dependent dependencies)
      (if (= (length dependencies) 0)
          (record-cell! dependent)
          (begin
            (for ([dependency (in-list dependencies)])
              (add-directed-edge! responsibility-graph dependency dependent))
            (set! dirty #t))))

    (define/public (get-update-schedule v)
      (define dependents (apply seteq (get-neighbors responsibility-graph v)))
      (filter (λ (maybe-dep) (set-member? dependents maybe-dep))
              tsorted))

    (define/public (refresh-graph-info!)
      (when dirty
        (unless (dag? responsibility-graph)
          (error 'stateful-cell "dependency cycle detected"))
        (set! tsorted (tsort responsibility-graph))
        (set! dirty #f)))

    (define/private (record-cell! n)
      (add-vertex! responsibility-graph n)
      (set! dirty #t))))
