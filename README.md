[![](https://img.shields.io/badge/%E2%99%A5-Support%20Ethical%20Software-red)](https://sagegerard.com/subscribe.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/kinda-ferpy/index.html)

Kinda Ferpy is a restricted, yet convenient interface for functional reactive programming.
Signals/events and dependency relationships are implicit in how you write code, making
certain kinds of work super convenient and nice.

```
(require kinda-ferpy)

(define x (stateful-cell 1))
(define y (stateful-cell 1))
(define sum (stateful-cell (λ _ (+ (x) (y)))))

(displayln (sum)) ; 2
(y 8)
(displayln (sum)) ; 9
```

The implementation model is based on @MaiaVictor's [PureState][] library.

[PureState]: https://github.com/MaiaVictor/PureState
