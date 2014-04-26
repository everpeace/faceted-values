Haskell Implementation of Faceted Values [![Build Status](https://travis-ci.org/everpeace/faceted-values.svg?branch=master)](https://travis-ci.org/everpeace/faceted-values)
----
_Faceted values_ can be a very strong primitive for _privacy sensitive values_.

This implementation is inspired by Thomas H. Austin and Cormac Flanagan, ["Multiple Facets for Dynamic Information Flow."](http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf)

> A _faceted value_ is a triple consisting _principal_ k and two values V<sub>H</sub>, V<sub>L</sub>, which write as:
>
> \< k ? V<sub>H</sub>, V<sub>L</sub> \>.
>
> Intuitively, this faceted value appeared as V<sub>H</sub> to private observers that can view k's private data, and as V<sub>L</sub> to other public observers.

[Jeeves](http://jeeveslang.org), a programming language for automatically enforcing privacy policies, is also based _faceted values_.

How to build
==
```
$ cabal update
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal build
# for testing.
$ cabal test
```
How to use
==
please see [test](https://github.com/everpeace/faceted-values/blob/master/test/Data/FacetedSpec.hs).

or, test log could be useful to understand how-to easily.
```
Test suite spec: RUNNING...

Data.Faceted
  facete value can be declared in intuitive manner "(\x -> x > 0) ? 1 .: 0)"
    - its observation with context 0 should be 0.
    - its observation with context 1 should be 1.

  use (??) for nested facete value "(\x -> x <= 2) ?? ((\x -> x < 2) ? 1 .: 2) .: ((\x -> x < 4 ) ? 3 .: 4)"
    - its observation with context 1 should be 1.
    - its observation with context 2 should be 2.
    - its observation with context 3 should be 3.
    - its observation with context 4 should be 4.

  Functor: ((*3) `fmap` (\x -> x > 0) ? 1 .: 0)) should be equivalent with < (x > 0) ? 1*3 : 0*3>.
    - observation with context 0 should be 0.
    - observation with context 1 should be 3.

  Applicative: ((+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8))
  This computation adds two faceted values. So in this case, 4 patterns of results can be observed.
  The result should be equivalent with
    < (0 < x < 3) ? < (1 < x < 4) ? 1+4 : 1+8 >
                  : < (1 < x < 4) ? 2+4 : 2+8 >>
    - observation with context 1 should be 9  (= 1 + 8).
    - observation with context 2 should be 5  (= 1 + 4).
    - observation with context 3 should be 6  (= 2 + 4).
    - observation with context 4 should be 10 (= 2 + 8).

  Applicative Do:
   do a <- ((\x -> 0 < x && x < 3) ? 1 .: 2)
      b <- ((\x -> 1 < x && x < 4) ? 4 .: 8)
      return a + b
  should be equivalent with above.

    - observation with context 1 should be 9  (= 1 + 8).
    - observation with context 2 should be 5  (= 1 + 4).
    - observation with context 3 should be 6  (= 2 + 4).
    - observation with context 4 should be 10 (= 2 + 8).

  Bind: ((\x -> 0 < x && x < 3) ? 1 .: 2) >>= (\v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))
  shoule be equivalent with
    < (0 < x < 3) ? < (1 < x < 4) ? 1+4 : 1+8 >
                  : < (1 < x < 4) ? 2+4 : 2+8 >>
    - observation with context 1 should be 9  (= 1 + 8).
    - observation with context 2 should be 5  (= 1 + 4).
    - observation with context 3 should be 6  (= 2 + 4).
    - observation with context 4 should be 10 (= 2 + 8).

  Do Syntax:
  do a <- ((\x -> 0 < x && x < 3) ? 1 .: 2)
     b <- ((\x -> 1 < x && x < 4) ? (a+4) .: (a+8))
     (\y -> y < 3) ? (10*b) .: (100*b)
  shoule be equivalent with
    < (0 < x < 3) ? < (1 < x < 4) ? <(y < 3)? 10*(1+4) : 100*(1+4)>
                                  : <(y < 3)? 10*(2+8) : 100*(2+8)>>
                  : < (1 < x < 4) ? <(y < 3)? 10*(2+4) : 100*(2+4)>
                                  : <(y < 3)? 10*(2+8) : 100*(2+8)>>
    - observation with context 1 should be 90   (= 10 * (1 + 8)).
    - observation with context 2 should be 50   (= 10 * (1 + 4)).
    - observation with context 3 should be 600  (= 100 * (2 + 4)).
    - observation with context 4 should be 1000 (= 100 * (2 + 8)).

Finished in 0.0016 seconds
24 examples, 0 failures
Test suite spec: PASS
Test suite logged to: dist/test/faceted-0.1.0.0-spec.log
```

Documents
==
haddoc is [here](http://everpeace.github.io/faceted-values/faceted/index.html).
