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
Data.Faceted
  faceted value simple =  Faceted (\x -> x > 0) (1, 0) (which is equivalent with < (x > 0) ? 1 : 0>)
    - observation with context 1 should be 1
    - observation with context 0 should be 0

  Functor: ((*3) `fmap` simple) should be equivalent with < (x > 0) ? 1*3 : 0*3>
    - observation with context 1 should be 3
    - observation with context 0 should be 0

  Applicative: ((+) <$> simple <*> simple) should  be equivalent with < (x > 0) ? 1+1 : 0+0>
    - observation with context 1 should be 3
    - observation with context 0 should be 0

  Applicative Do:
         do a <- simple
            b <- simple
            return a+b
        should be equivalent with
          < (x > 0) ? 1+1 : 0+0>
    - observation with context 1 should be 3
    - observation with context 0 should be 0

  Bind: simple >>= (\v -> Faceted (\y -> y < 2) (v + 1, v + 3))
        shoule be equivalent with
          < (x > 0) ? < (y < 2) ? 1+1, 1+3> : < (y < 2) ? 0+1, 0+3> >
    - observation with context 2 should be 4
    - observation with context 1 should be 2
    - observation with context 0 should be 0

  Do Syntax:
        do a <- simple
           b <- simple
           Faceted (\y -> y < 2) (a*b, a+b)
        shoule be equivalent with
          < (x > 0) ? < (y < 2) ? 1*1, 1+1> : < (y < 2) ? 0*0, 0-0> >
    - observation with context 2 should be 2
    - observation with context 1 should be 1
    - observation with context 0 should be 0

Finished in 0.0011 seconds
14 examples, 0 failures
```

Documents
==
haddoc is [here](http://everpeace.github.io/faceted-values/faceted/index.html).
