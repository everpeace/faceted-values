Haskell Implementation of Faceted Values
----

Inspired by Thomas H. Austin and Cormac Flanagan, ["Multiple Facets for Dynamic Information Flow."](http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf)

> A _faceted value_ is a triple consisting _principal_ k and two values V<sub>H</sub>, V<sub>L</sub>, which write as:
>
> \< k ? V<sub>H</sub>, V<sub>L</sub> \>.
>
> Intuitively, this faceted value appeared as V<sub>H</sub> to private observers that can view k's private data, and as V<sub>L</sub> to other public observers.

As you can see, _faceted value_ can be a very strong primitive for supporting _privacy sensitive value_ in programming language level.

[Jeeves](http://jeeveslang.org), a programming language for automatically enforcing privacy policies, is also based _faceted values_.
