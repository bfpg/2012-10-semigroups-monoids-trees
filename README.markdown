
# Semigroups, Monoids, Trees

A talk given at the [Brisbane Functional Programming Group][bfpg], on [23
October 2012][talk].

[bfpg]: http://www.bfpg.org/
[talk]: http://www.bfpg.org/events/84176892/

## Synopsis

Semigroups and monoids are really simple abstractions, but very common and
surprisingly useful. In this talk, we'll explain what they are, and how they're
related, with lots of examples. But we want more than just taxonomy from our
abstractions, so we'll also show how semigroups and monoids allow us to
generalise a range of algorithms. In particular, we'll show to exploit the
relationship between semigroups and tree-shaped computations to make an
amazingly flexible data structure.

This talk is mostly at an intermediate level, but we'll attempt to explain
concepts as we go, and will include an introduction to Haskell type classes.
There will hopefully be just enough mind expansion to keep the more advanced
folk entertained, too.

## Warning

The Haskell code shown in this talk is _not_ consistent with the libraries
distributed with the [Haskell Platform][platform], or available on [Hackage][].
I made a number of changes, including simplifications to improve the flow of
the talk, and refinements to better illustrate the monoid and semigroup
abstractions and the relationships between them.

[platform]: http://www.haskell.org/platform/
[Hackage]: http://hackage.haskell.org/packages/hackage.html

The major change was to introduce a separate Semigroup class, and to make the
Monoid class a subclass of Semigroup. Changes to other classes and instances
generally followed from this change.

## Acknowledgements

The content this talk was largely inspired by, and derived from:

- the standard [Monoid][] and [Foldable][] libraries,
- the amazing [FingerTree][] library, and the [paper][] which describes it,
- a [blog post][blog] by [Heinrich Apfelmus][apfelmus].

[Monoid]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html
[Foldable]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html
[FingerTree]: http://hackage.haskell.org/package/fingertree
[paper]: http://www.soi.city.ac.uk/~ross/papers/FingerTree.html
[blog]: http://apfelmus.nfshost.com/articles/monoid-fingertree.html
[apfelmus]: http://apfelmus.nfshost.com/

There are lots of other libraries on [Hackage][] which make use of monoids,
semigroups and foldable structures.

