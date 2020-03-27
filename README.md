# linear-tests
This library provides [QuickCheck](https://hackage.haskell.org/package/QuickCheck) `Arbitrary` instances of data types in the [linear](http://hackage.haskell.org/package/linear) package.

The property tests in this package test both the newly defined `Arbitrary` instances as well as methods in the linear package itself. It's hard to distinguish between the two types of test so they are all just lumped together.

The current instances are just ones I (and hopefully others) find useful. Feel free to add your own and submit a PR.
