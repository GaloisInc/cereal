
Future
======

* Implement the AMP recommended refactoring for the Functor/Applicative/Monad
  hierarchy for Get and PutM (thanks to Herbert Valerio Riedel!)
* Unconditionally support GHC generics (thanks to Eric Mertens!)

0.5.1.0
=======

* Re-enable GHC.Generics support which was accidentally removed in 0.5.0.0

0.5.0.0
=======

* Switch to using the builder provided by the `ByteString` package
* Change the encoding of Float and Double with the Serialize class to use the
  `Data.Serialize.IEEE754` module
* Add support for encoding and decoding `ShortByteString`
* New and improved test suite thanks to Kei Hibino
* Fix two bugs involving the `lookAhead` combinator and partial chunks.
