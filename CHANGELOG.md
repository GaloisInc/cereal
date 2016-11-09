
0.5.4.0
=======

* Allow building with older versions of GHC (thanks to Ryan Scott!)
* Additional putters for ints (thanks to Andrew Martin!)

0.5.2.0
======

* Implement the AMP recommended refactoring for the Functor/Applicative/Monad
  hierarchy for Get and PutM (thanks to Herbert Valerio Riedel!)
* Unconditionally support GHC generics (thanks to Eric Mertens!)
* Split the GSerialize class in two, to deal with a GHC bug (thanks Austin Seipp!)
* No longer use Enum in the Serialize instance for Bool (thanks Francesco Mazzoli!)

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
