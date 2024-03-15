{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module GetTests (tests) where

import           Control.Applicative
import           Control.Monad
import           Data.Word
import           Data.Function
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import           Data.Serialize.Get
import           Test.Framework (Test(),testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, (@=?), assertFailure)
import           Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck as QC
import Data.List (isInfixOf)
import Debug.Trace


-- Data to express Get parser to generate
data GetD
  = Get8
  | Eof
  | Get16be
  | Get32be
  | Get64be
  | Get16le
  | Get32le
  | Get64le
  | GetD  :*>   GetD
  | GetD  :<|>  GetD
  | LookAhead GetD
  | Skip Int
  deriving Show

-- Get parser generator
buildGet :: GetD -> Get ()
buildGet = d  where
  d Get8           =  getWord8    *> pure ()
  d Eof            =  guard =<< isEmpty
  d Get16be        =  getWord16be *> pure ()
  d Get32be        =  getWord32be *> pure ()
  d Get64be        =  getWord64be *> pure ()
  d Get16le        =  getWord16le *> pure ()
  d Get32le        =  getWord32le *> pure ()
  d Get64le        =  getWord64le *> pure ()
  d (x :*>  y)     =  d x *>  d y
  d (x :<|> y)     =  d x <|> d y
  d (LookAhead x)  =  lookAhead $ d x
  d (Skip i)       =  skip i

-- Randomly generate parser
genGetD :: Gen GetD
genGetD =
    oneof $
    [ pure g
    | g <- [ Get8, Eof
           , Get16be, Get32be, Get64be
           , Get16le, Get32le, Get64le
           ]
    ] ++
    [ (:*>)     <$> genGetD <*> genGetD
    , (:<|>)    <$> genGetD <*> genGetD
    , LookAhead <$> genGetD
    , Skip      <$> choose (0, 10)
    ]

instance Arbitrary GetD where
  arbitrary = genGetD

instance Arbitrary (Get ()) where
  arbitrary = buildGet <$> genGetD

newtype R a =
  R { unR :: Either String a }
  deriving Show


-- Ignore equality of error message string
instance Eq a => Eq (R a) where
  (==)  =  (==) `on` either (const Nothing) Just . unR

data Chunks = Chunks [[Word8]] deriving (Eq, Show)

mkChunks :: Word -> Chunks
mkChunks n = Chunks . take (fromIntegral n) $ cycle [ [x] | x <- [0 .. 255] ]

instance Arbitrary Chunks where
  arbitrary = mkChunks <$> choose (0, 512)


testLength :: Word
testLength = 255

-- Equality between strict and lazy parsing
eqStrictLazy :: GetD -> Property
eqStrictLazy getD =
  conjoin
  [ counterexample (show in0) $ R (runGet parser sb) == R (runGetLazy parser lb)
  | n <- [0 .. testLength]
  , let Chunks in0 = mkChunks n
        lb = LB.fromChunks [ BS.pack c | c <- in0 ]
        sb = BS.pack $ concat in0
  ]
  where
    parser = buildGet getD

-- Remaining length equality between strict and lazy parsing
remainingStrictLazy :: GetD -> Property
remainingStrictLazy getD =
  conjoin
  [ counterexample (show in0) $ R (runGet parser sb) == R (runGetLazy parser lb)
  | n <- [0 .. testLength]
  , let Chunks in0 = mkChunks n
        lb = LB.fromChunks [ BS.pack c | c <- in0 ]
        sb = BS.pack $ concat in0
  ]
  where
    parser = buildGet getD *> remaining

isEmpty2 :: Get Bool
isEmpty2 = do
  lookAhead getWord8 *> pure False
  <|>
  pure True

-- Compare with chunks
(==~) :: (Eq a, Show a) => Get a -> Get a -> Property
p1 ==~ p2 =
  conjoin
  [ let rl = runGetLazy p1 s
        rr = runGetLazy p2 s
    in counterexample (show (in0, n, s, rl, rr)) $ R rl == R rr
  | n <- [0 .. testLength]
  , let Chunks in0 = mkChunks n
        s = LB.fromChunks [ BS.pack c | c <- in0 ]
  ]

(==!) :: Eq a => Get a -> Get a -> Property
p1 ==! p2 =
  conjoin
  [ counterexample (show s) $ R (runGet p1 s) == R (runGet p2 s)
  | n <- [0 .. testLength]
  , let Chunks in0 = mkChunks n
        s = BS.pack $ concat in0
  ]

infix 2 ==~, ==!

-- Equality between two eof definition - lazy
eqEof :: GetD -> Property
eqEof getD =
    x *> isEmpty ==~ x *> isEmpty2
  where
    x = buildGet getD

-- Equality between two eof definition - strict
eqEof' :: GetD -> Property
eqEof' getD =
    x *> isEmpty ==! x *> isEmpty2
  where
    x = buildGet getD


monadIdL :: GetD -> Property
monadIdL getD =
    (return () >>= const x) ==~ x
  where
    x = buildGet getD

monadIdL' :: GetD -> Property
monadIdL' getD =
    (return () >>= const x) ==! x
  where
    x = buildGet getD

monadIdR :: GetD -> Property
monadIdR getD =
    (x >>= return) ==~ x
  where
    x = buildGet getD

monadIdR' :: GetD -> Property
monadIdR' getD =
    (x >>= return) ==! x
  where
    x = buildGet getD

monadAssoc :: GetD -> GetD -> GetD -> Property
monadAssoc p1 p2 p3 =
    (x >> (y >> z)) ==~ (x >> y >> z)
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

monadAssoc' :: GetD -> GetD -> GetD -> Property
monadAssoc' p1 p2 p3 =
    (x >> (y >> z)) ==! (x >> y >> z)
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

alterIdL :: GetD -> Property
alterIdL getD =
    empty <|> x ==~ x
  where
    x = buildGet getD

alterIdL' :: GetD -> Property
alterIdL' getD =
    empty <|> x ==! x
  where
    x = buildGet getD

alterIdR :: GetD -> Property
alterIdR getD =
    x <|> empty ==~ x
  where
    x = buildGet getD

alterIdR' :: GetD -> Property
alterIdR' getD =
    x <|> empty ==! x
  where
    x = buildGet getD

alterAssoc :: GetD -> GetD -> GetD -> Property
alterAssoc p1 p2 p3 =
    x <|> y <|> z ==~ x <|> (y <|> z)
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

alterAssoc' :: GetD -> GetD -> GetD -> Property
alterAssoc' p1 p2 p3 =
    x <|> y <|> z ==! x <|> (y <|> z)
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

alterDistr :: GetD -> GetD -> GetD -> Property
alterDistr p1 p2 p3 =
    x *> (y <|> z) ==~ x *> y <|> x *> z
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

alterDistr' :: GetD -> GetD -> GetD -> Property
alterDistr' p1 p2 p3 =
    x *> (y <|> z) ==! x *> y <|> x *> z
  where
    x = buildGet p1
    y = buildGet p2
    z = buildGet p3

isolateLazyIsIncremental :: Assertion
isolateLazyIsIncremental = go (runGetPartial parser $ BS.replicate 11 0)
  where
    parser :: Get ()
    parser = isolateLazy 100 $ do
      skip 10
      fail failStr
      pure ()

    failStr :: String
    failStr = "no thanks"

    go :: Result () -> IO ()
    go r = case r of
      Done () _ -> assertFailure "Impossible"
      Fail failure _ -> unless (failStr `isInfixOf` failure) $ assertFailure "Wrong error!"
      Partial cont -> assertFailure "Asked for more input!"

isolateLazyLeavesRemainingBytes :: Assertion
isolateLazyLeavesRemainingBytes = go (runGetPartial parser $ BS.replicate 11 0)
  where
    parser :: Get ()
    parser = isolateLazy 100 $ do
      skip 10
      fail failStr
      pure ()

    failStr :: String
    failStr = "no thanks"

    go :: Result () -> IO ()
    go r = case r of
      Done () _ -> assertFailure "Impossible"
      Fail failure _ -> unless (failStr `isInfixOf` failure) $ assertFailure "Wrong error!"
      Partial cont -> assertFailure "Asked for more input!"

instance Arbitrary LB.ByteString where
  arbitrary = LB.fromChunks . pure . BS.pack <$> arbitrary

newtype IsolationRes a = IRes (Either String a)
  deriving Show

-- Sometimes lazy and strict isolations return different errors,
-- eg. when EOF is called before the end of an isolation which isn't prodided
-- enough input.
-- Strict sees it as a lack of bytes, Lazy sees it as a guard failure ("empty").
instance Eq a => Eq (IsolationRes a) where
  IRes a == IRes b = case (a, b) of
    (Left e1, Left e2) -> e1 == e2 || errsEqAsymmetric e1 e2 || errsEqAsymmetric e2 e1
    _ -> a == b
    where
      errsEqAsymmetric e1 e2 = "too few bytes" `isInfixOf` e1 && "empty" `isInfixOf` e2

isolateAndIsolateLazy :: Int -> GetD -> LB.ByteString -> Property
isolateAndIsolateLazy n parser' bs
  = IRes (runGetLazy (isolate n parser) bs) === IRes (runGetLazy (isolateLazy n parser) bs)
  where
    parser = buildGet parser'

isolateIsNotIncremental :: Assertion
isolateIsNotIncremental = go (runGetPartial parser $ BS.replicate 11 0)
  where
    parser :: Get ()
    parser = isolate 100 $ do
      skip 10
      fail failStr
      pure ()

    failStr :: String
    failStr = "no thanks"

    go :: Result () -> IO ()
    go r = case r of
      Done () _ -> assertFailure "Impossible"
      Fail failure _ -> assertFailure $ "Strict isolate was incremental: " <> failure
      Partial cont -> pure ()

-- Checks return values, leftovers, fails for continuations
assertResultsMatch :: Eq a => Result a -> (Maybe a, BS.ByteString) -> Assertion
assertResultsMatch r1 r2 = case (r1, r2) of
  (Partial _, _) -> assertFailure "Continuation received"
  (Done a1 bs1, (Just a2, bs2)) -> do
    unless (a1 == a2) $ assertFailure "Result mismatch"
    unless (bs1 == bs2) $ assertFailure $ "Success leftover mismatch: " ++ show (bs1, bs2)
  (Fail msg1 bs1, (Nothing, bs2)) ->
    unless (bs1 == bs2) $ assertFailure $ "Failure leftovers mismatch: " ++ show (bs1, bs2)
  _ -> assertFailure "Different result types"

isolateLazyDeterminesLeftovers :: Assertion
isolateLazyDeterminesLeftovers = do
  assertResultsMatch (runGetPartial (isolateLazy 1 getWord8) "123") (Just $ toEnum $ fromEnum '1', "23")
  assertResultsMatch (runGetPartial (isolateLazy 2 getWord8) "123") (Nothing, "3")
  -- Note(414owen): I don't think this is the ideal behaviour, but it's the existing behaviour, so
  -- I'll at least check that isolateLazy matches the behaviour of isolate...
  assertResultsMatch (runGetPartial (isolate 2 $ fail "no thanks" *> pure ()) "123") (Nothing, "12")
  assertResultsMatch (runGetPartial (isolateLazy 2 $ fail "no thanks" *> pure ()) "123") (Nothing, "12")

tests :: Test
tests  = testGroup "GetTests"
  [ testProperty "lazy   - monad left id"          monadIdL
  , testProperty "strict - monad left id"          monadIdL'
  , testProperty "lazy   - monad right id"         monadIdR
  , testProperty "strict - monad right id"         monadIdR'
  , testProperty "lazy   - monad assoc"            monadAssoc
  , testProperty "strict - monad assoc"            monadAssoc'
  , testProperty "strict lazy - equality"          eqStrictLazy
  , testProperty "strict lazy - remaining equality"remainingStrictLazy
  , testProperty "lazy   - two eof"                eqEof
  , testProperty "strict - two eof"                eqEof'
  , testProperty "lazy   - alternative left Id"    alterIdL
  , testProperty "strict - alternative left Id"    alterIdL'
  , testProperty "lazy   - alternative right Id"   alterIdR
  , testProperty "strict - alternative right Id"   alterIdR'
  , testProperty "lazy   - alternative assoc"      alterAssoc
  , testProperty "strict - alternative assoc"      alterAssoc'
  , testProperty "lazy   - alternative distr"      alterDistr
  , testProperty "strict - alternative distr"      alterDistr'
  , testCase     "isolate is not incremental"      isolateIsNotIncremental
  , testCase     "isolateLazy is incremental"      isolateLazyIsIncremental
  , testProperty "isolations are equivalent"       isolateAndIsolateLazy
  , testCase     "isolateLazy determines leftovers" isolateLazyDeterminesLeftovers
  ]
