{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Test.QuickCheck as QC


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
(==~) :: Eq a => Get a -> Get a -> Property
p1 ==~ p2 =
  conjoin
  [ counterexample (show in0) $ R (runGetLazy p1 s) == R (runGetLazy p2 s)
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
  ]
