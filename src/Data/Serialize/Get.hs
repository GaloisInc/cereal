{-# LANGUAGE CPP        #-}
{-# LANGUAGE MagicHash  #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Serialize.Get
-- Copyright   : Lennart Kolmodin, Galois Inc. 2009
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Trevor Elliott <trevor@galois.com>
-- Stability   :
-- Portability :
--
-- The Get monad. A monad for efficiently building structures from
-- strict ByteStrings
--
-----------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

module Data.Serialize.Get (

    -- * The Get type
      Get
    , runGet
    , runGetLazy
    , runGetState
    , runGetLazyState

    -- ** Incremental interface
    , Result(..)
    , runGetPartial
    , runGetChunk

    -- * Parsing
    , ensure
    , isolate
    , label
    , skip
    , uncheckedSkip
    , lookAhead
    , lookAheadM
    , lookAheadE
    , uncheckedLookAhead

    -- * Utility
    , getBytes
    , remaining
    , isEmpty

    -- * Parsing particular types
    , getWord8
    , getInt8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString

#if MIN_VERSION_bytestring(0,10,4)
    , getShortByteString
#endif

    -- ** Big-endian reads
    , getWord16be
    , getWord32be
    , getWord64be
    , getInt16be
    , getInt32be
    , getInt64be

    -- ** Little-endian reads
    , getWord16le
    , getWord32le
    , getWord64le
    , getInt16le
    , getInt32le
    , getInt64le

    -- ** Host-endian, unaligned reads
    , getWordhost
    , getWord16host
    , getWord32host
    , getWord64host

    -- ** Containers
    , getTwoOf
    , getListOf
    , getIArrayOf
    , getTreeOf
    , getSeqOf
    , getMapOf
    , getIntMapOf
    , getSetOf
    , getIntSetOf
    , getMaybeOf
    , getEitherOf
    , getNested
  ) where

import qualified Control.Applicative as A
import qualified Control.Monad as M
import Control.Monad (unless)
import Data.Array.IArray (IArray,listArray)
import Data.Ix (Ix)
import Data.List (intercalate)
import Data.Maybe (isNothing,fromMaybe)
import Foreign
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe   as B
import qualified Data.ByteString.Lazy     as L
import qualified Data.IntMap              as IntMap
import qualified Data.IntSet              as IntSet
import qualified Data.Map                 as Map
import qualified Data.Sequence            as Seq
import qualified Data.Set                 as Set
import qualified Data.Tree                as T


#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as BS
#endif


#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

-- | The result of a parse.
data Result r = Fail String B.ByteString
              -- ^ The parse failed. The 'String' is the
              --   message describing the error, if any.
              | Partial (B.ByteString -> Result r)
              -- ^ Supply this continuation with more input so that
              --   the parser can resume. To indicate that no more
              --   input is available, use an 'B.empty' string.
              | Done r B.ByteString
              -- ^ The parse succeeded.  The 'B.ByteString' is the
              --   input that had not yet been consumed (if any) when
              --   the parse succeeded.

instance Show r => Show (Result r) where
    show (Fail msg _) = "Fail " ++ show msg
    show (Partial _)  = "Partial _"
    show (Done r bs)  = "Done " ++ show r ++ " " ++ show bs

instance Functor Result where
    fmap _ (Fail msg rest) = Fail msg rest
    fmap f (Partial k)     = Partial (fmap f . k)
    fmap f (Done r bs)     = Done (f r) bs

-- | The Get monad is an Exception and State monad.
newtype Get a = Get
  { unGet :: forall r. Input -> Buffer -> More
                    -> Failure r -> Success a r
                    -> Result r }

type Input  = B.ByteString
type Buffer = Maybe B.ByteString

emptyBuffer :: Buffer
emptyBuffer  = Just B.empty

extendBuffer :: Buffer -> B.ByteString -> Buffer
extendBuffer buf chunk =
  do bs <- buf
     return $! bs `B.append` chunk
{-# INLINE extendBuffer #-}

append :: Buffer -> Buffer -> Buffer
append l r = B.append `fmap` l A.<*> r
{-# INLINE append #-}

bufferBytes :: Buffer -> B.ByteString
bufferBytes  = fromMaybe B.empty
{-# INLINE bufferBytes #-}

type Failure   r = Input -> Buffer -> More -> [String] -> String -> Result r
type Success a r = Input -> Buffer -> More -> a                  -> Result r

-- | Have we read all available input?
data More
  = Complete
  | Incomplete (Maybe Int)
    deriving (Eq)

moreLength :: More -> Int
moreLength m = case m of
  Complete      -> 0
  Incomplete mb -> fromMaybe 0 mb

instance Functor Get where
    fmap p m =        Get $ \ s0 b0 m0 kf ks ->
      unGet m s0 b0 m0 kf $ \ s1 b1 m1 a     -> ks s1 b1 m1 (p a)

instance A.Applicative Get where
    pure a = Get $ \ s0 b0 m0 _ ks -> ks s0 b0 m0 a
    {-# INLINE pure #-}

    f <*> x =         Get $ \ s0 b0 m0 kf ks ->
      unGet f s0 b0 m0 kf $ \ s1 b1 m1 g     ->
      unGet x s1 b1 m1 kf $ \ s2 b2 m2 y     -> ks s2 b2 m2 (g y)
    {-# INLINE (<*>) #-}

    m *> k =          Get $ \ s0 b0 m0 kf ks ->
      unGet m s0 b0 m0 kf $ \ s1 b1 m1 _     -> unGet k s1 b1 m1 kf ks
    {-# INLINE (*>) #-}

instance A.Alternative Get where
    empty = failDesc "empty"
    {-# INLINE empty #-}

    (<|>) = M.mplus
    {-# INLINE (<|>) #-}

-- Definition directly from Control.Monad.State.Strict
instance Monad Get where
    return = A.pure
    {-# INLINE return #-}

    m >>= g  =        Get $ \ s0 b0 m0 kf ks ->
      unGet m s0 b0 m0 kf $ \ s1 b1 m1 a     -> unGet (g a) s1 b1 m1 kf ks
    {-# INLINE (>>=) #-}

    (>>) = (A.*>)
    {-# INLINE (>>) #-}

    fail     = failDesc
    {-# INLINE fail #-}


instance M.MonadPlus Get where
    mzero     = failDesc "mzero"
    {-# INLINE mzero #-}

    mplus a b =
      Get $ \s0 b0 m0 kf ks ->
        let ks' s1 b1        = ks s1 (b0 `append` b1)
            kf' _  b1 m1     = kf (s0 `B.append` bufferBytes b1)
                                  (b0 `append` b1) m1
            try _  b1 m1 _ _ = unGet b (s0 `B.append` bufferBytes b1)
                                       b1 m1 kf' ks'
         in unGet a s0 emptyBuffer m0 try ks'
    {-# INLINE mplus #-}


------------------------------------------------------------------------

formatTrace :: [String] -> String
formatTrace [] = "Empty call stack"
formatTrace ls = "From:\t" ++ intercalate "\n\t" ls ++ "\n"

get :: Get B.ByteString
get  = Get (\s0 b0 m0 _ k -> k s0 b0 m0 s0)
{-# INLINE get #-}

put :: B.ByteString -> Get ()
put s = Get (\_ b0 m _ k -> k s b0 m ())
{-# INLINE put #-}

label :: String -> Get a -> Get a
label l m =
  Get $ \ s0 b0 m0 kf ks ->
    let kf' s1 b1 m1 ls = kf s1 b1 m1 (l:ls)
     in unGet m s0 b0 m0 kf' ks

finalK :: Success a a
finalK s _ _ a = Done a s

failK :: Failure a
failK s b _ ls msg =
  Fail (unlines [msg, formatTrace ls]) (s `B.append` bufferBytes b)

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGet :: Get a -> B.ByteString -> Either String a
runGet m str =
  case unGet m str Nothing Complete failK finalK of
    Fail i _  -> Left i
    Done a _  -> Right a
    Partial{} -> Left "Failed reading: Internal error: unexpected Partial."
{-# INLINE runGet #-}

-- | Run the get monad on a single chunk, providing an optional length for the
-- remaining, unseen input, with Nothing indicating that it's not clear how much
-- input is left.  For example, with a lazy ByteString, the optional length
-- represents the sum of the lengths of all remaining chunks.
runGetChunk :: Get a -> Maybe Int -> B.ByteString -> Result a
runGetChunk m mbLen str = unGet m str Nothing (Incomplete mbLen) failK finalK
{-# INLINE runGetChunk #-}

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runGetPartial :: Get a -> B.ByteString -> Result a
runGetPartial m = runGetChunk m Nothing
{-# INLINE runGetPartial #-}

-- | Run the Get monad applies a 'get'-based parser on the input
-- ByteString. Additional to the result of get it returns the number of
-- consumed bytes and the rest of the input.
runGetState :: Get a -> B.ByteString -> Int
            -> Either String (a, B.ByteString)
runGetState m str off = case runGetState' m str off of
  (Right a,bs) -> Right (a,bs)
  (Left i,_)   -> Left i
{-# INLINE runGetState #-}

-- | Run the Get monad applies a 'get'-based parser on the input
-- ByteString. Additional to the result of get it returns the number of
-- consumed bytes and the rest of the input, even in the event of a failure.
runGetState' :: Get a -> B.ByteString -> Int
             -> (Either String a, B.ByteString)
runGetState' m str off =
  case unGet m (B.drop off str) Nothing Complete failK finalK of
    Fail i bs -> (Left i,bs)
    Done a bs -> (Right a, bs)
    Partial{} -> (Left "Failed reading: Internal error: unexpected Partial.",B.empty)
{-# INLINE runGetState' #-}



-- Lazy Get --------------------------------------------------------------------

runGetLazy' :: Get a -> L.ByteString -> (Either String a,L.ByteString)
runGetLazy' m lstr =
  case L.toChunks lstr of
    [c]  -> wrapStrict (runGetState' m c       0)
    []   -> wrapStrict (runGetState' m B.empty 0)
    c:cs -> loop (runGetChunk m (Just (len - B.length c)) c) cs
  where
  len = fromIntegral (L.length lstr)

  wrapStrict (e,s) = (e,L.fromChunks [s])

  loop result chunks = case result of

    Fail str rest -> (Left str, L.fromChunks (rest : chunks))

    Partial k     -> case chunks of
                       c:cs -> loop (k c)       cs
                       []   -> loop (k B.empty) []

    Done r rest   -> (Right r, L.fromChunks (rest : chunks))
{-# INLINE runGetLazy' #-}

-- | Run the Get monad over a Lazy ByteString.  Note that this will not run the
-- Get parser lazily, but will operate on lazy ByteStrings.
runGetLazy :: Get a -> L.ByteString -> Either String a
runGetLazy m lstr = fst (runGetLazy' m lstr)
{-# INLINE runGetLazy #-}

-- | Run the Get monad over a Lazy ByteString.  Note that this does not run the
-- Get parser lazily, but will operate on lazy ByteStrings.
runGetLazyState :: Get a -> L.ByteString -> Either String (a,L.ByteString)
runGetLazyState m lstr = case runGetLazy' m lstr of
  (Right a,rest) -> Right (a,rest)
  (Left err,_)   -> Left err
{-# INLINE runGetLazyState #-}

------------------------------------------------------------------------

-- | If at least @n@ bytes of input are available, return the current
--   input, otherwise fail.
{-# INLINE ensure #-}
ensure :: Int -> Get B.ByteString
ensure n0 = n0 `seq` Get $ \ s0 b0 m0 kf ks -> let
    n' = n0 - B.length s0
    in if n' <= 0
        then ks s0 b0 m0 s0
        else getMore n' s0 [] b0 m0 kf ks
    where
        -- The "accumulate and concat" pattern here is important not to incur
        -- in quadratic behavior, see <https://github.com/GaloisInc/cereal/issues/48>

        finalInput s0 ss = B.concat (reverse (s0 : ss))
        finalBuffer b0 s0 ss = extendBuffer b0 (B.concat (reverse (init (s0 : ss))))

        getMore !n s0 ss b0 m0 kf ks = let
            tooFewBytes = let
                !s = finalInput s0 ss
                !b = finalBuffer b0 s0 ss
                in kf s b m0 ["demandInput"] "too few bytes"
            in case m0 of
                Complete -> tooFewBytes
                Incomplete mb -> Partial $ \s ->
                    if B.null s
                        then tooFewBytes
                        else let
                            !mb' = case mb of
                                Just l -> Just $! l - B.length s
                                Nothing -> Nothing
                            in checkIfEnough n s (s0 : ss) b0 (Incomplete mb') kf ks

        checkIfEnough !n s0 ss b0 m0 kf ks = let
            n' = n - B.length s0
            in if n' <= 0
                then let
                    !s = finalInput s0 ss
                    !b = finalBuffer b0 s0 ss
                    in ks s b m0 s
                else getMore n' s0 ss b0 m0 kf ks

-- | Isolate an action to operating within a fixed block of bytes.  The action
--   is required to consume all the bytes that it is isolated to.
isolate :: Int -> Get a -> Get a
isolate n m = do
  M.when (n < 0) (fail "Attempted to isolate a negative number of bytes")
  s <- ensure n
  let (s',rest) = B.splitAt n s
  put s'
  a    <- m
  used <- get
  unless (B.null used) (fail "not all bytes parsed in isolate")
  put rest
  return a

failDesc :: String -> Get a
failDesc err = do
    let msg = "Failed reading: " ++ err
    Get (\s0 b0 m0 kf _ -> kf s0 b0 m0 [] msg)

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Int -> Get ()
skip n = do
  s <- ensure n
  put (B.drop n s)

-- | Skip ahead @n@ bytes. No error if there isn't enough bytes.
uncheckedSkip :: Int -> Get ()
uncheckedSkip n = do
    s <- get
    put (B.drop n s)

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Get a -> Get a
lookAhead ga = Get $ \ s0 b0 m0 kf ks ->
  -- the new continuation extends the old input with the new buffered bytes, and
  -- appends the new buffer to the old one, if there was one.
  let ks' _ b1 = ks (s0 `B.append` bufferBytes b1) (b0 `append` b1)
      kf' _ b1 = kf s0 (b0 `append` b1)
   in unGet ga s0 emptyBuffer m0 kf' ks'

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    M.when (isNothing ma) (put s)
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

-- | Get the next up to @n@ bytes as a ByteString, without consuming them.
uncheckedLookAhead :: Int -> Get B.ByteString
uncheckedLookAhead n = do
    s <- get
    return (B.take n s)

------------------------------------------------------------------------
-- Utility

-- | Get the number of remaining unparsed bytes.  Useful for checking whether
-- all input has been consumed.
--
-- WARNING: when run with @runGetPartial@, remaining will only return the number
-- of bytes that are remaining in the current input.
remaining :: Get Int
remaining = Get (\ s0 b0 m0 _ ks -> ks s0 b0 m0 (B.length s0 + moreLength m0))

-- | Test whether all input has been consumed.
--
-- WARNING: when run with @runGetPartial@, isEmpty will only tell you if you're
-- at the end of the current chunk.
isEmpty :: Get Bool
isEmpty = Get (\ s0 b0 m0 _ ks -> ks s0 b0 m0 (B.null s0 && moreLength m0 == 0))

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input. This function creates a fresh
-- copy of the underlying bytes.
getByteString :: Int -> Get B.ByteString
getByteString n = do
  bs <- getBytes n
  return $! B.copy bs

getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n = f `fmap` getByteString (fromIntegral n)
  where f bs = L.fromChunks [bs]

#if MIN_VERSION_bytestring(0,10,4)
getShortByteString :: Int -> Get BS.ShortByteString
getShortByteString n = do
  bs <- getBytes n
  return $! BS.toShort bs
#endif


------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get B.ByteString
getBytes n | n < 0 = fail "getBytes: negative length requested"
getBytes n = do
    s <- ensure n
    let consume = B.unsafeTake n s
        rest    = B.unsafeDrop n s
        -- (consume,rest) = B.splitAt n s
    put rest
    return consume
{-# INLINE getBytes #-}



------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying strict byteString.

getPtr :: Storable a => Int -> Get a
getPtr n = do
    (fp,o,_) <- B.toForeignPtr `fmap` getBytes n
    let k p = peek (castPtr (p `plusPtr` o))
    return (unsafeDupablePerformIO (withForeignPtr fp k))
{-# INLINE getPtr #-}

-----------------------------------------------------------------------

-- | Read a Int8 from the monad state
getInt8 :: Get Int8
getInt8 = do
    s <- getBytes 1
    return $! fromIntegral (B.unsafeHead s)

-- | Read a Int16 in big endian format
getInt16be :: Get Int16
getInt16be = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 1) )

-- | Read a Int16 in little endian format
getInt16le :: Get Int16
getInt16le = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Int32 in big endian format
getInt32be :: Get Int32
getInt32be = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )

-- | Read a Int32 in little endian format
getInt32le :: Get Int32
getInt32le = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Int64 in big endian format
getInt64be :: Get Int64
getInt64be = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )

-- | Read a Int64 in little endian format
getInt64le :: Get Int64
getInt64le = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 7) `shiftL` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftL` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftL` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftL` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftL`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

{-# INLINE getInt8    #-}
{-# INLINE getInt16be #-}
{-# INLINE getInt16le #-}
{-# INLINE getInt32be #-}
{-# INLINE getInt32le #-}
{-# INLINE getInt64be #-}
{-# INLINE getInt64le #-}

------------------------------------------------------------------------

-- | Read a Word8 from the monad state
getWord8 :: Get Word8
getWord8 = do
    s <- getBytes 1
    return (B.unsafeHead s)

-- | Read a Word16 in big endian format
getWord16be :: Get Word16
getWord16be = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 1))

-- | Read a Word16 in little endian format
getWord16le :: Get Word16
getWord16le = do
    s <- getBytes 2
    return $! (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Word32 in big endian format
getWord32be :: Get Word32
getWord32be = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 3) )

-- | Read a Word32 in little endian format
getWord32le :: Get Word32
getWord32le = do
    s <- getBytes 4
    return $! (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

-- | Read a Word64 in big endian format
getWord64be :: Get Word64
getWord64be = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 7) )

-- | Read a Word64 in little endian format
getWord64le :: Get Word64
getWord64le = do
    s <- getBytes 8
    return $! (fromIntegral (s `B.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `B.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `B.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `B.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `B.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `B.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `B.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `B.unsafeIndex` 0) )

{-# INLINE getWord8    #-}
{-# INLINE getWord16be #-}
{-# INLINE getWord16le #-}
{-# INLINE getWord32be #-}
{-# INLINE getWord32le #-}
{-# INLINE getWord64be #-}
{-# INLINE getWord64le #-}

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get Word
getWordhost = getPtr (sizeOf (undefined :: Word))

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Get Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Get Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Get Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))

------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif


-- Containers ------------------------------------------------------------------

getTwoOf :: Get a -> Get b -> Get (a,b)
getTwoOf ma mb = M.liftM2 (,) ma mb

-- | Get a list in the following format:
--   Word64 (big endian format)
--   element 1
--   ...
--   element n
getListOf :: Get a -> Get [a]
getListOf m = go [] =<< getWord64be
  where
  go as 0 = return $! reverse as
  go as i = do x <- m
               x `seq` go (x:as) (i - 1)

-- | Get an IArray in the following format:
--   index (lower bound)
--   index (upper bound)
--   Word64 (big endian format)
--   element 1
--   ...
--   element n
getIArrayOf :: (Ix i, IArray a e) => Get i -> Get e -> Get (a i e)
getIArrayOf ix e = M.liftM2 listArray (getTwoOf ix ix) (getListOf e)

-- | Get a sequence in the following format:
--   Word64 (big endian format)
--   element 1
--   ...
--   element n
getSeqOf :: Get a -> Get (Seq.Seq a)
getSeqOf m = go Seq.empty =<< getWord64be
  where
  go xs 0 = return $! xs
  go xs n = xs `seq` n `seq` do
              x <- m
              go (xs Seq.|> x) (n - 1)

-- | Read as a list of lists.
getTreeOf :: Get a -> Get (T.Tree a)
getTreeOf m = M.liftM2 T.Node m (getListOf (getTreeOf m))

-- | Read as a list of pairs of key and element.
getMapOf :: Ord k => Get k -> Get a -> Get (Map.Map k a)
getMapOf k m = Map.fromList `fmap` getListOf (getTwoOf k m)

-- | Read as a list of pairs of int and element.
getIntMapOf :: Get Int -> Get a -> Get (IntMap.IntMap a)
getIntMapOf i m = IntMap.fromList `fmap` getListOf (getTwoOf i m)

-- | Read as a list of elements.
getSetOf :: Ord a => Get a -> Get (Set.Set a)
getSetOf m = Set.fromList `fmap` getListOf m

-- | Read as a list of ints.
getIntSetOf :: Get Int -> Get IntSet.IntSet
getIntSetOf m = IntSet.fromList `fmap` getListOf m

-- | Read in a Maybe in the following format:
--   Word8 (0 for Nothing, anything else for Just)
--   element (when Just)
getMaybeOf :: Get a -> Get (Maybe a)
getMaybeOf m = do
  tag <- getWord8
  case tag of
    0 -> return Nothing
    _ -> Just `fmap` m

-- | Read an Either, in the following format:
--   Word8 (0 for Left, anything else for Right)
--   element a when 0, element b otherwise
getEitherOf :: Get a -> Get b -> Get (Either a b)
getEitherOf ma mb = do
  tag <- getWord8
  case tag of
    0 -> Left  `fmap` ma
    _ -> Right `fmap` mb

-- | Read in a length and then read a nested structure
--   of that length.
getNested :: Get Int -> Get a -> Get a
getNested getLen getVal = do
    n <- getLen
    isolate n getVal
