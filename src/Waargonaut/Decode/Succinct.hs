{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Waargonaut.Decode.Succinct
  (
    -- * Types
    CursorHistory
  , SuccinctCursor
  , DecodeResult
  , Decoder
  , JCurs (..)

    -- * Runners
  , runDecode
  , runPureDecode
  , overrideParser

    -- * Cursors
  , withCursor
  , mkCursor
  , cursorRankL
  , manyMoves
  , down
  , up
  , moveRightN
  , moveRight1
  , moveLeftN
  , moveLeft1
  , moveToValAtKey

    -- * Decoding
  , jsonAtCursor
  , leftwardCons
  , rightwardSnoc
  , directedConsumption
  , int
  , scientific
  , integral
  , string
  , unboundedChar
  , boundedChar
  , text
  , boolean
  , null
  , array
  ) where

import           Control.Lens                              (Cons, Lens', Snoc,
                                                            cons, lens,
                                                            modifying, snoc,
                                                            traverseOf, (.~),
                                                            (^.), _Wrapped)

import           Prelude                                   (Bool, Bounded, Char,
                                                            Int, Integral,
                                                            String,
                                                            fromIntegral, (-),
                                                            (==))

import           Control.Applicative                       (Applicative (..))
import           Control.Category                          ((.))
import           Control.Monad                             (Monad (..), (>=>))

import           Control.Monad.Except                      (lift, liftEither,
                                                            throwError)
import           Control.Monad.Reader                      (ReaderT (..), ask,
                                                            local, runReaderT)
import           Control.Monad.State                       (MonadState)

import           Control.Error.Util                        (note)
import           Control.Monad.Error.Hoist                 ((<?>))

import           Data.Either                               (Either (..))
import           Data.Foldable                             (foldl)
import           Data.Function                             (const, flip, ($),
                                                            (&))
import           Data.Functor                              ((<$), (<$>))
import           Data.Functor.Identity                     (Identity,
                                                            runIdentity)
import           Data.Monoid                               (mempty)

import           Data.Scientific                           (Scientific)

import           Data.List                                 (replicate)
import           Data.Maybe                                (Maybe (..))

import           Data.Text                                 (Text)

import           Data.ByteString.Char8                     (ByteString)
import qualified Data.ByteString.Char8                     as BS8

import           Numeric.Natural                           (Natural)

import           Waargonaut.Types

import           HaskellWorks.Data.Positioning             (Count)
import qualified HaskellWorks.Data.Positioning             as Pos

import qualified HaskellWorks.Data.BalancedParens.FindOpen as BP

import           HaskellWorks.Data.Bits                    ((.?.))
import           HaskellWorks.Data.FromByteString          (fromByteString)
import           HaskellWorks.Data.TreeCursor              (TreeCursor (..))

import           HaskellWorks.Data.Json.Succinct.Cursor    (JsonCursor (..))
import qualified HaskellWorks.Data.Json.Succinct.Cursor    as JC

import           Waargonaut.Decode.Error                   (DecodeError (..))
import           Waargonaut.Decode.ZipperMove              (ZipperMove (..))

import qualified Waargonaut.Decode.Internal                as DI

import           Waargonaut.Decode.Succinct.Types          (CursorHistory,
                                                            DecodeResult (..),
                                                            Decoder (..),
                                                            JCurs (..), ParseFn,
                                                            SuccinctCursor)

withCursor
  :: Monad f
  => (JCurs -> DecodeResult f a)
  -> Decoder f a
withCursor g = Decoder $ \p ->
  DI.runDecoder' $ DI.withCursor' (flip runReaderT p . unDecodeResult . g)

mkCursor :: ByteString -> JCurs
mkCursor = JCurs . fromByteString

cursorRankL :: Lens' (JsonCursor s i p) Count
cursorRankL = lens JC.cursorRank (\c r -> c { cursorRank = r })

manyMoves :: Monad m => Natural -> (b -> m b) -> b -> m b
manyMoves i g = foldl (>=>) pure (replicate (fromIntegral i) g)

moveCursBasic
  :: Monad f
  => (SuccinctCursor -> Maybe SuccinctCursor)
  -> ZipperMove
  -> JCurs
  -> DecodeResult f JCurs
moveCursBasic f m c =
  traverseOf _Wrapped f c <?> FailedToMove m >>= recordRank m

down
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
down =
  moveCursBasic firstChild D

up
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
up =
  moveCursBasic parent U

moveRightN
  :: Monad f
  => Natural
  -> JCurs
  -> DecodeResult f JCurs
moveRightN i =
  moveCursBasic (manyMoves i nextSibling) (R i)

moveRight1
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
moveRight1 =
  moveRightN 1

moveLeft1
  :: Monad f
  => JCurs
  -> DecodeResult f JCurs
moveLeft1 jc =
  let
    c = jc ^. _Wrapped
    rnk = c ^. cursorRankL

    setRank r = jc & _Wrapped . cursorRankL .~ r
    prev = rnk - 1
  in
    setRank <$> BP.findOpen (JC.balancedParens c) prev <?> InputOutOfBounds prev

moveLeftN
  :: Monad f
  => Natural
  -> JCurs
  -> DecodeResult f JCurs
moveLeftN i =
  manyMoves i moveLeft1

jsonAtCursor
  :: Monad f
  => (ByteString -> Either DecodeError a)
  -> JCurs
  -> DecodeResult f a
jsonAtCursor p jc = do
  let
    c         = jc ^. _Wrapped
    rnk       = c ^. cursorRankL

    leading   = fromIntegral $ Pos.toCount (JC.jsonCursorPos c)
    cursorTxt = BS8.drop leading (JC.cursorText c)

  if JC.balancedParens c .?. Pos.lastPositionOf rnk
    then liftEither (p cursorTxt)
    else throwError (InputOutOfBounds rnk)

recordRank
  :: ( MonadState CursorHistory f
     , Monad f
     )
  => ZipperMove
  -> JCurs
  -> f JCurs
recordRank mv c =
  c <$ modifying _Wrapped (`snoc` (mv, c ^. _Wrapped . cursorRankL))

focus
  :: Monad f
  => Decoder f a
  -> JCurs
  -> DecodeResult f a
focus decoder curs = DecodeResult $ do
  p <- ask
  lift $ runDecoder decoder p curs

moveToValAtKey
  :: Monad f
  => Text
  -> JCurs
  -> DecodeResult f JCurs
moveToValAtKey k c =
  -- Tease out the key
  focus text c >>= \k' -> if k' == k -- Are we at the key we want to be at ?
  -- if we are, then move into the THING at the key
  then moveRight1 c
  -- if not, then jump to the next key index, the adjacent sibling is opening of the value of the current key
  else moveRightN 2 c >>= moveToValAtKey k

atCursor
  :: Monad f
  => Text
  -> (Json -> Maybe c)
  -> Decoder f c
atCursor m c = withCursor $ \curs -> do
  p <- ask
  jsonAtCursor p curs >>=
    liftEither . note (ConversionFailure m) . c

directedConsumption
  :: Monad f
  => (b -> a -> b)
  -> (JCurs -> DecodeResult f JCurs)
  -> b
  -> Decoder f a
  -> JCurs
  -> DecodeResult f b
directedConsumption nom f s elemD curs = DecodeResult . ReaderT $ \p ->
  DI.directedConsumption' s nom
    (flip runReaderT p . unDecodeResult . f)
    (DI.Decoder' $ runDecoder elemD p)
    curs

leftwardCons
  :: ( Monad f
     , Cons s s a a
     )
  => s
  -> Decoder f a
  -> JCurs
  -> DecodeResult f s
leftwardCons =
  directedConsumption (flip cons) moveLeft1

rightwardSnoc
  :: ( Monad f
     , Snoc s s a a
     )
  => s
  -> Decoder f a
  -> JCurs
  -> DecodeResult f s
rightwardSnoc =
  directedConsumption snoc moveRight1

runDecode
  :: Monad f
  => Decoder f a
  -> ParseFn
  -> JCurs
  -> f (Either (DecodeError, CursorHistory) a)
runDecode dr p =
  DI.runDecoderResultT . runDecoder dr p

runPureDecode
  :: Decoder Identity a
  -> ParseFn
  -> JCurs
  -> Either (DecodeError, CursorHistory) a
runPureDecode dr p =
  runIdentity . runDecode dr p

overrideParser
  :: Monad f
  => ParseFn
  -> DecodeResult f a
  -> DecodeResult f a
overrideParser parseOverride =
  local (const parseOverride)

integral :: (Monad f, Integral n, Bounded n) => Decoder f n
integral = atCursor "integral" DI.integral'

int :: Monad f => Decoder f Int
int = integral

scientific :: Monad f => Decoder f Scientific
scientific = atCursor "scientific" DI.scientific'

string :: Monad f => Decoder f String
string = atCursor "string" DI.string'

unboundedChar :: Monad f => Decoder f Char
unboundedChar = atCursor "unbounded char" DI.unboundedChar'

boundedChar :: Monad f => Decoder f Char
boundedChar = atCursor "bounded char" DI.boundedChar'

text :: Monad f => Decoder f Text
text = atCursor "text" DI.text'

null :: Monad f => Decoder f ()
null = atCursor "null" DI.null'

boolean :: Monad f => Decoder f Bool
boolean = atCursor "bool" DI.boolean'

array :: Monad f => Decoder f a -> JCurs -> DecodeResult f [a]
array elemD = down >=> rightwardSnoc mempty elemD

-- fooDecoder :: Monad f => Decoder f [Int]
-- fooDecoder = withCursor (array int)

-- wutDecoder :: Monad f => Decoder f [Int]
-- wutDecoder = withCursor $ \curs -> do
--   -- Move into the array
--   x <- down curs
--   -- Decode first
--   a <- focus int x
--   -- Step to the right
--   x' <- moveRightN 1 x
--   -- Decode that one
--   b <- focus int x'

--   -- Jump to the left
--   x'' <- moveLeft1 x'
--   -- Decode that one again
--   c <- focus int x''
--   -- Step back to the right again
--   x''' <- moveRightN 1 x''
--   -- Decode that one
--   d <- focus int x'''
--   -- Shake it all out
--   pure [a,b,c,d]
