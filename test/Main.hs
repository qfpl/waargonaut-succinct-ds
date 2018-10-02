{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Control.Lens               (over, _Left)
import           Control.Monad              ((>=>))

import           Data.Bool                  (bool)
import           Data.Functor.Identity      (Identity)

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy       as BSL

import qualified Data.Text                  as Text

import           Hedgehog                   (Property, checkParallel, discover,
                                             forAll, property, tripping, (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           System.Exit                as Exit

import           Text.Parsec                (parse)

import           Waargonaut.Decode.Succinct (Decoder)
import qualified Waargonaut.Decode.Succinct as WS

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as W

import           Waargonaut                 (Json, parseWaargonaut)
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

parseBS :: ByteString -> Either DecodeError Json
parseBS = over _Left (ParseFailed . Text.pack . show)
  . parse parseWaargonaut "ByteString"

encode :: Encoder a -> a -> BSL.ByteString
encode enc = W.runPureEncoder enc

decode :: Decoder Identity a -> BSL.ByteString -> Either (DecodeError, WS.CursorHistory) a
decode dec = WS.runPureDecode dec parseBS . WS.mkCursor . BSL.toStrict

intListDecode :: Monad f => Decoder f [Int]
intListDecode = WS.list WS.int

intListEncode :: Encoder [Int]
intListEncode = W.list W.int

prop_trippingSimpleIntList :: Property
prop_trippingSimpleIntList = property $ do
  xs <- forAll . Gen.list (Range.linear 0 100) $ Gen.int (Range.linear 0 9999)
  tripping xs (encode intListEncode) (decode intListDecode)

prop_maybe_maybe :: Property
prop_maybe_maybe = property $ do
  b <- forAll (Gen.maybe (Gen.maybe Gen.bool)) -- Generate a (Maybe (Maybe Bool))
  tripping b (encode enc) (decode dec)
  where
    enc =
      let
        beepObj = W.mapLikeObj $
          W.maybeOrNull W.bool `W.atKey` "beep"
      in
        -- Encode as {"boop":{"beep": bool|null }}|null
        W.maybeOrNull (
          W.mapLikeObj (
              beepObj `W.atKey` "boop"
              )
          )

    dec =
      let
        boopObj = WS.down >=> WS.moveToKey "boop"
        beepObj = WS.down >=> WS.fromKey "beep" WS.boolean
      in
        WS.withCursor (WS.try . boopObj >=> traverse (WS.try . beepObj))

shuffleListDecoder :: Monad f => Decoder f [Int]
shuffleListDecoder = WS.withCursor $ \curs -> do
  -- Move into the array
  x <- WS.down curs
  -- Decode first
  a <- WS.focus WS.int x
  -- Step to the right
  x' <- WS.moveRight1 x
  -- Decode that one
  b <- WS.focus WS.int x'
  -- Jump to the left
  x'' <- WS.moveLeft1 x'
  -- Decode that one again
  c <- WS.focus WS.int x''
  -- Step back to the right again
  x''' <- WS.moveRight1 x''
  -- Decode that one
  d <- WS.focus WS.int x'''
  -- Shake it all out
  pure [a,b,c,d]

prop_shuffleList :: Property
prop_shuffleList = property $ do
  [a,b] <- forAll . Gen.list (Range.singleton 2) $ Gen.int (Range.linear 0 1000)
  Right [a,b,a,b] === (decode shuffleListDecoder . encode intListEncode) [a,b]

main :: IO ()
main = checkParallel $$(discover) >>= bool (Exit.exitFailure) (Exit.exitSuccess)
