module Test.Main where

import Data.URI.Location (Location)

import Prelude
import Data.Either (Either (..))
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Type.Proxy (Proxy (..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Test.QuickCheck (class Arbitrary, quickCheck, Result (..))



main :: Eff _ Unit
main = do
  log "JSON Iso:"
  jsonIsoAssert "Data.URI.Location" (Proxy :: Proxy Location)


jsonIsoAssert :: forall a
               . EncodeJson a
              => DecodeJson a
              => Eq a
              => Show a
              => Arbitrary a
              => String -> Proxy a -> Eff _ Unit
jsonIsoAssert name Proxy = do
  log ("    " <> name)
  quickCheck (\(x :: a) -> jsonIso x)
  log ""


jsonIso :: forall a. EncodeJson a => DecodeJson a => Eq a => Show a => a -> Result
jsonIso x = case decodeJson (encodeJson x) of
  Left y -> Failed $ "decoding failure: " <> y <> ", " <> show x
  Right y
    | x == y -> Success
    | otherwise -> Failed $ "Not identical: " <> show x <> ", " <> show y
