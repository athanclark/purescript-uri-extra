module Data.URI.Location where

import Prelude
import Data.Maybe (Maybe (..), maybe)
import Data.Either (Either (..))
import Data.URI (URIPathAbs, Query, Fragment, Scheme, Authority, HierarchicalPart (..), URI (..))
import Data.URI.Query as Query
import Data.URI.Fragment as Fragment
import Data.URI.Path (parseURIPathAbs)
import Data.URI.Path as URIPath
import Data.Generic (class Generic, gEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson, fail)
import Data.NonEmpty (NonEmpty (..))
import Data.Path.Pathy.Gen (genAbsFilePath, genAbsDirPath)
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen as QC


data Location = Location URIPathAbs (Maybe Query) (Maybe Fragment)

derive instance genericLocation :: Generic Location

instance arbitraryLocation :: Arbitrary Location where
  arbitrary = Location <$> arbitraryURIPathAbs <*> arbitrary <*> arbitrary
    where
      arbitraryURIPathAbs = QC.oneOf $ NonEmpty
        (Left <$> genAbsDirPath) [Right <$> genAbsFilePath]


instance eqLocation :: Eq Location where
  eq = gEq

instance showLocation :: Show Location where
  show = printLocation

instance encodeJsonLocation :: EncodeJson Location where
  encodeJson x = encodeJson (printLocation x)

instance decodeJsonLocation :: DecodeJson Location where
  decodeJson json = do
    s <- decodeJson json
    case runParser parseLocation s of
      Left e -> fail (show e)
      Right x -> pure x

class ToLocation sym where
  toLocation :: sym -> Location

class FromLocation sym where
  fromLocation :: Location -> Either String sym



printLocation :: Location -> String
printLocation (Location path query frag) =
  URIPath.printPath path <> maybe "" Query.print query <> maybe "" Fragment.print frag

parseLocation :: Parser Location
parseLocation = do
  Location <$> parseURIPathAbs
           <*> optionMaybe Query.parser
           <*> optionMaybe Fragment.parser
           <*  eof


toURI :: {scheme :: Maybe Scheme, authority :: Maybe Authority, location :: Location} -> URI
toURI {scheme,authority,location: Location p q f} = URI scheme (HierarchicalPart authority (Just p)) q f

fromURI :: URI -> Maybe {scheme :: Maybe Scheme, authority :: Maybe Authority, location :: Location}
fromURI (URI scheme (HierarchicalPart authority mPath) q f) = case mPath of
  Nothing -> Nothing
  Just path -> Just {scheme, authority, location: Location path q f}
