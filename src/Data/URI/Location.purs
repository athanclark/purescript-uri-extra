module Data.URI.Location where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe (..), maybe)
import Data.URI (URIPathAbs, Query, Fragment, Scheme, Authority, HierarchicalPart (..), URI (..))
import Data.URI.Query as Query
import Data.URI.Fragment as Fragment
import Data.URI.Path (parseURIPathAbs)
import Data.URI.Path as URIPath
import Data.Generic (class Generic)
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (optionMaybe)
import Text.Parsing.StringParser.String (eof)


data Location = Location URIPathAbs (Maybe Query) (Maybe Fragment)

derive instance genericLocation :: Generic Location

instance eqLocation :: Eq Location where
  eq (Location p1 q1 f1) (Location p2 q2 f2) = p1 == p2 && q1 == q2 && f1 == f2

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
