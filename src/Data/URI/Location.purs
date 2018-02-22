module Data.URI.Location where

import Data.Maybe (Maybe (..))
import Data.URI (URIPathAbs, Query, Fragment, Scheme, Authority, HierarchicalPart (..), URI (..))



data Location = Location URIPathAbs (Maybe Query) (Maybe Fragment)


toURI :: {scheme :: Maybe Scheme, authority :: Maybe Authority, location :: Location} -> URI
toURI {scheme,authority,location: Location p q f} = URI scheme (HierarchicalPart authority (Just p)) q f

fromURI :: URI -> Maybe {scheme :: Maybe Scheme, authority :: Maybe Authority, location :: Location}
fromURI (URI scheme (HierarchicalPart authority mPath) q f) = case mPath of
  Nothing -> Nothing
  Just path -> Just {scheme, authority, location: Location path q f}
