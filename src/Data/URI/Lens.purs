module Data.URI.Lens where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Lens.Lens (Lens', lens)
import Data.Lens.Prism (Prism', prism')
import Data.URI.URI (URI (..))
import Data.URI.Scheme (Scheme)
import Data.URI.HierarchicalPart (HierarchicalPart (..))
import Data.URI.Query (Query)
import Data.URI.UserInfo (UserInfo)
import Data.URI.Fragment (Fragment)
import Data.URI.Authority (Authority (..))
import Data.URI.Path (URIPathAbs)
import Data.URI.Host (Host (..))
import Data.URI.Port (Port)


scheme :: Lens' URI (Maybe Scheme)
scheme = lens (\(URI s _ _ _) -> s) (\(URI _ h q f) s -> URI s h q f)

hierarchicalPart :: Lens' URI HierarchicalPart
hierarchicalPart = lens (\(URI _ h _ _) -> h) (\(URI s _ q f) h -> URI s h q f)

query :: Lens' URI (Maybe Query)
query = lens (\(URI _ _ q _) -> q) (\(URI s h _ f) q -> URI s h q f)

fragment :: Lens' URI (Maybe Fragment)
fragment = lens (\(URI _ _ _ f) -> f) (\(URI s h q _) f -> URI s h q f)


authority :: Lens' HierarchicalPart (Maybe Authority)
authority = lens (\(HierarchicalPart a _) -> a) (\(HierarchicalPart _ p) a -> HierarchicalPart a p)

uriPathAbs :: Lens' HierarchicalPart (Maybe URIPathAbs)
uriPathAbs = lens (\(HierarchicalPart _ p) -> p) (\(HierarchicalPart a _) p -> HierarchicalPart a p)


userInfo :: Lens' Authority (Maybe UserInfo)
userInfo = lens (\(Authority i _) -> i) (\(Authority _ hs) i -> Authority i hs)

hosts :: Lens' Authority (Array (Tuple Host (Maybe Port)))
hosts = lens (\(Authority _ hs) -> hs) (\(Authority i _) hs -> Authority i hs)


ipv6Address :: Prism' Host String
ipv6Address = prism' IPv6Address $ case _ of
  IPv6Address x -> Just x
  _ -> Nothing

ipv4Address :: Prism' Host String
ipv4Address = prism' IPv4Address $ case _ of
  IPv4Address x -> Just x
  _ -> Nothing

nameAddress :: Prism' Host String
nameAddress = prism' NameAddress $ case _ of
  NameAddress x -> Just x
  _ -> Nothing
