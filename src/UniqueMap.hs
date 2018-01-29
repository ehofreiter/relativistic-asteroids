module UniqueMap where

import Prelude hiding (foldr, foldl, map, filter)
import qualified Prelude

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Either
import Data.Functor
import Data.Foldable
import Data.Traversable

-- This module defines a map that automatically generates a unique
-- identifier for each element.

data UMap a = UMap { key :: Key,
                     set :: Map Key a }
                   deriving Show

instance Functor UMap where
  fmap = _overSet . fmap

instance Foldable UMap where
  foldMap f = foldMap f . set
  foldr f x = foldr f x . set

instance Traversable UMap where
  traverse f (UMap k m) = UMap k <$> traverse f m
  sequenceA (UMap k m) = UMap k <$> sequenceA m

newtype Key = Key Int deriving (Eq, Ord, Show)

-- No new elements can be inserted in the function f, since _overSet
-- does not increment the next key.
_overSet :: (Map Key a -> Map Key b) -> UMap a -> UMap b
_overSet f um = um { set = f (set um) }

_nextKey :: Key -> Key
_nextKey (Key n) = Key (n + 1)

infixl 9 !

(!) :: UMap a -> Key -> a
m ! k = set m Map.! k

empty :: UMap a
empty = UMap (Key 0) $ Map.empty

insert :: a -> UMap a -> UMap a
insert x (UMap k m) = UMap k' m'
    where k' = _nextKey k
          m' = Map.insert k x m

map :: (a -> b) -> UMap a -> UMap b
map f m = m { set = Map.map f (set m) }

keys :: UMap a -> [Key]
keys = Map.keys . set

toList :: UMap a -> [a]
toList = Prelude.map snd . Map.toList . set

assocs :: UMap a -> [(Key, a)]
assocs = Map.assocs . set

fromList :: [a] -> UMap a
fromList = foldr insert empty

filter :: (a -> Bool) -> UMap a -> UMap a
filter f m = m { set = Map.filter f (set m) }

delete :: Key -> UMap a -> UMap a
delete = _overSet . Map.delete

deleteKeys :: [Key] -> UMap a -> UMap a
deleteKeys ks = foldl (.) id (Prelude.map delete ks)

-- Untested, use with caution.
mapMaybe :: (a -> Maybe b) -> UMap a -> UMap b
mapMaybe f = map fromJust . filter isJust . map f

-- Similar to the Data.Either.rights function for lists.  Extracts all
-- of the Right values.  Keys are preserved.
rights :: UMap (Either a b) -> UMap b
rights = _overSet rights'
  where rights' = Map.fromList . Data.Either.rights . Prelude.map f . Map.toList
        f (k, Right x) = Right (k, x)
        f (k, Left x)  = Left  (k, x)
