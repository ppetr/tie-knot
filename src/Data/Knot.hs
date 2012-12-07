{-
    This file is part of tie-knot.

    tie-knot is free software: you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

    tie-knot is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
    more details.

    You should have received a copy of the GNU Lesser General Public License
    along with tie-knot.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- | Module for tying the knot on data structures that reference each other by
-- some kind of keys. The 'tie' function replaces all such references with the actual
-- value, creating possibly recursive or cyclic data structures.
--
-- The module re-exports a part of the recursion-schemes package.
--
-- An example how to construct a structure with circular dependencies:
--
-- > data Person = Person { name :: String, loves :: [Person] }
-- > -- Define a variant of Person where the recursive type
-- > -- is given as a parameter and the embedding function.
-- > data Loves t = Loves { _name :: String, _loves :: [t] }
-- > type instance Base Person = Loves
-- > instance Unfoldable Person where
-- >   embed ~(Loves n ps)    = Person n ps
-- >
-- > -- The easisest way to get 'Foldable' + 'Functor' is to implement
-- > -- 'Traversable' and then just use the default implementations.
-- > instance T.Traversable Loves where
-- >     traverse f (Loves n ns) = Loves n <$> T.traverse f ns
-- >
-- > instance Functor Loves where
-- >     fmap = T.fmapDefault
-- > instance F.Foldable Loves where
-- >     foldMap = T.foldMapDefault
-- >
-- > -- Let's create a person with cicrular dependencies:
-- > alice :: Person
-- > alice = fromJust . Map.lookup "Alice" .
-- >             tie' . Map.fromList . map (\l -> (_name l, l)) $ lst
-- >   where
-- >     lst = [ Loves "Alice" ["Bob", "cat"]
-- >           , Loves "Bob"   ["Alice"]
-- >           -- you may disagree, but the cat thinks of itself as Person
-- >           , Loves "cat"   ["cat"]
-- >           ]
module Data.Knot (tie, tie', isConsistent, RefMap, TieError(..), Base, Unfoldable, embed) where

import Control.Monad
import Control.Monad.Error
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Maybe

import Data.Functor.Foldable

-- | Represents a set of data 'v' that reference each other
-- using keys of type 'k'.
type RefMap k v = Map k (v k)

-- | Possible errors when tying the knot.
data TieError k
    = MissingKey k k    -- ^ A value with key k1 referenced non-existent key k2.
  deriving (Show, Eq, Ord)

-- | Check the loader for consistency, i.e. if all referenced keys
-- have a corresponding value. Values need to implement 'Foldable'
-- that traverses over all referenced keys.
isConsistent :: (Ord k, F.Foldable v, Functor v)
    => RefMap k v                           -- ^ The loader to check.
    -> Either (TieError k) (RefMap k v)     -- ^ The loader argument or an error.
isConsistent l = maybe (Right l) Left . getFirst $
        Map.foldrWithKey (\k -> mappend . keysOk k) mempty l
  where
    keysOk k = F.foldMap (\r -> First $ if (Map.member r l)
                                                     then Nothing
                                                     else (Just (MissingKey k r)) )

-- | Helper function for anamorphisms.
ana' :: Unfoldable t => (s -> Base t s) -> Base t s -> t
ana' f = embed . fmap (ana f)

-- | Ties the knot without checking consistency.
-- If the references are inconsistent, an exception is raised.
tie' :: (Ord k, Unfoldable v) => RefMap k (Base v) -> Map k v
tie' m = Map.map f m
  where
    -- (k -> v k) -> v k -> Fix v
    f = ana' $ \k -> fromJust (Map.lookup k m)

-- | Checks consistency by calling 'isConsistent' and then and ties the knot using 'tie''.
tie :: (Ord k, F.Foldable (Base v), Unfoldable v) => RefMap k (Base v) -> Either (TieError k) (Map k v)
tie = liftM tie' . isConsistent
