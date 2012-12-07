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
-- The module re-exports a part of the fixpoint package.
--
-- An example how to construct a structure with circular dependencies:
--
-- > data Person = Person { name :: String, loves :: [Person] }
-- > -- Define a variant of Person where the recursive type
-- > -- is given as a parameter, and injection/projection functions.
-- > instance Fixpoint Person where
-- >   data Pre Person t = Loves { _name :: String, _loves :: [t] }
-- >   inject ~(Loves n ps)    = Person n ps
-- >   project ~(Person n ps)  = Loves n ps
-- >
-- > -- The easisest way to get 'Foldable' + 'Functor' is to implement
-- > -- 'Traversable' and then just use the default implementations.
-- > instance T.Traversable (Pre Person) where
-- >     traverse f (Loves n ns) = Loves n <$> T.traverse f ns
-- >
-- > instance Functor (Pre Person) where
-- >     fmap = T.fmapDefault
-- > instance F.Foldable (Pre Person) where
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
module Data.Knot (tie, tie', isConsistent, RefMap, TieError(..), Fixpoint, Pre, inject, project) where

import Control.Monad
import Control.Monad.Error
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Maybe

import Data.Fixpoint

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
ana' :: Fixpoint t => (s -> Pre t s) -> Pre t s -> t
ana' f = inject . fmap (ana f)

-- | Ties the knot without checking consistency.
-- If the references are inconsistent, an exception is raised.
tie' :: (Ord k, Fixpoint v) => RefMap k (Pre v) -> Map k v
tie' m = Map.map f m
  where
    -- (k -> v k) -> v k -> Fix v
    f = ana' $ \k -> fromJust (Map.lookup k m)

-- | Checks consistency by calling 'isConsistent' and then and ties the knot using 'tie''.
tie :: (Ord k, F.Foldable (Pre v), Fixpoint v) => RefMap k (Pre v) -> Either (TieError k) (Map k v)
tie = liftM tie' . isConsistent
