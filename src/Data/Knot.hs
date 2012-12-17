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
module Data.Knot (
    -- classes
    Lookup(..),
    -- functions
    tie, tie', isConsistent,
    -- Map specializations
    tieMap, tieMap',
    -- Error reporting
    TieError(..), TieErrors,
    -- re-exports
    Base, Unfoldable(embed)
) where

import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Error
import qualified Data.Foldable as F
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Data.Functor.Foldable


class Functor map => Lookup map where
    type LookupKey map :: *
    lookupKey :: LookupKey map -> map u -> Maybe u

instance Ord k => Lookup (Map k) where
    type LookupKey (Map k) = k
    lookupKey = Map.lookup
instance Lookup ((->) k) where
    type LookupKey ((->) k) = k
    lookupKey k f = Just (f k)
instance Lookup IntMap.IntMap where
    type LookupKey IntMap.IntMap = Int
    lookupKey = IntMap.lookup

-- | Possible errors when tying the knot.
data TieError k v
    = MissingKey (v k) k    -- ^ Value @v@ referenced non-existent key @k@.
  deriving (Show, Eq, Ord)
type TieErrors k v = Seq (TieError k v)

-- | Check the loader for consistency, i.e. if all referenced keys
-- have a corresponding value. Values must to implement 'Foldable'
-- that traverses over all referenced keys. Similarly, the 'Lookup' instance
-- must implement 'Foldable'.
isConsistent :: (Functor v, F.Foldable v, Lookup map, F.Foldable map, k ~ LookupKey map)
    => map (v k)
        -- ^ The structure to check.
    -> Either (Seq (TieError k v)) (map (v k))
        -- ^ The structure (checked to be correct) or a non-empty sequence of errors.
isConsistent l = if Seq.null errors
                    then Right l
                    else Left errors
  where
    errors = F.foldMap keysOk l
    keysOk v = F.foldMap checkKey v
      where
        checkKey r = case lookupKey r l of
                        Nothing     -> Seq.singleton (MissingKey v r)
                        _           -> mempty

-- | Helper function for anamorphisms.
ana' :: Unfoldable t
     => (s -> Base t s)
     -> Base t s -> t
ana' f = embed . fmap (ana f)

-- | Ties the knot without checking consistency.
-- If the references are inconsistent, an exception is raised.
tie' :: (Unfoldable v, Lookup map, k ~ LookupKey map)
     => map (Base v k)
     -> map v
tie' m = fmap (ana' $ \k -> maybe (error "Missing key when tying the knot") id $ lookupKey k m) m

-- | Specialization of 'tie'' to 'Map's.
tieMap' :: (Ord k, Unfoldable v)
        => Map k (Base v k)
        -> Map k v
tieMap' = tie'

-- | Checks consistency by calling 'isConsistent' and then and ties the knot using 'tie''.
tie :: (F.Foldable (Base v), Unfoldable v, Lookup map, k ~ LookupKey map, F.Foldable map)
    => map (Base v k)
    -> Either (TieErrors k (Base v)) (map v)
tie = liftM tie' . isConsistent

-- | Specialization of 'tie' to 'Map's.
tieMap :: (Ord k, F.Foldable (Base v), Unfoldable v)
       => Map k (Base v k)
       -> Either (TieErrors k (Base v)) (Map k v)
tieMap = tie
