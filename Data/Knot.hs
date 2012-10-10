{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- | Module for tying the knot on data structures that reference each other by
-- some kind of keys. The 'tie' function replaces all such references with the actual
-- value, creating possibly recursive or cyclic data structures.
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
