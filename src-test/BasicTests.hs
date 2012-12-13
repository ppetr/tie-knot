{-# LANGUAGE TypeFamilies #-}
import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Knot

-- DLists
-- ---------------------------------------------------------------------

data DList a = DLNode { prev :: DList a, dlval :: a, next :: DList a }

data DList' a t = DLNode' t a t
type instance Base (DList a) = DList' a
instance Unfoldable (DList a) where
  embed ~(DLNode' u x v) = DLNode u x v

instance Functor (DList' n) where
    fmap = T.fmapDefault
instance T.Traversable (DList' n) where
    traverse f (DLNode' u n v) = DLNode' <$> f u <*> pure n <*> f v
instance F.Foldable (DList' n) where
    foldMap = T.foldMapDefault
 
mkDList :: [a] -> DList a
mkDList xs =fromJust . Map.lookup 0 . tie' $ dict
  where
    dict = Map.fromList 
            . map (\(i, x) -> (i, DLNode' (pre i) x (nxt i)))
            . zip [0..] $ xs
    n = length xs
    pre i = (i + n - 1) `rem` n
    nxt i = (i + 1) `rem` n


-- Persons
-- ---------------------------------------------------------------------

data Person = Person { name :: String, loves :: [Person] }

-- Define a variant of Person where the recursive type
-- is given as a parameter and the embedding function.

data Person' t = Person' { _name :: String, _loves :: [t] }
type instance Base Person = Person'
instance Unfoldable Person where
  embed ~(Person' n ps)    = Person n ps

-- The easisest way to get 'Foldable' + 'Functor' is to implement
-- 'Traversable' and then just use the default implementations.
instance T.Traversable Person' where
    traverse f (Person' n ns) = Person' n <$> T.traverse f ns
instance Functor Person' where
    fmap = T.fmapDefault
instance F.Foldable Person' where
    foldMap = T.foldMapDefault

-- Let's create a person with cicrular dependencies:
alice :: Person
alice = fromJust . Map.lookup "Alice" . 
            tie' . Map.fromList . map nameValue $ lst
  where
    lst = [ Person' "Alice" ["Bob", "cat"]
          , Person' "Bob"   ["Alice"]
          -- you may disagree, but the cat thinks of itself as Person
          , Person' "cat"   ["cat"]
          ]
    nameValue loves = (_name loves, loves)


-- ---------------------------------------------------------------------

main = do
    putStrLn (name alice)
    print (dlval . prev . mkDList $ [1,2,3])
