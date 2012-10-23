tie-knot
========

"Ties the knot" on a given set of structures that reference each other by keys - replaces the keys with their respective values.
Takes `Map k (v k)` and converts into `Map k v'` where v' is the fixed point of `v`.

This is accomplished by functions
```haskell
type RefMap k v = Map k (v k)

tie :: (Ord k, F.Foldable (Pre v), Fixpoint v) => RefMap k (Pre v) -> Either (TieError k) (Map k v)
tie' :: (Ord k, Fixpoint v) => RefMap k (Pre v) -> Map k v
```
One performs consistency checking, the other just ends with an error if a key is missing in the map.

Example:
--------

Suppose that Alice loves Bob and her cat, Bob loves Alice and the cat loves only itself. Imagine that we're reading this information from some kind of a text file, and store the intermediate data into a list. We would like to create a data structure which would contain these cyclic dependencies:

```haskell
data Person = Person { name :: String, loves :: [Person] }

-- Define a variant of Person where the recursive type
-- is given as a parameter, and injection/projection functions.
instance Fixpoint Person where
  data Pre Person t = Loves { _name :: String, _loves :: [t] }
  inject ~(Loves n ps)    = Person n ps
  project ~(Person n ps)  = Loves n ps

-- The easisest way to get 'Foldable' + 'Functor' is to implement
-- 'Traversable' and then just use the default implementations.
instance T.Traversable (Pre Person) where
    traverse f (Loves n ns) = Loves n <$> T.traverse f ns
instance Functor (Pre Person) where
    fmap = T.fmapDefault
instance F.Foldable (Pre Person) where
    foldMap = T.foldMapDefault
-- (Alternatively we could have used GHC's DeriveTraversable extension.)

-- Let's create a representaion of Alice with cicrular dependencies:
alice :: Person
alice = fromJust . Map.lookup "Alice" . 
            tie' . Map.fromList . map nameValue $ lst
  where
    lst = [ Loves "Alice" ["Bob", "cat"]
          , Loves "Bob"   ["Alice"]
          -- you may disagree, but the cat thinks of itself as Person
          , Loves "cat"   ["cat"]
          ]
    nameValue loves = (_name loves, loves)
```
