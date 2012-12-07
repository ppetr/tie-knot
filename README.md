# tie-knot

"Ties the knot" on a given set of structures that reference each other by keys
- replaces the keys with their respective values.  Takes `Map k (v k)` and
converts into `Map k v'` where `v'` is the fixed point of `v`.

This is accomplished by functions
```haskell
type RefMap k v = Map k (v k)

tie  :: (Ord k, F.Foldable (Pre v), Fixpoint v) => RefMap k (Pre v) -> Either (TieError k) (Map k v)
tie' :: (Ord k, Fixpoint v) => RefMap k (Pre v) -> Map k v
```
One performs consistency checking, the other just ends with an error if a key
is missing in the map.

## Examples:

# Alice, Bob and the cat

Suppose that Alice loves Bob and her cat, Bob loves Alice and the cat loves
only itself. Imagine that we're reading this information from some kind of a
text file, and store the intermediate data into a list. We would like to create
a data structure which would contain these cyclic dependencies:

```haskell
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
```

# Circular lists

There is a well known task of converting a list into a circular structure with
no beginning/end:

```haskell
data DList a = DLNode (DList a) a (DList a)

mkDList :: [a] -> DList a
```

We can accomplish this using tie-knot by simply numbering the fields of a list
and then letting the library to tie the knot:

```haskell
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
```
