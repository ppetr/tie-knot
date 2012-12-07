# tie-knot

"Ties the knot" on a given set of structures that reference each other by keys
- replaces the keys with their respective values.  Takes `Map k (v k)` and
converts into `Map k v'` where v' is the fixed point of `v`.

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
instance Fixpoint (DList a) where
  data Pre (DList a) t = DLNode' t a t
  inject ~(DLNode' u x v) = DLNode u x v
  project ~(DLNode u x v) = DLNode' u x v
instance Functor (Pre (DList a)) where
    fmap = T.fmapDefault
instance T.Traversable (Pre (DList n)) where
    traverse f (DLNode' u n v) = DLNode' <$> f u <*> pure n <*> f v
instance F.Foldable (Pre (DList n)) where
    foldMap = T.foldMapDefault

mkDList :: [a] -> DList a
mkDList xs = fromJust . Map.lookup 0 . tie' $ dict
  where
    dict = Map.fromList
            . map (\(i, x) -> (i, DLNode' (pre i) x (nxt i)))
            . zip [0..] $ xs
    n = length xs
    pre i = (i + n - 1) `rem` n
    nxt i = (i + 1) `rem` n
```
