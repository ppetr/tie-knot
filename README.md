tie-knot
========

"Ties the knot" on a given set of structures that reference each other by keys - replaces the keys with their respective values.
Takes `Map k (v k)` and converts into `Map k v'` where v' is the fixed point of `v`.
