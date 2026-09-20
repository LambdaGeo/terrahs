-- | The map algebra from Sérgio Costa's Master's thesis
-- ("Integration of Functional Programming and Spatial Databases for
-- GIS Application Development", INPE, 2006, advised by Gilberto
-- Câmara) — Chapter 4, "A Generalized Map Algebra in TerraHS".
--
-- The central idea: instead of Tomlin's (1990) fixed raster model, the
-- thesis starts from a /coverage/ — in the OGC sense
-- (@DiscreteCFunction@) — a discrete function @cov :: E -> A@ from a
-- domain of geographic elements to a set of attribute values. This
-- generalizes Tomlin: his FOCAL and ZONAL operators (which only use
-- the fixed topological relations "touch" and "inside") become a
-- single 'spatial' operator, parameterized by ANY spatial predicate
-- (Egenhofer's: disjoint, touch, inside, overlap, contains,
-- intersects...).
--
-- Faithfully reproduces the @Coverages@\/@CoverageOps@ classes and
-- the worked numeric examples from the text (Figures 4.6 through
-- 4.10) — see the tests in @test\/Spec.hs@, which match the values in
-- the thesis exactly.
module TerraHS.Algebra.Coverage
  ( Coverage
  , newCov
  , fromPairs
  , evaluate
  , domain
  , numElems
  , values
  , covFun
    -- * Non-spatial operations (equivalent to Tomlin's LOCAL)
  , single
  , multiple
    -- * Spatial operations (generalize Tomlin's FOCAL and ZONAL)
  , select
  , compose
  , spatial
  ) where

-- | A coverage: a discrete function from a finite domain of
-- geographic elements (@a@) to a set of attribute values (@b@) —
-- @cov :: E -> A@. Corresponds to the @Coverages@ class and the
-- @Coverage@ type from the thesis (there, a generic type class with a
-- single instance; here, a concrete type directly — simpler and
-- equally faithful, since the thesis only ever used that one
-- instance).
data Coverage a b = Coverage (a -> b) [a]

-- | Builds a coverage from a domain and the function giving the value
-- of each element of the domain.
newCov :: [a] -> (a -> b) -> Coverage a b
newCov dom f = Coverage f dom

-- | Builds a coverage from a list of (domain element, value) pairs —
-- the shape data most naturally comes in once it's been read from a
-- file (a shapefile record's geometry paired with one of its
-- attributes, say). A thin convenience over 'newCov': every domain
-- element must be present as a key, since the resulting function
-- errors on one that isn't (mirroring 'evaluate', which reports a
-- missing element with 'Nothing' instead — use that if a partial
-- function isn't acceptable for your use).
--
-- >>> values (fromPairs [(1::Int, "a"), (2, "b")])
-- ["a","b"]
fromPairs :: Eq a => [(a, b)] -> Coverage a b
fromPairs prs = newCov (map fst prs) lookupOrError
  where
    lookupOrError x =
      maybe (error "fromPairs: domain element not found in the pairs given") id (lookup x prs)

-- | The coverage's domain — the geographic elements it covers.
domain :: Coverage a b -> [a]
domain (Coverage _ dom) = dom

-- | How many elements the domain has.
numElems :: Coverage a b -> Int
numElems = length . domain

-- | The coverage's function.
covFun :: Coverage a b -> (a -> b)
covFun (Coverage f _) = f

-- | The coverage's values, one per domain element, in the same
-- order.
values :: Coverage a b -> [b]
values c = map (covFun c) (domain c)

-- | The coverage's value at an element — 'Nothing' if the element is
-- not in the domain.
evaluate :: Eq a => Coverage a b -> a -> Maybe b
evaluate c o
  | o `elem` domain c = Just (covFun c o)
  | otherwise         = Nothing

-- * Non-spatial operations — equivalent to Tomlin's LOCAL operator

-- | Applies a single-argument function to every value of the
-- coverage — the unary case of the LOCAL operator. The result's
-- domain is the same as the input coverage.
--
-- >>> let c1 = newCov [1,2,3::Int] (\x -> [2,4,12] !! (x-1))
-- >>> values (single (^2) c1)
-- [4,16,144]
single :: (b -> c) -> Coverage a b -> Coverage a c
single g c1 = newCov (domain c1) (g . covFun c1)

-- | Combines several coverages position by position with a
-- multivalued function — the n-ary case of the LOCAL operator, e.g.
-- summing several maps. The result's domain is that of the reference
-- coverage; at each position, coverages whose domain doesn't contain
-- that element simply don't enter the list of combined values (the
-- same behaviour as the auxiliary @faux@ function in the thesis).
multiple :: Eq a => ([b] -> c) -> [Coverage a b] -> Coverage a d -> Coverage a c
multiple fn covs ref = newCov (domain ref) (\x -> fn (valuesAt x))
  where
    valuesAt x = [ v | c <- covs, Just v <- [evaluate c x] ]

-- * Spatial operations — generalize Tomlin's FOCAL and ZONAL

-- | Selects the elements of @c@'s domain that satisfy a spatial
-- predicate against a reference object.
select :: Coverage a b -> (a -> ref -> Bool) -> ref -> Coverage a b
select c predicate ref = newCov selectedDomain (covFun c)
  where
    selectedDomain = [ loc | loc <- domain c, predicate loc ref ]

-- | Aggregates a coverage's values with a multivalued function — e.g.
-- the sum or mean of the selected values.
compose :: ([b] -> r) -> Coverage a b -> r
compose f c = f (values c)

-- | Selection followed by composition — the general spatial operator
-- that generalizes both FOCAL (the @touch@\/neighborhood predicate)
-- and ZONAL (the @inside@ predicate) from Tomlin into a single
-- operator, parameterized by the spatial predicate. For each element
-- of the reference coverage, it selects the elements of the input
-- coverage that satisfy the predicate, and aggregates the selected
-- values.
spatial :: ([b] -> b) -> Coverage a b -> (a -> ref -> Bool) -> Coverage ref b -> Coverage ref b
spatial fn c predicate covRef =
  newCov (domain covRef) (\x -> compose fn (select c predicate x))
