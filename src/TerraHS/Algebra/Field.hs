-- | 'Field': a 2D grid of values — the modern replacement for the old
-- @TeRaster@, without the TerraLib dependency.
--
-- Together with 'TerraHS.Algebra.Funct', this lets you express the
-- entire classic taxonomy of map algebra operators (Tomlin, 1990):
--
-- * __Local__ — 'lift1'\/'lift2' from 'TerraHS.Algebra.Funct.Funct':
--   cell by cell, without looking at neighbors.
-- * __Focal__ — 'focal3x3': aggregates the 3x3 neighborhood of each
--   cell. This is the operator the comments on the original
--   'TerraHS.Algebra.Funct.lift3'\/'TerraHS.Algebra.Funct.lift4' were
--   pointing at ("for convolution (3x3 kernel)") but never
--   implemented.
-- * __Zonal__ — 'zonalWith': aggregates values grouped by zone.
-- * __Global__ — 'foldField' and its derived functions ('sumField',
--   'meanField', ...): aggregates the whole field to a single value.
module TerraHS.Algebra.Field
  ( Field (..)
  , mkField
  , constField
  , fieldAt
    -- * Focal operators
  , focal3x3
    -- * Zonal operators
  , zonalWith
    -- * Global operators
  , foldField
  , sumField
  , meanField
  , maxField
  , minField
  ) where

import Data.List (nub)

import TerraHS.Algebra.Funct (Funct (..))

-- | A rectangular grid of values, @fieldRows@ x @fieldCols@, stored
-- as a list of rows.
data Field a = Field
  { fieldRows  :: Int
  , fieldCols  :: Int
  , fieldCells :: [[a]]
  } deriving (Eq, Show)

-- | Builds a 'Field' from a list of rows, checking that they are all
-- the same length. 'Nothing' if the list is empty or the rows have
-- different lengths.
mkField :: [[a]] -> Maybe (Field a)
mkField [] = Nothing
mkField rows@(firstRow : _)
  | all ((== ncols) . length) rows = Just (Field nrows ncols rows)
  | otherwise                      = Nothing
  where
    nrows = length rows
    ncols = length firstRow

-- | A constant field — every cell holding the same value. This is
-- the equivalent of 'TerraHS.Algebra.Funct.lift0' for this type,
-- exposed separately because 'Field' needs to know the dimensions
-- (which 'lift0' alone has no way to carry).
constField :: Int -> Int -> a -> Field a
constField rows cols v = Field rows cols (replicate rows (replicate cols v))

-- | The value at the given row\/column, or 'Nothing' if it's outside
-- the field's bounds.
fieldAt :: Field a -> Int -> Int -> Maybe a
fieldAt (Field nrows ncols cells) r c
  | r < 0 || r >= nrows || c < 0 || c >= ncols = Nothing
  | otherwise                                   = Just (cells !! r !! c)

instance Funct Field where
  lift1 f (Field r c cells) = Field r c (map (map f) cells)

  lift2 g (Field r1 c1 cells1) (Field r2 c2 cells2)
    | r1 /= r2 || c1 /= c2 =
        error "Funct Field: lift2 requires two fields of the same dimensions"
    | otherwise = Field r1 c1 (zipWith (zipWith g) cells1 cells2)

-- * Focal operator

-- | Aggregates the 3x3 neighborhood of each cell (the cell itself
-- plus its 8 neighbors) with the given aggregation function. Cells
-- outside the field's boundary use the @edge@ value.
--
-- >>> Just f = mkField [[1,2,3],[4,5,6],[7,8,9]]
-- >>> fieldAt (focal3x3 0 sum f) 1 1   -- the center cell: sums everything
-- Just 45.0
focal3x3 :: a -> ([a] -> b) -> Field a -> Field b
focal3x3 edge aggregate field@(Field nrows ncols _) =
  Field nrows ncols
    [ [ aggregate (neighborhood r c) | c <- [0 .. ncols - 1] ]
    | r <- [0 .. nrows - 1]
    ]
  where
    neighborhood r c =
      [ maybe edge id (fieldAt field (r + dr) (c + dc))
      | dr <- [-1, 0, 1]
      , dc <- [-1, 0, 1]
      ]

-- * Zonal operator

-- | Groups the values of @valueField@ by the corresponding zone in
-- @zoneField@ (same dimensions for both fields) and aggregates each
-- group — e.g. summing population by county, given a population
-- field and a field holding each cell's county code.
zonalWith :: Eq z => ([a] -> b) -> Field z -> Field a -> [(z, b)]
zonalWith aggregate (Field zr zc zoneCells) (Field vr vc valueCells)
  | zr /= vr || zc /= vc =
      error "zonalWith: the zone field and the value field have different dimensions"
  | otherwise =
      [ (zone, aggregate [ v | (z, v) <- pairs, z == zone ])
      | zone <- nub zones
      ]
  where
    zones = concat zoneCells
    values = concat valueCells
    pairs = zip zones values

-- * Global operators

-- | Aggregates every cell of the field with a fold function — the
-- basis for all the global operators below.
foldField :: (b -> a -> b) -> b -> Field a -> b
foldField step z0 (Field _ _ cells) = foldl step z0 (concat cells)

-- | The sum of every cell.
sumField :: Num a => Field a -> a
sumField = foldField (+) 0

-- | The mean of every cell.
meanField :: Fractional a => Field a -> a
meanField f = sumField f / fromIntegral (fieldRows f * fieldCols f)

-- | The largest value in the field.
maxField :: Ord a => Field a -> a
maxField (Field _ _ cells) = maximum (concat cells)

-- | The smallest value in the field.
minField :: Ord a => Field a -> a
minField (Field _ _ cells) = minimum (concat cells)
