-- | TerraHS: a purely functional environment for geospatial
-- programming.
--
-- A from-scratch rewrite of the original TerraHS (2006-2009), which
-- depended on the C++ TerraLib. This version has no heavy external
-- dependencies: geometry, topology, and map algebra are implemented
-- in plain Haskell (or with pure-Haskell libraries only, in the case
-- of the I/O layer).
module TerraHS
  ( module TerraHS.Geometry
  , module TerraHS.Geometry.Topology
  , module TerraHS.Geometry.Simplify
  , module TerraHS.IO.WKT
  , module TerraHS.IO.GeoJSON
  , module TerraHS.IO.Shapefile
  , module TerraHS.IO.Dbf
  , module TerraHS.IO.Vector
  , module TerraHS.Algebra.Coverage
  , module TerraHS.Algebra.Funct
  , module TerraHS.Algebra.Field
  ) where

import TerraHS.Geometry
import TerraHS.Geometry.Topology
import TerraHS.Geometry.Simplify
import TerraHS.IO.WKT
import TerraHS.IO.GeoJSON
import TerraHS.IO.Shapefile
import TerraHS.IO.Dbf
import TerraHS.IO.Vector
import TerraHS.Algebra.Coverage
import TerraHS.Algebra.Funct
import TerraHS.Algebra.Field
