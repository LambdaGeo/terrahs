-- | Minimal PNG rendering for 'TerraHS.Algebra.Coverage.Coverage'
-- values and integer grids, using @JuicyPixels@ (pure Haskell, no
-- FFI -- in keeping with the rest of TerraHS).
--
-- This is deliberately its own library component
-- (@terrahs-render@ in @terrahs.cabal@), not part of the main
-- @terrahs@ library: rendering pulls in @JuicyPixels@ (and
-- transitively @zlib@), a real dependency that most uses of TerraHS
-- (reading a shapefile, running a map-algebra pipeline) have no
-- reason to carry. Anything that wants PNG output -- an example, a
-- teaching notebook, a future @terrahs-viewer@ -- depends on
-- @terrahs-render@ explicitly instead.
--
-- Two things it knows how to draw:
--
--   * A @'Coverage' a v@ where @a@ is a 'Geometry' ('Point', 'Line'
--     or 'Polygon') -- each element drawn at its real bounding box
--     (via 'envelope', the same function 'intersects' uses
--     internally), filled by a colour derived from its value @v@.
--     'renderCoverage'\/'renderCoverageSteps' cover the common
--     two-state case (@v ~ 'Bool'@, red\/grey); 'renderCoverageWith'
--     takes an explicit @v -> 'PixelRGB8'@ for anything with more
--     states (a fire model's forest\/burning\/burned, say). Any
--     coverage of this shape works, not tied to any one example's
--     domain type.
--   * A plain @(Int, Int) -> Bool@ grid (no geometry involved) -- for
--     grid-based cellular automata such as Conway's Game of Life,
--     where the domain is just integer coordinates.
--
-- Each has a single-frame and a multi-frame ("strip", frames side by
-- side) version, since a simulation's whole run is usually more
-- useful to look at than any single step.
module TerraHS.Render.PNG
  ( -- * Coverage of geometry -> PNG (bounding box, cheap and approximate)
    renderCoverage
  , renderCoverageSteps
  , renderCoverageWith
  , renderCoverageStepsWith
  , coverageBBox
    -- * Coverage of Polygon -> PNG (real shape)
  , renderPolygonFillWith
    -- * Integer grid -> PNG
  , renderGrid
  , renderGridSteps
    -- * Colour
  , PixelRGB8 (..)
  ) where

import Codec.Picture (PixelRGB8 (..), generateImage, writePng)

import TerraHS.Algebra.Coverage (Coverage, domain, covFun)
import TerraHS.Geometry (Geometry (..), BBox (..), union)
import TerraHS.Geometry.Coord (Coord (..))
import TerraHS.Geometry.Point (Point (..))
import TerraHS.Geometry.Polygon (Polygon)
import TerraHS.Geometry.Topology (pointInPolygon)

-- * Coverage of geometry -> PNG

scalePx :: Double
scalePx = 80

-- | The smallest box covering every element of a coverage's domain --
-- a convenient default 'canvas' for 'renderCoverage'\/'renderCoverageSteps'
-- when the caller doesn't need several frames to share one fixed box.
coverageBBox :: Geometry a => Coverage a b -> BBox
coverageBBox cov = foldr1 union (map envelope (domain cov))

-- | Renders one 'Coverage a Bool' as a PNG: each element filled at
-- its real bounding box, red when its value is 'True' and light grey
-- otherwise, with a dark border. @canvas@ fixes the box all frames
-- share (pass the same one across a call to 'renderCoverageSteps' so
-- frames line up); use 'coverageBBox' for a coverage's own box, or
-- the union of several via 'TerraHS.Geometry.union' to cover a whole
-- run. A specialization of 'renderCoverageWith' for the common
-- two-colour case -- for a model with more than two states (three
-- fire states, say), use 'renderCoverageWith' with your own colour
-- function instead.
renderCoverage :: Geometry a => FilePath -> BBox -> Coverage a Bool -> IO ()
renderCoverage = renderCoverageWith boolColor

-- | Several time steps of a boolean coverage side by side, in one
-- strip PNG.
renderCoverageSteps :: Geometry a => FilePath -> BBox -> [Coverage a Bool] -> IO ()
renderCoverageSteps = renderCoverageStepsWith boolColor

boolColor :: Bool -> PixelRGB8
boolColor True  = PixelRGB8 200 60 60  -- red
boolColor False = PixelRGB8 210 210 220 -- light grey

-- | The general form behind 'renderCoverage': any @'Coverage' a v@
-- (still @a@ a 'Geometry'), given a function from a value @v@ to the
-- colour it's drawn in. Use this directly for a model with more than
-- two states -- a fire model's @Forest@\/@Burning@\/@Burned@, say.
renderCoverageWith :: Geometry a => (v -> PixelRGB8) -> FilePath -> BBox -> Coverage a v -> IO ()
renderCoverageWith color path canvas cov = writePng path img
  where
    (w, h) = canvasPixelSize canvas
    img    = generateImage (coveragePixel color canvas (frameOf cov)) w h

-- | The general form behind 'renderCoverageSteps'.
renderCoverageStepsWith :: Geometry a => (v -> PixelRGB8) -> FilePath -> BBox -> [Coverage a v] -> IO ()
renderCoverageStepsWith color path canvas covs = writePng path img
  where
    (frameW, frameH) = canvasPixelSize canvas
    gap              = 10
    img              = generateImage pixelAt (length covs * (frameW + gap) - gap) frameH
    pixelAt px py
      | localX >= frameW = PixelRGB8 255 255 255
      | otherwise         = coveragePixel color canvas (frameOf (covs !! frameIdx)) localX py
      where
        (frameIdx, localX) = px `divMod` (frameW + gap)

-- | A coverage's elements, reduced to what rendering needs: each
-- element's bounding box and value.
frameOf :: Geometry a => Coverage a v -> [(BBox, v)]
frameOf cov = [ (envelope e, covFun cov e) | e <- domain cov ]

canvasPixelSize :: BBox -> (Int, Int)
canvasPixelSize (BBox minX minY maxX maxY) =
  (round ((maxX - minX) * scalePx), round ((maxY - minY) * scalePx))

coveragePixel :: (v -> PixelRGB8) -> BBox -> [(BBox, v)] -> Int -> Int -> PixelRGB8
coveragePixel color (BBox cx0 _ _ cy1) frame px py =
  case [ v | (b, v) <- frame, inside b ] of
    (v : _)
      | onBorder box' -> PixelRGB8 40 40 40
      | otherwise     -> color v
      where
        box' = head [ b | (b, _) <- frame, inside b ]
    _ -> PixelRGB8 255 255 255
  where
    -- Pixel (px, py) back to data coordinates -- y is flipped, since
    -- image row 0 is the top but geometry y grows upward.
    x = cx0 + fromIntegral px / scalePx
    y = cy1 - fromIntegral py / scalePx
    inside (BBox minX minY maxX maxY) = x >= minX && x <= maxX && y >= minY && y <= maxY
    borderPx = 1.5 / scalePx
    inBand v' lo hi = v' - lo < borderPx || hi - v' < borderPx
    onBorder (BBox minX minY maxX maxY) = inBand x minX maxX || inBand y minY maxY

-- | Renders a @'Coverage' 'Polygon' v@ at its /real/ shape, not just
-- its bounding box: every pixel is tested with 'pointInPolygon' (ray
-- casting, exact) against whichever polygon's bounding box contains
-- it first (the cheap broad-phase filter every polygon already has to
-- pass before the exact test runs -- the same two-step pattern
-- 'TerraHS.Geometry.Topology.crossesPolygon' uses). Where
-- 'renderCoverageWith' only ever draws rectangles (fine for the
-- synthetic squares in @diffusion-demo@\/@fire-demo@, wrong for anything with a
-- real, irregular boundary -- a coastline, an administrative border),
-- this draws the actual shape. No border is drawn (tracing a real
-- boundary pixel-by-pixel this way, for a polygon with tens of
-- thousands of vertices, is the expensive part -- filling is cheap by
-- comparison since most pixels reject on the bounding box alone);
-- give adjacent elements visibly different colours instead.
renderPolygonFillWith :: (v -> PixelRGB8) -> FilePath -> BBox -> Coverage Polygon v -> IO ()
renderPolygonFillWith color path canvas cov = writePng path img
  where
    (w, h) = canvasPixelSize canvas
    frame  = [ (envelope poly, poly, covFun cov poly) | poly <- domain cov ]
    img    = generateImage (polygonPixel color canvas frame) w h

polygonPixel :: (v -> PixelRGB8) -> BBox -> [(BBox, Polygon, v)] -> Int -> Int -> PixelRGB8
polygonPixel color (BBox cx0 _ _ cy1) frame px py =
  case [ v | (b, poly, v) <- frame, inside b, pointInPolygon here poly ] of
    (v : _) -> color v
    []      -> PixelRGB8 255 255 255
  where
    -- Pixel (px, py) back to data coordinates -- y is flipped, since
    -- image row 0 is the top but geometry y grows upward.
    x    = cx0 + fromIntegral px / scalePx
    y    = cy1 - fromIntegral py / scalePx
    here = Point (Coord x y)
    inside (BBox minX minY maxX maxY) = x >= minX && x <= maxX && y >= minY && y <= maxY

-- * Integer grid -> PNG

cellPx :: Int
cellPx = 20

-- | A single generation's pixel function: @(x0, y0)@ is the
-- top-left cell of the window (in grid coordinates); @alive@ answers
-- whether a given grid cell is alive.
gridPixel :: (Int, Int) -> ((Int, Int) -> Bool) -> Int -> Int -> PixelRGB8
gridPixel (x0, y0) alive px py
  | onGridLine               = gridLine
  | alive (x0 + cx, y0 + cy) = black
  | otherwise                = white
  where
    cx = px `div` cellPx
    cy = py `div` cellPx
    onGridLine = px `mod` cellPx == 0 || py `mod` cellPx == 0
    black    = PixelRGB8 20 20 20
    white    = PixelRGB8 245 245 245
    gridLine = PixelRGB8 200 200 200

-- | Renders one window of a boolean grid as a PNG.
renderGrid :: FilePath -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> Bool) -> IO ()
renderGrid path (x0, y0) (x1, y1) alive = writePng path img
  where
    w   = (x1 - x0 + 1) * cellPx
    h   = (y1 - y0 + 1) * cellPx
    img = generateImage (gridPixel (x0, y0) alive) w h

-- | Several generations of a boolean grid side by side in one strip
-- PNG.
renderGridSteps :: FilePath -> (Int, Int) -> (Int, Int) -> [(Int, Int) -> Bool] -> IO ()
renderGridSteps path (x0, y0) (x1, y1) frames = writePng path img
  where
    frameW = (x1 - x0 + 1) * cellPx
    frameH = (y1 - y0 + 1) * cellPx
    gap    = 6
    img    = generateImage pixelAt (length frames * (frameW + gap) - gap) frameH
    pixelAt px py
      | localX >= frameW = PixelRGB8 255 255 255
      | otherwise         = gridPixel (x0, y0) (frames !! frameIdx) localX py
      where
        (frameIdx, localX) = px `divMod` (frameW + gap)
