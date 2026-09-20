-- | Minimal PNG rendering for 'comonad-ca-demo', so the two
-- simulations can be looked at instead of just read as ASCII/text.
-- Uses @JuicyPixels@ (pure Haskell, no FFI, in keeping with the rest
-- of TerraHS) -- scoped to this one executable, same as
-- @comonad@\/@contravariant@, so the core library and the other
-- examples are unaffected.
module Render
  ( renderLifeGrid
  , renderLifeStrip
  , renderZones
  , renderZonesStrip
  ) where

import Codec.Picture (PixelRGB8 (..), generateImage, writePng)

-- * Game of Life: a boolean grid, one cell = one square block.

cellPx :: Int
cellPx = 20

-- | A single generation's pixel function: @(x0, y0)@ is the
-- top-left cell of the window (in grid coordinates); @alive@ answers
-- whether a given grid cell is alive.
gridPixel :: (Int, Int) -> ((Int, Int) -> Bool) -> Int -> Int -> PixelRGB8
gridPixel (x0, y0) alive px py
  | onGridLine            = gridLine
  | alive (x0 + cx, y0 + cy) = black
  | otherwise              = white
  where
    cx = px `div` cellPx
    cy = py `div` cellPx
    onGridLine = px `mod` cellPx == 0 || py `mod` cellPx == 0
    black    = PixelRGB8 20 20 20
    white    = PixelRGB8 245 245 245
    gridLine = PixelRGB8 200 200 200

-- | Renders one generation window as a PNG.
renderLifeGrid :: FilePath -> (Int, Int) -> (Int, Int) -> ((Int, Int) -> Bool) -> IO ()
renderLifeGrid path (x0, y0) (x1, y1) alive = writePng path img
  where
    w   = (x1 - x0 + 1) * cellPx
    h   = (y1 - y0 + 1) * cellPx
    img = generateImage (gridPixel (x0, y0) alive) w h

-- | Renders several generations side by side in one strip PNG, so the
-- whole run can be seen at a glance.
renderLifeStrip :: FilePath -> (Int, Int) -> (Int, Int) -> [(Int, Int) -> Bool] -> IO ()
renderLifeStrip path (x0, y0) (x1, y1) frames = writePng path img
  where
    frameW = (x1 - x0 + 1) * cellPx
    frameH = (y1 - y0 + 1) * cellPx
    gap    = 6
    img    = generateImage pixelAt (length frames * (frameW + gap) - gap) frameH
    pixelAt px py
      | localX >= frameW = PixelRGB8 255 255 255 -- the gap between frames
      | otherwise         = gridPixel (x0, y0) (frames !! frameIdx) localX py
      where
        (frameIdx, localX) = px `divMod` (frameW + gap)

-- * Diffusion: zones drawn at their real geometric position.

-- | A zone to draw: its bounding box in data units (minX, minY, maxX,
-- maxY), and whether it's infected.
type ZoneBox = ((Double, Double, Double, Double), Bool)

scalePx :: Double
scalePx = 80

-- | Renders one time step: every zone's bounding box, filled red if
-- infected and light grey otherwise, with a dark border -- positioned
-- and sized from the real coordinates (via 'TerraHS.Geometry.envelope'
-- upstream), not a schematic diagram.
renderZones :: FilePath -> (Double, Double, Double, Double) -> [ZoneBox] -> IO ()
renderZones path canvasBBox zones = writePng path img
  where
    (cx0, _, cx1, cy1) = canvasBBox
    w   = round ((cx1 - cx0) * scalePx)
    h   = round (cy1 * scalePx)
    img = generateImage (zonesPixel canvasBBox zones) w h

zonesPixel :: (Double, Double, Double, Double) -> [ZoneBox] -> Int -> Int -> PixelRGB8
zonesPixel (cx0, _, _, cy1) zones px py =
  case [ infected | ((minX, minY, maxX, maxY), infected) <- zones, inside minX minY maxX maxY ] of
    (infected : _)
      | onBorder minX' minY' maxX' maxY' -> PixelRGB8 40 40 40
      | infected                         -> PixelRGB8 200 60 60
      | otherwise                        -> PixelRGB8 210 210 220
      where
        ((minX', minY', maxX', maxY'), _) =
          head [ z | z@((minX, minY, maxX, maxY), _) <- zones, inside minX minY maxX maxY ]
    _ -> PixelRGB8 255 255 255
  where
    -- Pixel (px, py) back to data coordinates -- y is flipped, since
    -- image row 0 is the top but geometry y grows upward.
    x = cx0 + fromIntegral px / scalePx
    y = cy1 - fromIntegral py / scalePx
    inside minX minY maxX maxY = x >= minX && x <= maxX && y >= minY && y <= maxY
    borderPx = 1.5 / scalePx
    inBand v lo hi = v - lo < borderPx || hi - v < borderPx
    onBorder minX minY maxX maxY = inBand x minX maxX || inBand y minY maxY

-- | Several time steps side by side in one strip PNG.
renderZonesStrip :: FilePath -> (Double, Double, Double, Double) -> [[ZoneBox]] -> IO ()
renderZonesStrip path canvasBBox steps = writePng path img
  where
    (cx0, _, cx1, cy1) = canvasBBox
    frameW = round ((cx1 - cx0) * scalePx)
    frameH = round (cy1 * scalePx)
    gap    = 10
    img    = generateImage pixelAt (length steps * (frameW + gap) - gap) frameH
    pixelAt px py
      | localX >= frameW = PixelRGB8 255 255 255
      | otherwise         = zonesPixel canvasBBox (steps !! frameIdx) localX py
      where
        (frameIdx, localX) = px `divMod` (frameW + gap)
