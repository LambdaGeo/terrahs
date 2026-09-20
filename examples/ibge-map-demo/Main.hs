-- | ibge-map-demo: plots the real IBGE Malha Municipal (Maranhao,
-- 2025) as a PNG -- using 'TerraHS.Render.PNG.renderPolygonFillWith'
-- to draw each municipality's actual shape (via 'pointInPolygon'),
-- not just its bounding box like 'examples/diffusion-demo' and
-- 'examples/fire-demo' do for their synthetic squares. That
-- distinction matters here: a bounding box would draw nothing but
-- overlapping rectangles for a real, irregular coastline.
--
-- The interesting part is what it costs, and what
-- 'TerraHS.Geometry.Simplify.simplifyPolygon' (Ramer-Douglas-Peucker)
-- does about it: rendering the *original* geometry -- one
-- municipality alone (Amarante do Maranhao) has over 17,000 vertices
-- -- takes tens of seconds, almost all of it 'pointInPolygon' ray
-- casting against vertices that, at the resolution a ~550x740px PNG
-- can even show, are individually invisible: a bend smaller than half
-- a pixel cannot change which color a pixel ends up. Simplifying to
-- that tolerance *first* removes the vertices that could never have
-- mattered to this render, and the two images that come out are
-- visually indistinguishable -- so this both renders the map and
-- demonstrates, with real numbers, why "simplify to the scale you're
-- about to draw at" is worth doing before a costly geometric
-- operation, not just for rendering.
module Main (main) where

import Data.List (foldl')
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)

import TerraHS
import TerraHS.Render.PNG (renderPolygonFillWith, PixelRGB8 (..))

outDir :: FilePath
outDir = "examples/ibge-map-demo/out"

-- | A small fixed palette, picked by a simple hash of the
-- municipality's name -- not meaningful data, just enough to make
-- neighbouring municipalities visually distinct without hand-tuning
-- 217 colours.
palette :: [PixelRGB8]
palette =
  [ PixelRGB8 141 211 199
  , PixelRGB8 255 255 179
  , PixelRGB8 190 186 218
  , PixelRGB8 251 128 114
  , PixelRGB8 128 177 211
  , PixelRGB8 253 180 98
  , PixelRGB8 179 222 105
  ]

colorFor :: String -> PixelRGB8
colorFor name = palette !! (h `mod` length palette)
  where
    h = foldl' (\acc c -> acc * 31 + fromEnum c) 0 name

totalVertices :: Coverage Polygon v -> Int
totalVertices cov = sum [ length (polygonRing p) | p <- domain cov ]

seconds :: Integer -> Integer -> Double
seconds t0 t1 = fromIntegral (t1 - t0) / 1e12

main :: IO ()
main = do
  createDirectoryIfMissing True outDir
  t0 <- getCPUTime
  result <- readVectorFile "examples/data/ibge/MA_Municipios_2025.shp"
  case result of
    Left err -> putStrLn ("failed to read the IBGE shapefile: " ++ err)
    Right feats -> do
      let pairs = [ (poly, name) | (poly, attrs) <- asPolygonPairs feats, Just name <- [attrAs "NM_MUN" attrs] ]
          cov    = fromPairs pairs :: Coverage Polygon String
          box    = foldr1 union (map (envelope . fst) pairs)

          -- TerraHS.Render.PNG draws at 80 pixels per data unit
          -- (degree, here) -- half of one pixel is the largest
          -- deviation that could never change a rendered pixel's
          -- colour, so it's the natural simplification tolerance for
          -- drawing at that resolution.
          renderScalePx = 80 :: Double
          epsilon       = 1 / (2 * renderScalePx)
          simplifiedCov = fromPairs [ (simplifyPolygon epsilon poly, name) | (poly, name) <- pairs ]

      putStrLn "== ibge-map-demo: the real Maranhao municipal map, original vs. simplified =="
      putStrLn ""
      printf "%d polygon parts (217 municipalities, some split by islands)\n" (length pairs)
      printf "vertices, original:   %d\n" (totalVertices cov)
      printf "vertices, simplified: %d (epsilon = %.5f degrees, ~half a render pixel)\n"
             (totalVertices simplifiedCov) epsilon
      t1 <- getCPUTime
      printf "load + build both coverages: %.2fs\n" (seconds t0 t1)

      putStrLn ""
      renderPolygonFillWith colorFor (outDir ++ "/maranhao-original.png") box cov
      t2 <- getCPUTime
      printf "render, original geometry:   %.2fs\n" (seconds t1 t2)

      renderPolygonFillWith colorFor (outDir ++ "/maranhao-simplified.png") box simplifiedCov
      t3 <- getCPUTime
      printf "render, simplified geometry: %.2fs\n" (seconds t2 t3)

      putStrLn ""
      putStrLn ("PNGs written to " ++ outDir ++ "/maranhao-original.png and maranhao-simplified.png --")
      putStrLn "compare them: the simplification is not supposed to be visible at this resolution."
