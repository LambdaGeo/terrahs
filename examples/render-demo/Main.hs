-- | render-demo: the straightforward recipe for "load a shapefile of
-- real municipalities, draw it, save a PNG" -- no comparison, no
-- timing. Reads the same real IBGE Malha Municipal for Maranhão
-- ('examples/data/ibge/', 217 municipalities) that
-- 'ibge-road-join-demo' and 'ibge-map-demo' do.
--
-- 'TerraHS.Geometry.Simplify.simplifyPolygon' still runs here, quietly,
-- as just one more step in the pipeline (real municipal boundaries
-- have far more detail than a small PNG can show, so skipping it
-- would make this take tens of seconds instead of under one) -- but
-- unlike 'ibge-map-demo', this doesn't render the *original* geometry
-- at all, measure anything, or compare the two: it's the version to
-- reach for when all that's wanted is the map. See 'ibge-map-demo'
-- for the side-by-side original-vs-simplified comparison, with real
-- numbers, that justifies this step.
module Main (main) where

import Data.List (foldl')
import System.Directory (createDirectoryIfMissing)

import TerraHS
import TerraHS.Render.PNG (renderPolygonFillWith, PixelRGB8 (..))

outFile :: FilePath
outFile = "examples/render-demo/out/maranhao.png"

-- | A small fixed palette, picked by a simple hash of each
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

main :: IO ()
main = do
  result <- readVectorFile "examples/data/ibge/MA_Municipios_2025.shp"
  case result of
    Left err -> putStrLn ("failed to read the shapefile: " ++ err)
    Right feats -> do
      let pairs   = [ (poly, name) | (poly, attrs) <- asPolygonPairs feats, Just name <- [attrAs "NM_MUN" attrs] ]
          box     = foldr1 union (map (envelope . fst) pairs)
          -- Half a render pixel -- see ibge-map-demo for why that's
          -- the natural tolerance at this render scale (renderScalePx
          -- below matches TerraHS.Render.PNG's internal scalePx).
          renderScalePx = 80 :: Double
          epsilon       = 1 / (2 * renderScalePx)
          cov     = fromPairs [ (simplifyPolygon epsilon poly, name) | (poly, name) <- pairs ]
      createDirectoryIfMissing True "examples/render-demo/out"
      renderPolygonFillWith colorFor outFile box cov
      putStrLn ("PNG written to " ++ outFile)
