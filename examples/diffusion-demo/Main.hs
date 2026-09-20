-- | diffusion-demo: a diffusion\/contamination spread model over real
-- polygon geometry -- the subject matter of the source paper "Modelos
-- dinamicos espaciais em programacao funcional" (Costa et al.,
-- WORCAP/INPE), reformulated as a co-Kleisli function over
-- 'Control.Comonad.Store.Store' instead of the paper's hand-written
-- neighbourhood-filtering-then-mapping recursion ('sim').
--
-- One of three examples split out of what used to be a single
-- @comonad-ca-demo@ (Life, diffusion, fire); this one and
-- @fire-demo@ share the same six-zone world ('ZoneWorld', under
-- @examples/common@) and the same adjacency predicate -- only the
-- rule and the state type differ between the two, which is easiest to
-- see with each in its own file rather than interleaved in one.
module Main (main) where

import Control.Comonad (extract)
import Control.Comonad.Store (Store, peek)
import Data.List (intercalate, nub, sort)
import System.Directory (createDirectoryIfMissing)

import TerraHS
import TerraHS.CA (neighborValues, runCA)
import TerraHS.Render.PNG (renderCoverage, renderCoverageSteps)

import StoreBridge (storeAt, storeToCoverage)
import ZoneWorld (Zone (..), zones, adjacent, canvasBBox, zoneCoverage)

outDir :: FilePath
outDir = "examples/diffusion-demo/out"

-- | The diffusion rule: a zone is infected next turn if it already is,
-- or if any zone adjacent to it (per 'ZoneWorld.adjacent') is
-- infected now. 'neighborValues' -- from 'TerraHS.CA' -- reads every
-- adjacent zone's value directly; a diffused-or-not state is already
-- a 'Bool', so "any neighbour infected" is just 'or' over that list.
diffusionRule :: Store Zone Bool -> Bool
diffusionRule w = extract w || or (neighborValues adjacent zones w)

-- | Builds the initial state as an ordinary 'Coverage' first (the
-- shape TerraHS data naturally comes in), then bridges it into a
-- 'Store' with 'storeAt'.
seedDiffusion :: String -> Store Zone Bool
seedDiffusion startId = storeAt seedCoverage (head zones)
  where
    seedCoverage = newCov zones (\z -> zoneId z == startId)

infectedIds :: Store Zone Bool -> [String]
infectedIds w = sort [ zoneId z | z <- zones, peek z w ]

main :: IO ()
main = do
  putStrLn "== diffusion-demo: diffusion over polygon geometry, seeded at Z1 =="
  putStrLn "(adjacency = TerraHS's own 'intersects', via TerraHS.CA's notSelf/touchesVia -- see ZoneWorld.adjacent)"
  putStrLn ""
  let steps = runCA diffusionRule (seedDiffusion "Z1")
  mapM_
    (\(n, w) -> putStrLn ("t" ++ show n ++ ": " ++ intercalate ", " (infectedIds w)))
    (zip [0 :: Int ..] (take 5 steps))

  putStrLn ""
  putStrLn "Expected spread, by hand (BFS over the adjacency listed in ZoneWorld, corner"
  putStrLn "touches included): t0={Z1} -> t1={Z1,Z2,Z4} -> t2={Z1,Z2,Z3,Z4,Z5}"
  putStrLn "                 -> t3={Z1,Z2,Z3,Z4,Z5,Z6} -> t4=same (fixed point)."
  let actual   = map infectedIds (take 5 steps)
      expected = [ ["Z1"], ["Z1", "Z2", "Z4"], ["Z1", "Z2", "Z3", "Z4", "Z5"]
                 , ["Z1", "Z2", "Z3", "Z4", "Z5", "Z6"], ["Z1", "Z2", "Z3", "Z4", "Z5", "Z6"] ]
  putStrLn ("Check -- matches the hand-traced spread above: " ++ show (actual == expected))

  -- Bridging back into TerraHS's own Coverage type: the simulation's
  -- final state, as a Coverage Zone Bool, used exactly like any other
  -- coverage (values/domain/select/compose all apply to it as-is).
  putStrLn ""
  let finalCov  = storeToCoverage zones (steps !! 3)
      infectedN = length (filter id (values finalCov))
  putStrLn ("Bridged back to a Coverage: " ++ show (numElems finalCov) ++ " zones, "
             ++ show infectedN ++ " infected (via 'values', same as any TerraHS Coverage).")
  putStrLn ("Sanity: no zone id is lost or duplicated in the bridge: "
             ++ show (sort (nub (map zoneId (domain finalCov))) == sort (map zoneId zones)))

  -- PNGs: each zone drawn at its real geometric position (via
  -- 'envelope'), red once infected -- one per time step, plus a strip
  -- with all of them side by side. 'renderCoverage' only knows about
  -- 'Coverage', not about 'Zone', so each step is turned into a plain
  -- @Coverage Polygon Bool@ first, via 'ZoneWorld.zoneCoverage'.
  let pngSteps = take 5 steps
  createDirectoryIfMissing True outDir
  mapM_
    (\(n, w) -> renderCoverage (outDir ++ "/t" ++ show n ++ ".png") canvasBBox (zoneCoverage w))
    (zip [0 :: Int ..] pngSteps)
  renderCoverageSteps (outDir ++ "/strip.png") canvasBBox (map zoneCoverage pngSteps)
  putStrLn ""
  putStrLn ("PNGs written to " ++ outDir ++ "/t0.png .. t4.png, and strip.png")
