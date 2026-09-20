-- | fire-demo: a forest-fire cellular automaton (forest \/ burning \/
-- burned) over the same six-zone polygon world as @diffusion-demo@ --
-- showing a second model built from the exact same pieces
-- ('TerraHS.CA', 'ZoneWorld') as the diffusion one, where only the
-- rule and the state type change.
--
-- One of three examples split out of what used to be a single
-- @comonad-ca-demo@ (Life, diffusion, fire).
module Main (main) where

import Control.Comonad (extract)
import Control.Comonad.Store (Store, peek)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)

import TerraHS.CA (neighborValues, runCA, seedCA)
import TerraHS.Render.PNG (renderCoverageWith, PixelRGB8 (..))

import ZoneWorld (Zone (..), zones, adjacent, canvasBBox, zoneCoverage)

outDir :: FilePath
outDir = "examples/fire-demo/out"

-- | A forest cell with at least one burning neighbour catches fire; a
-- burning cell burns out (becomes 'Burned') the very next step and
-- never reignites; a burned cell stays burned. Modelled directly on a
-- classic forest-fire cellular automaton.
data FireState = Forest | Burning | Burned
  deriving (Eq, Show)

-- | Same shape as @diffusion-demo@'s rule -- 'extract' for "me", read
-- neighbours via 'neighborValues' -- just a three-way state instead
-- of a boolean one.
fireRule :: Store Zone FireState -> FireState
fireRule w =
  case extract w of
    Burning -> Burned
    Forest | Burning `elem` neighborValues adjacent zones w -> Burning
    other -> other

seedFire :: String -> Store Zone FireState
seedFire startId = seedCA (\z -> if zoneId z == startId then Burning else Forest) (head zones)

fireStates :: Store Zone FireState -> [(String, FireState)]
fireStates w = [ (zoneId z, peek z w) | z <- zones ]

fireColor :: FireState -> PixelRGB8
fireColor Forest  = PixelRGB8 60 140 60   -- green
fireColor Burning = PixelRGB8 230 100 20  -- orange
fireColor Burned  = PixelRGB8 70 70 70    -- dark grey

main :: IO ()
main = do
  putStrLn "== fire-demo: forest fire over the same polygon domain as diffusion-demo, seeded at Z1 =="
  putStrLn "(same ZoneWorld.zones/adjacent as diffusion-demo -- only the rule and the state type differ)"
  putStrLn ""
  let steps = runCA fireRule (seedFire "Z1")
  mapM_
    (\(n, w) -> putStrLn ("t" ++ show n ++ ": " ++ intercalate ", " (map showState (fireStates w))))
    (zip [0 :: Int ..] (take 5 steps))

  putStrLn ""
  putStrLn "Expected, by hand: a burning zone burns out (Burned) the very next step,"
  putStrLn "while it sets any still-Forest neighbour alight -- so the fire front trails"
  putStrLn "one step behind where diffusion-demo's spread would already have reached."
  let actual   = map (map snd . fireStates) (take 5 steps)
      expected =
        [ [Burning, Forest,  Forest,  Forest,  Forest,  Forest ]  -- t0: Z1
        , [Burned,  Burning, Forest,  Burning, Forest,  Forest ]  -- t1: Z1|Z2,Z4
        , [Burned,  Burned,  Burning, Burned,  Burning, Forest ]  -- t2: Z3,Z5 catch
        , [Burned,  Burned,  Burned,  Burned,  Burned,  Burning]  -- t3: Z6 catches
        , [Burned,  Burned,  Burned,  Burned,  Burned,  Burned ]  -- t4: burned out
        ]
  putStrLn ("Check -- matches the hand-traced burn sequence above: " ++ show (actual == expected))

  let pngSteps = take 5 steps
  createDirectoryIfMissing True outDir
  mapM_
    (\(n, w) -> renderCoverageWith fireColor (outDir ++ "/t" ++ show n ++ ".png") canvasBBox (zoneCoverage w))
    (zip [0 :: Int ..] pngSteps)
  putStrLn ""
  putStrLn ("PNGs written to " ++ outDir ++ "/t0.png .. t4.png")
  where
    showState (zid, st) = zid ++ "=" ++ show st
