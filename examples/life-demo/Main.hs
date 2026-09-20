-- | life-demo: Conway's Game of Life, as a co-Kleisli function over
-- 'Control.Comonad.Store.Store' -- the classic comonadic example,
-- included as the "hello world" check that 'TerraHS.CA''s stepping
-- machinery is doing the right thing (a glider's motion is a
-- well-known, independently checkable fact).
--
-- One of three examples split out of what used to be a single
-- @comonad-ca-demo@ (Life, diffusion, fire): each is its own
-- self-contained model now, so that "one file per automaton" doesn't
-- force wading through the other two to read one. What they share --
-- the stepping-and-neighbourhood machinery -- lives in 'TerraHS.CA'
-- (its own library component, @terrahs-ca@), not duplicated between
-- them: 'stepCA'\/'runCA'\/'seedCA' step any model, and 'moore8'
-- computes an unbounded grid's neighbourhood the same way here as it
-- would for any other Life-like automaton.
module Main (main) where

import Control.Comonad (extract)
import Control.Comonad.Store (Store, pos, peek)
import Data.List (intercalate, sort)
import System.Directory (createDirectoryIfMissing)

import TerraHS.CA (runCA, seedCA, moore8)
import TerraHS.Render.PNG (renderGrid, renderGridSteps)

outDir :: FilePath
outDir = "examples/life-demo/out"

type Cell = (Int, Int)

-- | The classic B3/S23 rule, written directly against a 'Store':
-- "extract" is the cell's own state, "peek" at each neighbour reads
-- its state without ever leaving the current position. No manual
-- indexing, no explicit grid array -- an infinite board, for free.
-- 'moore8' (from 'TerraHS.CA') supplies the neighbourhood, since
-- there's no finite domain to search on an unbounded grid.
lifeRule :: Store Cell Bool -> Bool
lifeRule w =
  let alive     = extract w
      liveCount = length (filter id (map (`peek` w) (moore8 (pos w))))
  in (alive && (liveCount == 2 || liveCount == 3)) || (not alive && liveCount == 3)

-- | A glider, in its classic starting orientation:
--
-- > . X .
-- > . . X
-- > X X X
--
-- A well-known fact used here as a sanity check rather than asserted
-- from scratch: after 4 generations, a glider reproduces itself
-- shifted by (+1, +1) (and cycles through 4 rotations along the way).
glider :: [Cell]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

-- | Renders a rectangular window of a 'Store Cell Bool' as ASCII, for
-- a human to look at.
renderLife :: (Int, Int) -> (Int, Int) -> Store Cell Bool -> String
renderLife (x0, y0) (x1, y1) w =
  intercalate "\n"
    [ [ if peek (x, y) w then '#' else '.' | x <- [x0 .. x1] ] | y <- [y0 .. y1] ]

main :: IO ()
main = do
  putStrLn "== life-demo: Conway's Game of Life, via TerraHS.CA (Store + extend) =="
  putStrLn "(a glider, checked against the textbook fact that it reproduces itself shifted by (+1,+1) after 4 generations)"
  putStrLn ""
  let generations = runCA lifeRule (seedCA (`elem` glider) (0, 0))
  mapM_
    (\(n, w) -> do
        putStrLn ("Generation " ++ show n ++ ":")
        putStrLn (renderLife (-1, -1) (6, 6) w)
        putStrLn "")
    (zip [0 :: Int ..] (take 5 generations))

  -- Sanity check: after 4 generations, the glider is the same shape,
  -- shifted by (+1, +1) -- textbook behaviour, not a result we're
  -- claiming for the first time.
  let gen4          = generations !! 4
      expectedAlive = sort [ (x + 1, y + 1) | (x, y) <- glider ]
      windowCells   = [ (x, y) | x <- [-1 .. 6], y <- [-1 .. 6] ]
      actualAlive   = sort [ c | c <- windowCells, peek c gen4 ]
  putStrLn ("Check -- generation 4 == glider shifted by (+1,+1): "
             ++ show (expectedAlive == actualAlive))

  -- PNGs: one per generation, plus a strip with all of them side by
  -- side, so the run can actually be looked at.
  let window   = ((-1, -1), (6, 6))
      pngGens  = take 5 generations
      aliveFns = [ (`peek` w) | w <- pngGens ]
  createDirectoryIfMissing True outDir
  mapM_
    (\(n, aliveFn) -> renderGrid (outDir ++ "/gen" ++ show n ++ ".png") (fst window) (snd window) aliveFn)
    (zip [0 :: Int ..] aliveFns)
  renderGridSteps (outDir ++ "/strip.png") (fst window) (snd window) aliveFns
  putStrLn ""
  putStrLn ("PNGs written to " ++ outDir ++ "/gen0.png .. gen4.png, and strip.png")
