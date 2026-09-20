-- | comonad-ca-demo: the cellular-automata / dynamic-spatial-model
-- pattern from the paper "Modelos dinamicos espaciais em programacao
-- funcional" (Costa et al., WORCAP/INPE), reformulated in a more
-- modern Haskell idiom -- as a co-Kleisli function over a comonad
-- (@Control.Comonad.Store@, from the @comonad@ package) instead of
-- the paper's hand-written neighbourhood-filtering-then-mapping
-- recursion.
--
-- The paper's transition rule was, in essence, "look at a cell's
-- neighbourhood (via a spatial predicate) and decide the cell's next
-- state from it" -- applied to every cell, once per simulated instant
-- ('sim' in the paper, a manual @Monad m => [t] -> (t -> s -> m s) ->
-- s -> m s@ recursion). A 'Control.Comonad.Store.Store' is exactly
-- "a value together with its position, and a way to peek at any other
-- position" -- so the rule becomes a single function @Store e Bool ->
-- Bool@, and 'extend' applies it to every cell of the whole world at
-- once, which is what @iterate stepRule@ below replaces 'sim' with.
--
-- Two of the paper's worked models, redone this way:
--
--   * Conway's Game of Life on an infinite integer grid (the classic
--     comonadic example, included as the "hello world" check that the
--     'Store'-based machinery is doing the right thing -- a glider's
--     motion is a well-known, independently checkable fact).
--   * A diffusion / contamination spread model over polygon geometry
--     (the paper's actual subject matter), using TerraHS's own
--     'intersects' as the adjacency predicate, and bridging back into
--     'Coverage' at the end -- so the comonadic simulation is a
--     drop-in replacement for the "decide next state per cell" step
--     of a TerraHS model, not a separate universe.
--
-- This lives entirely as an example that /depends on/ TerraHS -- it
-- does not touch the core library, and the 'Coverage'-'Store' bridge
-- functions below ('storeAt', 'storeToCoverage') are local to this
-- file on purpose. The PNG rendering, on the other hand, lives in
-- 'TerraHS.Render.PNG' -- its own library component (@terrahs-render@
-- in @terrahs.cabal@), generic over any @Coverage a Bool@, not
-- specific to this demo's data. It's "TerraHS" in the sense of being
-- part of the project and reusable by any future example, just kept
-- out of the core @terrahs@ library so reading a shapefile doesn't
-- require pulling in JuicyPixels.
module Main (main) where

import Control.Comonad (extend, extract)
import Control.Comonad.Store (Store, store, pos, peek)
import Data.Functor.Contravariant (Predicate (..))
import Data.List (intercalate, sort, nub)
import System.Directory (createDirectoryIfMissing)

import TerraHS
import TerraHS.Render.PNG (renderGrid, renderGridSteps, renderCoverage, renderCoverageSteps)

-- | Where the PNGs land, relative to the repository root (same
-- convention as the other examples' @dataDir@).
outDir :: FilePath
outDir = "examples/comonad-ca-demo/out"

-- * Bridging 'Coverage' and 'Store'
--
-- A 'Coverage a b' is already "a domain, and a function from the
-- domain to values" -- precisely what 'store' wants. Going the other
-- way needs an explicit domain to enumerate, since a 'Store' alone
-- doesn't know which positions are "in bounds".

-- | A coverage's function, lifted into a 'Store' positioned at a
-- particular domain element.
storeAt :: Coverage e b -> e -> Store e b
storeAt cov = store (covFun cov)

-- | The reverse direction: given the domain to enumerate, turn a
-- 'Store' back into a 'Coverage' over it.
storeToCoverage :: [e] -> Store e b -> Coverage e b
storeToCoverage dom w = newCov dom (`peek` w)

-- ---------------------------------------------------------------
-- * Part 1: Conway's Game of Life, as a co-Kleisli function
-- ---------------------------------------------------------------

type Cell = (Int, Int)

-- | The eight Moore neighbours of a cell.
neighbours8 :: Cell -> [Cell]
neighbours8 (x, y) =
  [ (x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0) ]

-- | The classic B3/S23 rule, but written directly against a 'Store':
-- "extract" is the cell's own state, "peek" at each neighbour reads
-- its state without ever leaving the current position. No manual
-- indexing, no explicit grid array -- an infinite board, for free.
lifeRule :: Store Cell Bool -> Bool
lifeRule w =
  let alive     = extract w
      liveCount = length (filter id (map (`peek` w) (neighbours8 (pos w))))
  in (alive && (liveCount == 2 || liveCount == 3)) || (not alive && liveCount == 3)

-- | One generation: 'extend' applies 'lifeRule' to /every/ position at
-- once -- this single line is the modern replacement for the paper's
-- hand-written "map the rule over the neighbourhood-filtered cells"
-- step.
stepLife :: Store Cell Bool -> Store Cell Bool
stepLife = extend lifeRule

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

seedLife :: [Cell] -> Store Cell Bool
seedLife alive = store (`elem` alive) (0, 0)

-- | Renders a rectangular window of a 'Store Cell Bool' as ASCII, for
-- a human to look at.
renderLife :: (Int, Int) -> (Int, Int) -> Store Cell Bool -> String
renderLife (x0, y0) (x1, y1) w =
  intercalate "\n"
    [ [ if peek (x, y) w then '#' else '.' | x <- [x0 .. x1] ] | y <- [y0 .. y1] ]

runLifeDemo :: IO ()
runLifeDemo = do
  putStrLn "== Part 1: Conway's Game of Life, via Store + extend =="
  putStrLn "(the paper's 'sim' recursion, replaced by 'iterate (extend lifeRule)')"
  putStrLn ""
  let generations = iterate stepLife (seedLife glider)
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
  let window       = ((-1, -1), (6, 6))
      pngGens      = take 5 generations
      aliveFns     = [ (`peek` w) | w <- pngGens ]
  createDirectoryIfMissing True outDir
  mapM_
    (\(n, aliveFn) -> renderGrid (outDir ++ "/life-gen" ++ show n ++ ".png") (fst window) (snd window) aliveFn)
    (zip [0 :: Int ..] aliveFns)
  renderGridSteps (outDir ++ "/life-strip.png") (fst window) (snd window) aliveFns
  putStrLn ""
  putStrLn ("PNGs written to " ++ outDir ++ "/life-gen0.png .. life-gen4.png, and life-strip.png")

-- ---------------------------------------------------------------
-- * Part 2: diffusion over polygon geometry
-- ---------------------------------------------------------------

-- | A named zone: an id (for display and comparison) and the polygon
-- it stands for. Adjacency is decided from the polygon; identity
-- (for 'Store' positioning and the "not myself" check) from the id
-- alone, which is why 'Eq' is written by hand rather than derived.
data Zone = Zone { zoneId :: String, zonePoly :: Polygon }

instance Eq Zone where
  a == b = zoneId a == zoneId b

instance Show Zone where
  show = zoneId

sq :: String -> (Double, Double) -> (Double, Double) -> Zone
sq zid (x0, y0) (x1, y1) =
  Zone zid (maybe (error "invariant violated") id
              (mkPolygon [Coord x0 y0, Coord x1 y0, Coord x1 y1, Coord x0 y1]))

-- | Six unit squares laid out as a sparse, hand-traceable adjacency
-- graph (a chain with one side-branch) -- deliberately synthetic
-- rather than reusing the real IBGE municipalities: those four real
-- islands sit close enough together that every pair's bounding box
-- overlaps, so the whole set would "infect" in a single step. These
-- squares only share edges where drawn to, so a seed at one end
-- spreads gradually, matching the paper's own figure (diffusion
-- advancing t1 -> t2 -> t3 -> t4, not all at once):
--
-- >   Z5
-- >   Z4
-- > Z1 Z2 Z3 Z6
--
-- 'intersects' is bounding-box based (see 'TerraHS.Geometry.Topology')
-- with inclusive comparisons on each axis independently, so it treats
-- two squares that only meet at a shared corner as adjacent too, not
-- only ones that share a full edge -- worth knowing when reading the
-- spread below. That gives: Z1-Z2 (edge x=1), Z2-Z3 (edge x=2), Z3-Z6
-- (edge x=3), Z2-Z4 (edge y=1), Z4-Z5 (edge y=2), plus two corner
-- touches, Z1-Z4 (at (1,1)) and Z3-Z4 (at (2,1)) -- every other pair
-- is disjoint.
zones :: [Zone]
zones =
  [ sq "Z1" (0, 0) (1, 1)
  , sq "Z2" (1, 0) (2, 1)
  , sq "Z3" (2, 0) (3, 1)
  , sq "Z4" (1, 1) (2, 2)
  , sq "Z5" (1, 2) (2, 3)
  , sq "Z6" (3, 0) (4, 1)
  ]

-- | Two 'Predicate's, composed with the 'Monoid' instance for
-- 'Predicate' (where '<>' is logical AND) -- exactly the composition
-- the source paper asked for when it wanted to combine a spatial test
-- with a non-spatial one. Here both happen to be spatial/identity
-- tests, but the composition mechanism is the same either way.
notSelf :: Predicate (Zone, Zone)
notSelf = Predicate (\(a, b) -> zoneId a /= zoneId b)

touches :: Predicate (Zone, Zone)
touches = Predicate (\(a, b) -> intersects (zonePoly a) (zonePoly b))

adjacent :: Predicate (Zone, Zone)
adjacent = notSelf <> touches

-- | The smallest box covering every zone, for sizing the PNG canvas
-- ('TerraHS.Render.PNG.renderCoverage' takes this explicitly so every
-- frame of a run shares the same canvas and lines up).
canvasBBox :: BBox
canvasBBox = foldr1 union (map (envelope . zonePoly) zones)

-- | The diffusion rule: a zone is infected next turn if it already is,
-- or if any zone adjacent to it (per 'adjacent' above) is infected
-- now. Same shape as 'lifeRule' -- 'extract' for "me", 'peek' for
-- "some other position" -- just a different rule and a different
-- domain (polygons instead of grid cells).
diffusionRule :: Store Zone Bool -> Bool
diffusionRule w =
  let here             = pos w
      neighbourInfected = or [ peek z w | z <- zones, getPredicate adjacent (here, z) ]
  in extract w || neighbourInfected

stepDiffusion :: Store Zone Bool -> Store Zone Bool
stepDiffusion = extend diffusionRule

-- | Builds the initial state as an ordinary 'Coverage' first (the
-- shape TerraHS data naturally comes in), then bridges it into a
-- 'Store' with 'storeAt' -- the other direction of the bridge from
-- 'storeToCoverage', used below to read the final state back out.
seedDiffusion :: String -> Store Zone Bool
seedDiffusion startId = storeAt seedCoverage (head zones)
  where
    seedCoverage = newCov zones (\z -> zoneId z == startId)

infectedIds :: Store Zone Bool -> [String]
infectedIds w = sort [ zoneId z | z <- zones, peek z w ]

runDiffusionDemo :: IO ()
runDiffusionDemo = do
  putStrLn "== Part 2: diffusion over polygon geometry, seeded at Z1 =="
  putStrLn "(adjacency = TerraHS's own 'intersects', composed with 'notSelf' via Predicate's Monoid)"
  putStrLn ""
  let steps = iterate stepDiffusion (seedDiffusion "Z1")
  mapM_
    (\(n, w) -> putStrLn ("t" ++ show n ++ ": " ++ intercalate ", " (infectedIds w)))
    (zip [0 :: Int ..] (take 5 steps))

  putStrLn ""
  putStrLn "Expected spread, by hand (BFS over the adjacency listed above, corner touches"
  putStrLn "included): t0={Z1} -> t1={Z1,Z2,Z4} -> t2={Z1,Z2,Z3,Z4,Z5}"
  putStrLn "         -> t3={Z1,Z2,Z3,Z4,Z5,Z6} -> t4=same (fixed point)."
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
  -- @Coverage Polygon Bool@ first, with 'fromPairs' -- the same core
  -- combinator 'road-city-join-demo' uses to build a coverage from
  -- loaded shapefile data.
  let pngSteps    = take 5 steps
      polyCoverage :: Store Zone Bool -> Coverage Polygon Bool
      polyCoverage w = fromPairs [ (zonePoly z, peek z w) | z <- zones ]
  mapM_
    (\(n, w) -> renderCoverage (outDir ++ "/diffusion-t" ++ show n ++ ".png") canvasBBox (polyCoverage w))
    (zip [0 :: Int ..] pngSteps)
  renderCoverageSteps (outDir ++ "/diffusion-strip.png") canvasBBox (map polyCoverage pngSteps)
  putStrLn ""
  putStrLn ("PNGs written to " ++ outDir ++ "/diffusion-t0.png .. diffusion-t4.png, and diffusion-strip.png")

main :: IO ()
main = do
  runLifeDemo
  putStrLn ""
  runDiffusionDemo
