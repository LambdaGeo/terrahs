module Main (main) where

import TerraHS
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stdout, utf8)

-- A simple "unit test" harness without depending on hspec/tasty, to
-- keep the project free of extra dependencies for now. Each case is
-- a pair (name, check result).
type Case = (String, Bool)

approxEq :: Double -> Double -> Bool
approxEq a b = abs (a - b) < 1e-9

square :: Polygon
square = maybe (error "invariant violated") id (mkPolygon
  [ Coord 0 0, Coord 4 0, Coord 4 4, Coord 0 4 ])

diagonal :: Line
diagonal = maybe (error "invariant violated") id
  (mkLine [ Coord 0 0, Coord 4 4 ])

-- A simple 3x3 field for the map algebra tests — a reconstruction of
-- the Funct class / Field instance from the original TerraHS
-- codebase (TerraHS.Algebras.Base.Category / TeRaster.hs).
mapAlgebraField :: Field Double
mapAlgebraField = maybe (error "invariant violated") id (mkField
  [ [1, 2, 3]
  , [4, 5, 6]
  , [7, 8, 9]
  ])

cases :: [Case]
cases =
  [ ("area of the 4x4 square is 16", approxEq (area square) 16)
  , ("perimeter of the 4x4 square is 16", approxEq (perimeter square) 16)
  , ("centroid of the 4x4 square is (2,2)",
      centroid square == Coord 2 2)
  , ("length of the (0,0)-(4,4) diagonal is 4*sqrt 2",
      approxEq (perimeter diagonal) (4 * sqrt 2))
  , ("the square and the diagonal intersect (same bbox)",
      intersects square diagonal)
  , ("WKT: parseWKT reads a simple POINT",
      parseWKT "POINT (1 2)" == Right (AGPoint (Point (Coord 1 2))))
  , ("WKT: parseWKT reads a POLYGON and preserves its area",
      case parseWKT "POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0))" of
        Right g  -> approxEq (area g) 16
        Left _   -> False)
  , ("WKT: round-trip parse . render is the identity (POINT)",
      let g = AGPoint (Point (Coord 1.5 (-2.5)))
      in parseWKT (renderWKT g) == Right g)
  , ("GeoJSON: round-trip encode . decode is the identity (POLYGON)",
      let g = AGPolygon square
      in decodeGeoJSON (encodeGeoJSON g) == Right g)
  , ("Funct []: lift1 doubles each element (local operator)",
      lift1 (*2) [1, 2, 3 :: Int] == [2, 4, 6])
  , ("Funct []: lift2 sums two lists position by position",
      lift2 (+) [1, 2, 3 :: Int] [10, 20, 30] == [11, 22, 33])
  , ("Field: lift1 (local operator) doubles every cell of the field",
      fieldCells (lift1 (* 2) mapAlgebraField) == [[2, 4, 6], [8, 10, 12], [14, 16, 18]])
  , ("Field: lift2 (local operator) sums the field with itself",
      fieldCells (lift2 (+) mapAlgebraField mapAlgebraField)
        == [[2, 4, 6], [8, 10, 12], [14, 16, 18]])
  , ("Field: focal3x3 sums the entire neighborhood at the center cell",
      fieldAt (focal3x3 0 sum mapAlgebraField) 1 1 == Just 45)
  , ("Field: focal3x3 at a corner sums only the valid neighborhood (edge = 0)",
      fieldAt (focal3x3 0 sum mapAlgebraField) 0 0 == Just 12)
  , ("Field: sumField (global operator) sums every cell",
      sumField mapAlgebraField == 45)
  , ("Field: meanField (global operator) is the mean of every cell",
      approxEq (meanField mapAlgebraField) 5)
  , ("Field: zonalWith (zonal operator) groups and sums by zone",
      let Just zones = mkField [[1, 1], [2, 2 :: Int]]
          Just vals  = mkField [[10, 20], [30, 40 :: Double]]
      in zonalWith sum zones vals == [(1, 30), (2, 70)])

  -- Faithful reproductions of the thesis's worked numeric examples
  -- (Chapter 4, "A Generalized Map Algebra in TerraHS") using the
  -- reconstruction in TerraHS.Algebra.Coverage.
  , ("Coverage: Figure 4.6 — single (^2) over [2,4,12] gives [4,16,144]",
      let c1 = newCov [1, 2, 3 :: Int] (\x -> [2, 4, 12 :: Int] !! (x - 1))
          c2 = single (^ (2 :: Int)) c1
      in values c2 == [4, 16, 144])
  , ("Coverage: Figure 4.7 — multiple sum of [2,4,8] and [4,5,10] gives [6,9,18]",
      let c1 = newCov [1, 2, 3 :: Int] (\x -> [2, 4, 8 :: Int] !! (x - 1))
          c2 = newCov [1, 2, 3 :: Int] (\x -> [4, 5, 10 :: Int] !! (x - 1))
          c3 = multiple sum [c1, c2] c1
      in values c3 == [6, 9, 18])
  , ("Coverage: Figure 4.9 — compose sum of [2,6,8] gives 16",
      let c1 = newCov [1, 2, 3 :: Int] (\x -> [2, 6, 8 :: Int] !! (x - 1))
      in compose sum c1 == 16)
  , ("Coverage: Figure 4.10 — spatial sum with a point-on-line predicate gives [14]",
      let pts   = [Point (Coord 4 5), Point (Coord 1 2), Point (Coord 2 3), Point (Coord 1 3)]
          vals  = [2, 4, 5, 10 :: Double]
          c1    = newCov pts (\p -> maybe 0 id (lookup p (zip pts vals)))
          Just ln = mkLine [Coord 1 2, Coord 2 2, Coord 1 3, Coord 0 4]
          cref  = newCov [ln] (const (0 :: Double))
          c3    = spatial sum c1 (pointOnLine 1e-9) cref
      in values c3 == [14])
  , ("Topology: pointInPolygon — a point clearly inside the square",
      pointInPolygon (Point (Coord 2 2)) square)
  , ("Topology: pointInPolygon — a point clearly outside the square",
      not (pointInPolygon (Point (Coord 10 10)) square))

  -- crossesPolygon: the same regression cases examples/road-city-join-demo
  -- was built around — one road with a vertex landing inside the
  -- polygon, one crossing straight through an edge with no vertex
  -- ever inside (the case a naive "is an endpoint inside?" test would
  -- miss).
  , ("Topology: crossesPolygon — a line with a vertex inside the square crosses it",
      let Just ln = mkLine [Coord (-1) 2, Coord 2 2, Coord 6 2]
      in crossesPolygon ln square)
  , ("Topology: crossesPolygon — a line passing straight through with no vertex inside still crosses",
      let Just ln = mkLine [Coord (-1) 2, Coord 6 2]
      in crossesPolygon ln square)
  , ("Topology: crossesPolygon — a line entirely outside the square doesn't cross it",
      let Just ln = mkLine [Coord 10 10, Coord 20 20]
      in not (crossesPolygon ln square))
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  results <- mapM report cases
  if and results
    then putStrLn "All tests passed."
    else exitFailure
  where
    report (name, ok) = do
      putStrLn ((if ok then "OK     " else "FAILED ") ++ name)
      pure ok
