# TerraHS

A purely functional Haskell library for geospatial programming and
map algebra, with no foreign dependencies.

TerraHS is a from-scratch rewrite of the original TerraHS
(2006–2009), a Haskell binding to the C++ GIS library TerraLib
developed at Brazil's National Institute for Space Research (INPE).
This version keeps the ideas — geometry, spatial predicates, and a
generalized map algebra — while dropping the FFI dependency entirely:
everything is plain Haskell, or built on well-established pure-Haskell
libraries for file I/O.

The library serves two purposes at once:

1. **Teaching material.** Each module is meant to illustrate a
   functional-programming concept applied to a real problem: algebraic
   data types, type classes for ad-hoc polymorphism, smart
   constructors for enforcing invariants, hand-written parser
   combinators, and a small map-algebra DSL built from first
   principles.
2. **A research tool.** The map algebra (`TerraHS.Algebra.Coverage`)
   is a faithful reconstruction of the algebra proposed in the
   original TerraHS Master's thesis (INPE, 2006), generalizing
   Tomlin's (1990) classic map algebra with arbitrary spatial
   predicates.

## Features

- **Geometry** — `Point`, `Line`, `Polygon`, bounding boxes, and a
  `Geometry` type class unifying area/perimeter/centroid/envelope
  across all three.
- **Topology** — bounding-box overlap tests, exact point-on-line
  (via point-to-segment distance) and point-in-polygon (via ray
  casting) predicates.
- **File I/O** — read and write WKT and GeoJSON; read ESRI Shapefiles
  (`.shp` + `.dbf`), pairing geometry with attributes.
- **Map algebra** — two complementary implementations:
  - `TerraHS.Algebra.Coverage`, a direct reconstruction of the
    algebra from the original thesis: a discrete `Coverage`
    (domain → values, in the OGC sense) with local, focal, and zonal
    operators generalized around arbitrary spatial predicates.
  - `TerraHS.Algebra.Funct` / `TerraHS.Algebra.Field`, a classic
    Tomlin-style raster algebra (local, focal, zonal, global
    operators over a 2D grid), reconstructed from the `Funct` type
    class found in the original codebase.

## Project layout

```
terrahs-new/
├── terrahs.cabal
├── src/
│   ├── TerraHS.hs                -- top-level module, re-exports everything below
│   ├── TerraHS/Geometry.hs        -- the Geometry type class
│   ├── TerraHS/Geometry/
│   │   ├── Coord.hs
│   │   ├── Point.hs
│   │   ├── Line.hs
│   │   ├── Polygon.hs
│   │   ├── BBox.hs
│   │   ├── Any.hs                -- AnyGeometry, a sum of Point/Line/Polygon (used by I/O)
│   │   └── Topology.hs           -- spatial predicates
│   ├── TerraHS/IO/
│   │   ├── WKT.hs                -- hand-written parser combinator
│   │   ├── GeoJSON.hs            -- via aeson
│   │   ├── Shapefile.hs          -- .shp via binary
│   │   ├── Dbf.hs                -- .dbf (attributes) via binary
│   │   └── Vector.hs             -- joins .shp + .dbf (readVectorFile)
│   └── TerraHS/Algebra/
│       ├── Coverage.hs           -- the thesis's map algebra
│       ├── Funct.hs              -- the generic lifting class (lift1/lift2/...)
│       └── Field.hs              -- 2D grid with local/focal/zonal/global operators
├── app/
│   ├── Main.hs                   -- demo executable
│   └── Synthetic.hs              -- reproducible synthetic-data generation
└── test/
    └── Spec.hs                   -- 24 test cases
```

## Installation

TerraHS is a standard Cabal package (`cabal-version: 3.0`). It has no
system dependencies beyond a working GHC and Cabal toolchain (GHC
≥ 8.10, Cabal ≥ 3.0 recommended).

The recommended way to get that toolchain is
[GHCup](https://www.haskell.org/ghcup/), which installs and manages
GHC, Cabal, and (optionally) Stack and HLS independently of your
system's package manager:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

The installer is interactive and lets you pick which components to
install; make sure it adds `~/.ghcup/bin` to your `PATH`. This is
worth doing even if your distribution already ships a `ghc`/`cabal`
package (e.g. via `apt`), since those are often old enough to hit real
problems — in particular, older `cabal-install` builds can fail to
validate Hackage's current package-index signatures
(`<repo>/root.json does not have enough signatures signed with the
appropriate keys`) after a key rotation on Hackage's end; installing a
current `cabal-install` through GHCup avoids that.

**On Linux**, building the dependency chain (`aeson` pulls in
`integer-logarithms`, which needs arbitrary-precision arithmetic) may
fail at the link step with `cannot find -lgmp` if only the GMP
runtime, and not its development headers/symlinks, is installed.
Install the `-dev` package for your distribution first — on
Debian/Ubuntu:

```sh
sudo apt install libgmp-dev
```

Then:

```sh
git clone <repository-url>
cd terrahs-new
cabal build
```

## Usage

**Run the test suite:**
```sh
cabal test
```

**Run the demo executable**, which prints geometry operations, WKT
round-tripping, and map algebra examples (including a reproduction of
the thesis's Pará deforestation example, with reproducible synthetic
data) to the terminal:
```sh
cabal run terrahs-demo
```

**Explore interactively in a REPL:**
```sh
cabal repl lib:terrahs
```
```haskell
ghci> import TerraHS
ghci> let Just sq = mkPolygon [Coord 0 0, Coord 4 0, Coord 4 4, Coord 0 4]
ghci> area sq
16.0
ghci> parseWKT "POINT (1 2)"
Right (AGPoint (Point {pointCoord = Coord {coordX = 1.0, coordY = 2.0}}))
```

**As a dependency of another project:** add this directory to a local
`cabal.project` (`packages: ./terrahs-new`), or publish it to a Git
repository and reference it via `source-repository-package`.

## Map algebra

Two implementations coexist here, and they are complementary rather
than redundant.

### `TerraHS.Algebra.Coverage`

This is the algebra described in Chapter 4 of the original TerraHS
thesis, "A Generalized Map Algebra in TerraHS" ("Integration of
Functional Programming and Spatial Databases for GIS Application
Development", Sérgio Costa, INPE, 2006, advised by Gilberto Câmara).
The same material was later published as a peer-reviewed chapter:
Costa, S.S., Câmara, G., Palomo, D. (2007). "TerraHS: Integration of
Functional Programming and Spatial Databases for GIS Application
Development." In: Davis, C.A., Monteiro, A.M.V. (eds) *Advances in
Geoinformatics*. Springer, Berlin, Heidelberg.
[doi.org/10.1007/978-3-540-73414-7_8](https://link.springer.com/chapter/10.1007/978-3-540-73414-7_8).
Rather than a fixed raster grid, it starts from a **coverage** in the
OGC sense (`DiscreteCFunction`): a discrete function `cov :: E -> A`
from a domain of geographic elements to a set of values. This
generalizes Tomlin (1990): his FOCAL ("neighborhood") and ZONAL
("contained within") operators, which only use two fixed topological
relations, become a single `spatial` operator, parameterized by *any*
spatial predicate (Egenhofer's set: disjoint, touch, inside, overlap,
contains, intersects). The `Coverages`/`CoverageOps` classes from the
thesis are reproduced directly:

- `single` — unary LOCAL (applies a function to every value)
- `multiple` — n-ary LOCAL (combines several coverages)
- `select` + `compose` — spatial selection and aggregation, kept
  separate
- `spatial` — selection plus composition in one operator; generalizes
  FOCAL and ZONAL

The tests in `test/Spec.hs` reproduce the exact numeric examples from
Figures 4.6, 4.7, 4.9, and 4.10 of the thesis, including the
points-and-a-line example (Figure 4.10), which required an exact
point-on-line test (`TerraHS.Geometry.Topology.pointOnLine`, via
point-to-segment distance) since a bounding-box filter alone wasn't
precise enough to match the original result.

### `TerraHS.Algebra.Funct` / `TerraHS.Algebra.Field`

These implement Tomlin's (1990) classic map algebra directly, built
from the `Funct` type class (`lift1`/`lift2`/...) found in the
original 2005–2009 codebase's `TerraHS.Algebras.Base.Category`
module, and the partial `Funct TeRaster` instance found in a later
snapshot (which only implemented `lift1`/`lift2`; comments in that
code pointed at `lift3`/`lift4` as the destination for 3x3-kernel
convolution, but that was never implemented). `Field` completes the
full taxonomy:

- **Local** — `lift1`/`lift2` (cell by cell)
- **Focal** — `focal3x3` (aggregates the 3x3 neighborhood — what the
  original `lift3`/`lift4` comments were aiming at)
- **Zonal** — `zonalWith` (aggregates grouped by zone)
- **Global** — `foldField`, `sumField`, `meanField`, `maxField`,
  `minField` (aggregates the whole field)

In short: `Field` is textbook Tomlin, with a fixed 3x3 neighborhood
and aggregation over a grid; `Coverage` is the thesis's generalization,
trading the fixed neighborhood for any spatial predicate over an
arbitrary domain of geometries.

## Dependencies

The core (`TerraHS.Geometry`, `TerraHS.Algebra.Funct`,
`TerraHS.Algebra.Field`) has no dependency beyond `base`. The I/O
layer adds four pure-Haskell libraries, with no FFI to C/C++:

- `aeson` — GeoJSON
- `binary` — Shapefile and DBF (binary / fixed-width parsing)
- `bytestring`, `text` — used by the above

WKT needs no new dependency: the parser is hand-written in
`TerraHS.IO.WKT`.

The `terrahs-demo` executable (not the library) has one dependency of
its own, `random`, used to generate reproducible synthetic data in
`app/Synthetic.hs`. It stays isolated to the executable by design —
consumers of just the library don't pull it in.

## Status and known limitations

- No hole support in polygons (`Polygon` models only the outer ring);
  the Shapefile reader likewise doesn't distinguish an outer ring from
  a hole by winding order.
- `Topology` covers bounding-box overlap plus exact point-on-line and
  point-in-polygon tests; an exact segment-to-segment
  (`Line`-`Line`) test is not yet implemented.
- GeoJSON reading/writing covers bare geometry objects, not
  `Feature`/`FeatureCollection` (which would carry attributes
  alongside geometry).
- Shapefile reading covers `.shp` and `.dbf`; the `.shx` index file is
  not read (not needed for sequential reads of a whole file).

**Build verification.** The library, test suite, and demo executable
have been built and run with GHC 9.4.7 (`aeson-2.1.2.1`,
`binary-0.8.9.1`, `random-1.2.1.1`, `text-2.0.2`, all other
dependencies from GHC's boot packages). All 24 test cases pass. One
real bug was found and fixed in the process: both `test/Spec.hs` and
`app/Main.hs` print non-ASCII characters (em dashes, in test names and
section headers), and on a system without a UTF-8 locale configured
(e.g. `LC_CTYPE=POSIX`, common in minimal containers/CI images), GHC's
runtime defaults `stdout` to an encoding that can't represent them,
crashing with `commitBuffer: invalid argument`. Both `main` functions
now call `hSetEncoding stdout utf8` explicitly at startup, so this no
longer depends on the environment's locale. The Shapefile/DBF/Vector
I/O modules compile cleanly but have not been exercised against real
`.shp`/`.dbf` files as part of this verification — only unit-testable
logic (geometry, WKT, GeoJSON round-tripping, the map algebra) has
been run.

## References

- Costa, S.S. (2006). *Integration of Functional Programming and
  Spatial Databases for GIS Application Development.* Master's
  thesis, INPE, advised by Gilberto Câmara.
- Costa, S.S., Câmara, G., Palomo, D. (2007). "TerraHS: Integration of
  Functional Programming and Spatial Databases for GIS Application
  Development." In: Davis, C.A., Monteiro, A.M.V. (eds) *Advances in
  Geoinformatics*, pp. 127–149. Springer, Berlin, Heidelberg.
  [doi.org/10.1007/978-3-540-73414-7_8](https://link.springer.com/chapter/10.1007/978-3-540-73414-7_8)
- Tomlin, C.D. (1983). "A Map Algebra." In: *Harvard Computer
  Graphics Conference*, Cambridge, MA.

## License

GPL-3.0-only, matching the license of the original TerraHS. See
`LICENSE`.