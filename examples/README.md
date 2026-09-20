# Examples

Worked examples beyond the flagship `terrahs-demo` (in `app/`), each a
small, self-contained executable. Source data lives under
[`data/`](data/), shared across the examples that need it.

Run any of them with `cabal run <name>` from the repository root (the
data paths they use are relative to it).

## `geojoin-demo`

A focused demonstration of the spatial join / zonal-statistics
operation, entirely from synthetic, hand-checkable data: rain-gauge
stations (points with a rainfall reading) joined to three
municipalities (polygons), computing count/sum/mean rainfall per
zone with `TerraHS.Algebra.Coverage.spatial` and `pointInPolygon` as
the predicate. Contrasted with `multiple`, a *non-spatial* join that
matches by point identity instead of geometry.

```sh
cabal run geojoin-demo
```

## `road-city-join-demo`

The other classic spatial join — "which polygons does this line
cross?" — loading two real Shapefiles (`data/cities.shp`, three
municipality polygons; `data/roads.shp`, two road polylines) via
`TerraHS.IO.Vector.readVectorFile`. Uses
`TerraHS.Geometry.Topology.crossesPolygon` as the `select` predicate.
One of the two roads crosses a municipality without any of its
vertices ever landing inside it — checking that `crossesPolygon` gets
that case right (not just "is an endpoint inside?") is exactly why
the synthetic data was drawn that way.

```sh
cabal run road-city-join-demo
```

## `ibge-road-join-demo`

The same crosses-polygon join, against real data this time: IBGE's
2025 Malha Municipal for Maranhão (`data/ibge/`, 217 municipalities,
official government data — see `data/ibge/LEIA-ME.txt`). No real road
layer was available, so the "road" tested against it is an
explicitly-labeled invented line across the Ilha do Maranhão area;
every municipality the join reports is the real, unscripted result of
testing that line against real polygon boundaries. Also shows carrying
more than one attribute per domain element in a `Coverage` (a
municipality's name *and* its official area, both straight from the
`.dbf`).

```sh
cabal run ibge-road-join-demo
```

## Cellular automata: `life-demo`, `diffusion-demo`, `fire-demo`

The cellular-automata / dynamic-spatial-model pattern from the paper
*"Modelos dinâmicos espaciais em programação funcional"* (Costa et
al., WORCAP/INPE), reformulated as a co-Kleisli function over a
comonad (`Control.Comonad.Store`, from the `comonad` package) instead
of the paper's hand-written neighbourhood-filtering recursion. Three
models share the same stepping-and-neighbourhood machinery, so — like
`ibge-road-join-demo` sitting alongside `road-city-join-demo` rather
than inside it — each gets its own executable rather than all three
being read together as one file:

* **`life-demo`** — Conway's Game of Life on an infinite grid: a
  glider, checked against the textbook fact that it reproduces itself
  shifted by `(+1,+1)` after 4 generations.
* **`diffusion-demo`** — a diffusion/contamination model over polygon
  geometry, using TerraHS's own `intersects` as the adjacency
  predicate over six synthetic squares laid out so the spread is
  gradual and hand-traceable, checked against a BFS worked out by
  hand.
* **`fire-demo`** — a forest-fire model (forest / burning / burned)
  over that same six-square domain and the same adjacency as
  `diffusion-demo` — only the rule and the state type differ, checked
  against a hand-traced burn sequence (the fire front trails one step
  behind where the diffusion would already have spread, since a
  burning zone only sets its neighbours alight the step before it
  burns out).

```sh
cabal run life-demo
cabal run diffusion-demo
cabal run fire-demo
```

**What they share, and where it lives.** The stepping/neighbourhood
machinery is `TerraHS.CA`, its own library component (`terrahs-ca`) —
the same role a `CellularAutomaton` base class plays in an
object-oriented framework (`neighborValues`, `stepCA` = `extend`,
`runCA` = the paper's `sim` replaced by `iterate`, `seedCA` = `store`),
except there's no class to subclass: a model is just a domain, an
adjacency `Predicate`, and a rule function. It also holds two small
generic pieces for *building* that predicate — `notSelf` (a domain
element is never its own neighbour) and `touchesVia` (lifts any binary
relation on a projected value, typically a spatial predicate like
`intersects`, into a `Predicate` over pairs of domain elements) — and
`moore8`, the eight-neighbour offsets for an unbounded integer grid,
which `life-demo` uses in place of `neighborValues` (there's no finite
domain to search on an infinite board).

`diffusion-demo` and `fire-demo` additionally share one *world*: the
six-zone polygon domain and its adjacency predicate live in
`examples/common/ZoneWorld.hs` (not its own library — it's this pair
of demos' shared fixture, not reusable machinery), along with
`zoneCoverage`, turning either model's `Store` state into a plain
`Coverage Polygon v` for rendering. `examples/common/StoreBridge.hs`
holds the two-way bridge between `Store` and TerraHS's own `Coverage`
(`storeAt`, `storeToCoverage`) — fully generic, no dependency on
`Zone` at all, usable by any comonadic model that also wants to talk
to the rest of TerraHS as a `Coverage`.

Besides the text output, each renders its run as PNGs (via
`TerraHS.Render.PNG`, another library component — `JuicyPixels`, pure
Haskell, no FFI) into its own `out/` directory (not checked in —
regenerated on every run): one frame per generation/time step plus a
side-by-side strip (`life-demo`, `diffusion-demo`), all drawn at their
real geometric position, via `envelope`, not a schematic.

## `render-demo`

The straightforward recipe: load a shapefile of real municipalities,
draw it, save a PNG. Reads the same real IBGE Malha Municipal for
Maranhão (`data/ibge/`, 217 municipalities) that `ibge-road-join-demo`
and `ibge-map-demo` do. `simplifyPolygon` still runs here, quietly, as
just one more step in the pipeline — skipping it would make this take
tens of seconds instead of under one, since real municipal boundaries
have far more detail than a small PNG can show — but unlike
`ibge-map-demo`, this doesn't render the *original* geometry at all,
measure anything, or compare the two. This is the version to reach for
when all that's wanted is the map; `ibge-map-demo` below is the
side-by-side original-vs-simplified comparison, with real numbers,
that justifies this step in the first place.

```sh
cabal run render-demo
```

## `ibge-map-demo`

Plots the real IBGE Malha Municipal for Maranhão (the same
`data/ibge/` data `ibge-road-join-demo` reads) as an actual map, not
just bounding boxes: `TerraHS.Render.PNG.renderPolygonFillWith` fills
each municipality's real shape, testing every pixel against it with
`pointInPolygon`. A bounding-box render would draw nothing but
overlapping rectangles for a real, irregular coastline — so this is
the first example that needs true polygon-shape rendering, not the
synthetic squares `diffusion-demo`/`fire-demo` get away with.

That's also what makes it a demonstration of
`TerraHS.Geometry.Simplify` (Ramer-Douglas-Peucker line/polygon
simplification, in the core library — pure arithmetic on coordinates,
no new dependency). Rendering the *original* geometry — 810,584
vertices across 280 polygon parts, one municipality alone
(Amarante do Maranhão) has over 17,000 — takes tens of seconds, almost
all of it `pointInPolygon` ray-casting against vertices that, at the
resolution a ~550×740px PNG can even show, are individually invisible:
a bend smaller than half a render pixel cannot change which colour a
pixel ends up. Simplifying to that tolerance first
(`epsilon = 1 / (2 * scalePx)`) removes exactly the vertices that
could never have mattered to this render — down to 10,133 vertices,
a 98.7% reduction — and the two images that come out are visually
indistinguishable, while the render itself goes from ~29s to ~0.45s
(a ~65x speedup). Real numbers from real government data, not a
synthetic benchmark, for the general lesson: simplify to the
resolution you're about to use, before the costly operation, not just
for rendering.

Writes both PNGs to `examples/ibge-map-demo/out/` (not checked in —
regenerated on every run) so they can be compared side by side.

```sh
cabal run ibge-map-demo
```
