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
