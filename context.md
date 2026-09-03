# Product contract: `usgslpc`

## Purpose

Given one or more valid polygonal AOIs, produce reproducible normalized-LiDAR
metrics without requiring the caller to understand EPT, LAZ delivery layouts,
processing blocks, or metric-family internals.

## Public contract

**Input:** an `sf` polygon/multipolygon (or a vector file + layer) and an
output location. Metric families and compute limits have safe defaults; they
are optional function arguments, not required workflow knowledge.

**Output:**

- native-CRS and delivery-CRS mosaics for requested standard, canopy/tree, and
  graph metric families;
- retained normalized LAZ blocks with halos, unless disabled explicitly;
- a complete provenance bundle: AOI, blocks, source assignment, acquisition
  dates, quality level, metric profile, software settings, performance, and
  failures/uncovered area.

## Source-resolution policy

1. Prefer an authoritative public EPT source that intersects the AOI.
2. Select source coverage geometrically, never from an AOI bounding box alone.
3. Resolve quality level and acquisition date from authoritative USGS metadata;
   do not infer QL from point density.
4. Among overlapping eligible sources, prefer newest acquisition end date;
   use QL1 as the tie-breaker. Record every rejected candidate and the rule.
5. Areas with no EPT source become a direct-LAZ acquisition plan, not a silent
   omission. Existing local LAZ coverage is credited before remote download.

## Partial-coverage behavior

`run_metrics(aoi)` is successful when it produces metrics for every source
area that can be resolved and processed. It must not fail the complete AOI
because another piece lacks EPT, has no published QL metadata, or needs a
direct LAZ acquisition. Instead it returns and writes a `coverage_report`
with each area classified as `processed`, `needs_direct_laz`,
`needs_metadata`, or `failed`. It fails only when no processable area remains
or an explicitly requested strict mode is enabled.

## Processing invariants

- Core blocks are stable squares (500 m default); streamed and normalized data
  include a halo (60 m default).
- Metrics are calculated in the native point-cloud CRS. Reprojection occurs
  only after native mosaicking, using documented delivery resampling.
- Raw streamed extracts are disposable. Normalized halo blocks and final
  metrics are resumable products.
- Each stage is independently concurrent and bounded: network streams, point
  normalization, and metrics must not compete as one worker pool.
- A source/QL ambiguity is a preflight failure, not a default guess.

## Non-goals

- Downloading an entire USGS project just because its bounding box intersects
  an AOI.
- Requiring a person to manually select EPT resource polygons per forest.
- Making a different metric product because a source endpoint changes.

## Current migration target

Existing scripts are the working reference implementation only. The public
package API is `run_metrics(aoi, output_dir, ...)`; its internal phases are
`validate_aoi()`, `preflight_sources()`, `plan_lidar()`, normalization, and
metrics. No public workflow should depend on a specific USGS index service
being healthy or require source polygons, EPT URLs, QL tables, or CLI flags.
