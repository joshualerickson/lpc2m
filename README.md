# USGS LPC to lidar.metrics

This repository orchestrates AOI-driven USGS 3DEP point-cloud processing.
By default it retains normalized LAZ processing blocks (including their halos),
metric rasters, and provenance. Raw EPT extracts are transient and deleted once
their normalized block and metrics have completed successfully. When EPT is
not available, required USGS delivery LAZ tiles are downloaded once into a
resumable source cache and shared by all intersecting processing blocks.

## How to use

### Interactive R interface

Pass an `sf` polygon or multipolygon and receive metric products plus
provenance. Source resolution is automatic: every run discovers EPT coverage
from the current USGS EPT catalog, confirms the exact STAC item/end point, and
attaches current USGS QL and acquisition-date metadata. It may use EPT for one part of an
AOI, direct USGS delivery LAZ for another part, and explicitly report a third
part with no published LiDAR. The same normalization and metric code is used
after either acquisition path.

```r
# From this repository during development.
devtools::load_all(".")

aoi <- sf::st_read("data/district.gpkg", layer = "district")

lidar_opts <- lidar_options(
  block_m = 500, buffer_m = 60,
  families = c("standard", "canopy", "graph"),
  download_workers = 8, stream_workers = 16,
  normalize_workers = 40, metric_workers = 64,
  write_normalized = TRUE, resume = TRUE
)

result <- run_metrics(
  aoi = aoi, name = "district",
  output_dir = "/lidar/metrics/district",
  delivery_template = "/path/to/delivery_grid.tif",
  normalized_dir = "/lidar/normalized/district",
  options = lidar_opts,
  project_dir = getwd(),
  pdal_bin = Sys.getenv("PDAL_BIN")
)
```

Use `dry_run = TRUE` to resolve sources and write the coverage report without
downloading point clouds or starting processing. This produces an exact source
inventory CSV with source type, URL, local path when already available, QL,
acquisition dates/year, estimated bytes, and availability. Missing coverage or
unresolved metadata does not cancel processable portions of the AOI.

```r
plan <- run_metrics(
  aoi, output_dir = "/lidar/metrics/district", name = "district",
  delivery_template = "/path/to/delivery_grid.tif",
  dry_run = TRUE
)

plan$source_inventory
plan$candidates[, c("source_type", "coverage_status", "quality_level")]
plan$paths
```

If some delivery LAZ files already exist locally, create a PDAL tile index once
and pass it as an optional credit against the download plan. Nothing else about
the high-level workflow changes.

```r
result <- run_metrics(
  aoi, output_dir = "/lidar/metrics/district", name = "district",
  delivery_template = "/path/to/delivery_grid.tif",
  local_laz_index = "data/local_sources/existing_tiles.shp",
  local_laz_layer = "existing_tiles"
)
```

Optional diagnostic only: `preflight_ept_sources(aoi, ept_sources)` returns
the exact EPT survey pieces and URLs that intersect an AOI. It is useful for
mapping or source review, but ordinary users should not need to call it before
`run_metrics()`.

### Provenance and event-date filtering

Use `run_provenance()` when source timing is part of the analysis contract or
when only the EPT/direct-LAZ/missing acquisition plan is needed.
For example, this keeps fire polygons only when eligible EPT LiDAR was acquired
before the burn year. Acquisition during the burn year is conservatively
excluded because the exact fire date may be unknown.

```r
fires <- sf::st_read("data/fires.gpkg", layer = "fires")

provenance <- run_provenance(
  fires,
  output_dir = "outputs/fire_preflight",
  name = "fires",
  id_col = "fire_id",
  event_year_col = "burn_year",
  minimum_coverage = 0.80
)

# Original fire features meeting the contract.
eligible_fires <- provenance$eligible_aoi

# One row per fire, including eligible area/fraction and pass/fail.
provenance$summary

# Every intersecting source and why it was or was not eligible.
provenance$candidates

# One row per EPT resource or direct-LAZ tile, including URL, QL, date/year,
# estimated bytes, local path, and whether a download is required.
provenance$source_inventory

# IDs ready for downstream metrics or modeling.
eligible_ids <- provenance$summary$aoi_id[provenance$summary$meets_contract]

# Files are also written for use outside R/QGIS workflows.
provenance$paths
```

Use `event_date_col` instead when an exact event date exists. Set
`minimum_coverage = 1` for complete pre-event LiDAR coverage, `0.8` for 80%,
or `0` (the default) to require any eligible coverage. The function writes the
candidate source layer, feature summary, eligible AOI, and resolved QL/date
profile under `OUTPUT_DIR/provenance`.

For an exact ignition or burn date, replace `event_year_col` with:

```r
provenance <- run_provenance(
  fires,
  output_dir = "outputs/fire_preflight",
  name = "fires_exact_date",
  id_col = "fire_id",
  event_date_col = "ignition_date",
  minimum_coverage = 0.80
)
```

1. Clone the repository with its `lidar.metrics` submodule, or initialize it
   after cloning:

   ```bash
   git submodule update --init --recursive
   ```

2. Install PDAL with `readers.ept` and `writers.las` support. On a server
   without `sudo`, install it in a user-owned Conda environment; no system
   packages or administrator access are required. If Conda is not already
   available, install Miniforge or Miniconda under your home directory first.

   ```bash
   # Create the environment once. `mamba` may be substituted for `conda`.
   conda create -y -n usgs-lpc -c conda-forge pdal=2.10

   # Activate it in each new shell before running the workflow.
   conda activate usgs-lpc
   pdal --version
   pdal --drivers | grep -E 'readers.ept|writers.las'
   ```

   If the R installation remains outside Conda, that is fine: activate this
   environment to expose PDAL, then record its absolute path for reproducible
   runs:

   ```bash
   export PDAL_BIN="$(command -v pdal)"
   Rscript scripts/check_prerequisites.R
   ```

   To add PDAL to an existing environment instead, use:

   ```bash
   conda install -y -n usgs-lpc -c conda-forge pdal=2.10
   ```

3. In R, install the required packages and the local `lidar.metrics` package:

   ```r
   install.packages(c("sf", "terra", "jsonlite", "curl", "lidR", "remotes", "devtools"))
   remotes::install_local("lidar.metrics", dependencies = TRUE)
   ```

4. Verify the environment before processing:

   ```bash
   Rscript scripts/check_prerequisites.R
   ```

5. Optionally run an EPT preflight before processing. This creates an inspectable
   GeoPackage of source-footprint polygons with `coverage_status` equal to
   `ept` or `ept_unavailable`, plus the normal dates, quality level, project,
   and work-unit fields. It makes no point-cloud requests and does not require
   PDAL.

   ```bash
   Rscript scripts/run_aoi_production.R \
     --aoi data/district.gpkg --layer district --name district_preflight \
     --metrics-dir data/lidar/metrics/district_preflight \
     --preflight-only true
   ```

   Load `data/lidar/metrics/district_preflight/provenance/district_preflight_jobs_coverage.gpkg`
   in QGIS. `ept` is processable by the streaming workflow; `ept_unavailable`
   needs an alternate source such as the USGS LAZ delivery.

   To make an exact EPT-versus-non-EPT acquisition footprint from the published
   Entwine boundary catalog and confirm every selected endpoint against the
   official USGS STAC items, run:

   ```bash
   Rscript scripts/make_ept_catalog_footprint.R \
     --aoi data/lidar_need.gpkg --layer lidar_need \
     --output data/lidar_need_stac_ept_coverage.gpkg \
     --output-layer ept_coverage
   ```

   The companion `*_not_ept.gpkg` is the area that needs a direct/local LAZ
   source if it is selected for metrics. The `*_summary.csv` reports unioned
   coverage area, avoiding double counting where EPT resource footprints overlap.

   ### Optional: index existing delivery LAZ

   The package handles mixed EPT and non-EPT AOIs automatically. If a project
   already has downloaded LAZ files, index them once so `run_metrics()` can
   reuse them instead of planning the same downloads:

   ```bash
   # Run once for a local directory of LAZ files.  The index records each
   # source-tile footprint and its absolute local path.
   pdal tindex create \
     --tindex data/local_sources/northcentral_tiles.shp \
     --filespec '/path/to/ID_NorthCentral_D22/*.laz' \
     --write_absolute_path
   ```

   Then supply `local_laz_index` and `local_laz_layer` to `run_metrics()` or
   `run_provenance()`. The provenance inventory distinguishes `local`,
   `download_required`, `downloaded`, `failed`, and EPT `stream` sources. No
   AWS credentials or Requester-Pays access are used by this path.

   ### Run confirmed EPT coverage without the National Map index

   For broad or disjoint EPT-ready AOIs, use the authoritative EPT source
   polygons directly. This avoids a National Map spatial-index request for the
   district bounding box. EPT STAC records do not publish USGS quality level,
   so initialize and review a source-profile table once before the first run:

   ```bash
   # The first direct-EPT command writes this template automatically if it is
   # missing. Copy it, then set quality_level to QL1 or QL2 for each source.
   cp data/ept_source_profiles_template.csv data/ept_source_profiles.csv
   ```

   Keep this CSV as project provenance. It is reused for every forest and
   prevents the workflow from guessing QL from point density.

   ```bash
   Rscript scripts/run_aoi_production.R \
     --aoi data/lidar_need_ept_by_forest.gpkg --layer ept_by_forest \
     --filter-field forestname --filter-value 'Forest Name' --name forest_ept \
     --ept-sources data/lidar_need_ept.gpkg --ept-source-layer ept_sources \
     --ept-profiles data/ept_source_profiles.csv \
     --delivery-template /path/to/delivery_grid.tif \
     --normalized-dir /lidar/normalized/forest_ept \
     --metrics-dir /lidar/metrics/forest_ept \
     --resume true
   ```

6. The interactive `run_metrics()` call above is the recommended mixed-source
   workflow. The production script remains available as a lower-level adapter
   for EPT-only or already-planned jobs. Start conservatively with the
   independent queues below; increase only after observing RAM, disk, and USGS
   request behavior on a small AOI.

   ```bash
   # Either activate the environment containing PDAL, or pass its path below.
   # conda activate usgs-lpc
   Rscript scripts/run_aoi_production.R \
     --aoi data/district.gpkg --layer district --name district \
     --delivery-template /path/to/delivery_grid.tif \
     --stream-workers 8 --normalize-workers 48 --metric-workers 64 \
     --normalized-dir /lidar/normalized/district \
     --metrics-dir /lidar/metrics/district \
     --pdal-bin /path/to/pdal
   ```

   `--block-m 500` is the default and may be changed for testing. The generated
   grid and USGS job layer are retained under `METRICS_DIR/provenance`. The job
   layer records `source_type`, source URL/files, `quality_level`, work unit,
   acquisition start/end dates, acquisition completion year, and source area.
   It gives newest coverage priority and splits only blocks that cross a source
   boundary. Streaming is capped separately from compute. A block moves to normalization
   as soon as its EPT stream completes, then to metrics as soon as normalization
   completes; the workflow does not wait for all district tiles at each stage.
   Unsupported quality levels, failed downloads, and genuinely missing source
   coverage remain explicit in provenance and do not erase completed areas.
   By default the production run writes standard, canopy/tree, and graph metric
   families. Limit or rerun families as needed with, for example,
   `--families graph` or `--families standard,canopy`. With retained normalized
   LAZs and `--resume true`, only missing selected outputs are recomputed.

## Development path

1. Make a small core AOI from `data/lidar_need.gpkg`.
2. Select an intersecting USGS EPT endpoint from project metadata.
3. Stream the AOI plus halo to a temporary LAZ with PDAL.
4. Normalize the temporary LAZ from its classified ground points.
5. Run the canopy, tree, and graph functions supplied by the `lidar.metrics`
   submodule in the source LAS/tile CRS, keeping metrics and a manifest while
   expiring temporary points.
6. Mosaic native-CRS metric rasters only after computation, then bilinearly
   reproject the final mosaic to its delivery CRS.

`scripts/make_test_block.R` creates a 1 km² core test block.  The extraction
script refuses to operate without an explicit EPT URL and a bounded AOI; it
does not accept a forest/district-wide request by default.

## First test

```bash
Rscript scripts/make_test_block.R
Rscript scripts/extract_ept_block.R \
  --ept-url 'https://.../ept.json' \
  --aoi data/test_block.gpkg --layer test_block \
  --output cache/test_block_raw.laz
```

For a controlled single-project test only, `run_block_test.R` also accepts
explicit `--ept-url` and `--quality-level` arguments instead of a job manifest.

## Advanced: retained normalized blocks and lower-level execution

`scripts/run_block_test.R` retains each normalized LAZ by default. These files
include the processing halo, so they are suitable inputs for later metric
reruns. They overlap at block edges and must not be merged as a point cloud.

```bash
Rscript scripts/run_block_test.R \
  --blocks data/district_500m_jobs.gpkg --layer district_500m_jobs \
  --delivery-template /path/to/delivery_grid.tif --name district_ql1 \
  --stream-workers 8 --normalize-workers 48 --metric-workers 64 \
  --buffer-m 60 --families standard,canopy,graph \
  --normalized-dir /lidar/normalized/district_ql1 \
  --metrics-dir /lidar/metrics/district_ql1
```

The normalized files are written as `BLOCK_ID_norm.laz`; per-block rasters are
in `METRICS_DIR/blocks`, and the native and delivery mosaics plus manifests are
in `METRICS_DIR`. Raw streamed LAZs are deleted after a block completes.

To retain only final metrics, use `--write-normalized false`. The normalization
output then stays temporary and is deleted after the metric raster is written.
Use `--resume true` to reuse complete retained normalized blocks or block
rasters after an interrupted run.
