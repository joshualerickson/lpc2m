#!/usr/bin/env Rscript

# Produce standard, canopy/tree, and graph layers for one normalized block.
# Inputs include a halo; outputs are masked back to the core AOI.
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(lidR)
  library(lidar.metrics)
})
options(lidR.progress = FALSE)
set_lidr_threads(1)

usage <- paste(
  "Usage: Rscript scripts/run_lidar_metrics_block.R",
  "--input NORMALIZED.laz --core FILE --layer NAME --output-dir DIR",
  "[--res 30] [--quality-level QL2] [--profile production_v1]",
  "[--families standard,canopy,graph] [--name STEM] [--block-id ID]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("input", "core", "layer", "output-dir") %in% names(opts))) stop(usage, call. = FALSE)

opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default
res <- as.numeric(opt_or("res", "30"))
quality_level <- toupper(opt_or("quality-level", "QL2"))
if (!quality_level %in% c("QL1", "QL2")) stop("quality-level must be QL1 or QL2.", call. = FALSE)
profile_name <- opt_or("profile", "production_v1")
source("scripts/metric_profiles.R")
settings <- metric_profile(profile_name)
families <- strsplit(opt_or("families", "standard,canopy,graph"), ",", fixed = TRUE)[[1]]
families <- trimws(families)
if (!length(families) || any(!families %in% c("standard", "canopy", "graph"))) {
  stop("families must be a comma-separated subset of: standard, canopy, graph.", call. = FALSE)
}
ql1 <- identical(quality_level, "QL1")
core <- st_read(opts[["core"]], layer = opts[["layer"]], quiet = TRUE)
if ("block-id" %in% names(opts)) {
  if (!"block_id" %in% names(core)) stop("core has no block_id field.", call. = FALSE)
  core <- core[core$block_id == opts[["block-id"]], ]
}
if (nrow(core) != 1L) stop("Metrics requires exactly one core AOI feature.", call. = FALSE)
las <- readLAS(opts[["input"]])
if (is.empty(las)) stop("Input contains no points.", call. = FALSE)
las_crs <- st_crs(las@crs)
if (is.na(las_crs)) stop("Input LAS/LAZ has no readable CRS.", call. = FALSE)
if (!identical(st_crs(core)$wkt, las_crs$wkt)) core <- st_transform(core, las_crs)
las_standard <- filter_poi(las, Classification %in% settings$standard$classes &
                             Z >= settings$standard$zmin & Z <= settings$standard$zmax)
las_canopy <- filter_poi(las, Classification %in% settings$canopy$classes &
                            Z >= settings$canopy$zmin & Z <= settings$canopy$zmax)
las_graph <- filter_poi(las, !(Classification %in% settings$graph$drop_classes) &
                           Z >= settings$graph$zmin & Z <= settings$graph$zmax &
                           Intensity >= settings$graph$intensity_min)
if (isTRUE(settings$graph$drop_withheld)) {
  if (!"Withheld_flag" %in% names(las_graph@data)) stop("Graph profile requires Withheld_flag, but it is absent from the LAS/LAZ.", call. = FALSE)
  las_graph <- filter_poi(las_graph, !Withheld_flag)
}
if (isTRUE(settings$graph$drop_overlap)) {
  if (!"Overlap_flag" %in% names(las_graph@data)) stop("Graph profile requires Overlap_flag, but it is absent from the LAS/LAZ.", call. = FALSE)
  las_graph <- filter_poi(las_graph, !Overlap_flag)
}
if (isTRUE(settings$graph$first_returns_only)) las_graph <- filter_poi(las_graph, ReturnNumber == 1L)

clip_core <- function(x) {
  core_vector <- vect(core)
  raster_extent <- ext(x)
  core_extent <- ext(core_vector)
  overlaps <- raster_extent$xmin < core_extent$xmax && raster_extent$xmax > core_extent$xmin &&
    raster_extent$ymin < core_extent$ymax && raster_extent$ymax > core_extent$ymin
  if (overlaps) return(mask(crop(x, core_vector), core_vector))
  # Source-specific EPT pieces can be narrower than one metric cell at a
  # survey boundary.  Preserve the expected metric footprint as NA rather
  # than failing the entire AOI because no cell center overlaps that sliver.
  empty <- rast(ext = core_extent, resolution = res(x), crs = crs(x))
  mask(empty, core_vector)
}
dir.create(opts[["output-dir"]], recursive = TRUE, showWarnings = FALSE)
prefix <- opt_or("name", tools::file_path_sans_ext(basename(opts[["input"]])))

if ("standard" %in% families) {
  standard <- pixel_metrics(las_standard, lidR:::.stdmetrics, res = res)
  writeRaster(clip_core(standard), file.path(opts[["output-dir"]], paste0(prefix, "_standard.tif")), overwrite = FALSE)
}
if ("canopy" %in% families) {
  canopy_tree <- pixel_metrics(
    las_canopy,
    ~lidar.metrics::canopy_cover_metrics(X, Y, Z, psid = PointSourceID,
                                         return_number = ReturnNumber, QL1 = ql1,
                                         n = settings$canopy$ql1_density,
                                         res = settings$canopy$ql1_decimation_res,
                                         window_func = settings$canopy$tree_window),
    res = res
  )
  writeRaster(clip_core(canopy_tree), file.path(opts[["output-dir"]], paste0(prefix, "_canopy_tree.tif")), overwrite = FALSE)
}
if ("graph" %in% families) {
  if (is.empty(las_graph)) stop("Graph profile removed every point.", call. = FALSE)
  graph <- pixel_metrics(
    las_graph,
    ~lidar.metrics::connectivity_metrics_binned(
      X, Y, Z, psid = PointSourceID, QL1 = ql1,
      edge_thresh_values = settings$graph$edge_thresholds,
      z_1 = settings$graph$z_1, z_20 = settings$graph$z_20, z_40 = settings$graph$z_40,
      voxel_res = settings$graph$voxel_res,
      n = settings$graph$ql1_points_per_voxel, res = settings$graph$ql1_voxel_res
    ),
    res = res
  )
  writeRaster(clip_core(graph), file.path(opts[["output-dir"]], paste0(prefix, "_graph.tif")), overwrite = FALSE)
}
