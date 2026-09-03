#!/usr/bin/env Rscript

# Direct, unfiltered connectivity-metric baseline for validating a previously
# normalized LAS/LAZ against historical graph products.
suppressPackageStartupMessages({ library(lidR); library(terra); library(lidar.metrics) })
set_lidr_threads(1)

usage <- paste(
  "Usage: Rscript scripts/run_graph_direct.R --input NORMALIZED.laz --output GRAPH.tif",
  "[--res 30] [--ql1 false] [--z-1 1] [--z-20 6] [--z-40 12]",
  "[--filter none|class1|ql2_graph]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("input", "output") %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)
opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default

las <- readLAS(opts[["input"]])
if (is.empty(las)) stop("Input contains no points.", call. = FALSE)
res <- as.numeric(opt_or("res", "30"))
ql1 <- tolower(opt_or("ql1", "false")) %in% c("true", "t", "1", "yes")
z_1 <- as.numeric(opt_or("z-1", "1"))
z_20 <- as.numeric(opt_or("z-20", "6"))
z_40 <- as.numeric(opt_or("z-40", "12"))
filter_name <- opt_or("filter", "none")
if (!filter_name %in% c("none", "class1", "ql2_graph")) stop("filter must be none, class1, or ql2_graph.", call. = FALSE)

if (filter_name == "class1") {
  # Optional stricter class-1-only filter, excluding scan-angle limits.
  las <- filter_poi(las, Classification == 1L & !Withheld_flag & !Overlap_flag &
                       Z >= 0.3 & Z <= 12.1 & Intensity >= 5 & ReturnNumber == 1L)
} else if (filter_name == "ql2_graph") {
  # Exact QL2 catalog filter from lidar_canopymetrics_2025_ql12.R, without
  # scan-angle limits (which were not used in that catalog filter).
  las <- filter_poi(las, Classification != 7L & Classification != 9L &
                       !Withheld_flag & !Overlap_flag & Intensity >= 5 &
                       Z >= 1 & Z <= 12.1 & ReturnNumber == 1L)
}
if (is.empty(las)) stop("Selected filter removed every point.", call. = FALSE)
message("Points after ", filter_name, " filter: ", npoints(las))

graph <- pixel_metrics(
  las,
  ~lidar.metrics::connectivity_metrics_binned(
    X, Y, Z, psid = PointSourceID, QL1 = ql1,
    edge_thresh_values = c(3, 3), z_1 = z_1, z_20 = z_20, z_40 = z_40,
    voxel_res = 3, n = 1, res = 3
  ),
  res = res
)
dir.create(dirname(opts[["output"]]), recursive = TRUE, showWarnings = FALSE)
writeRaster(graph, opts[["output"]], overwrite = FALSE)
message("Wrote direct graph metrics: ", opts[["output"]])
