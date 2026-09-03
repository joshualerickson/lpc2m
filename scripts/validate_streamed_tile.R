#!/usr/bin/env Rscript

# Summarize an original normalized tile and a streamed/re-normalized equivalent
# over the same core footprint.  This is input validation, not a metric test.
suppressPackageStartupMessages({ library(sf); library(lidR) })

usage <- paste(
  "Usage: Rscript scripts/validate_streamed_tile.R --original ORIGINAL.laz",
  "--streamed STREAMED.laz --core FILE --layer NAME --output REPORT.csv",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("original", "streamed", "core", "layer", "output") %in% names(opts))) stop(usage, call. = FALSE)

original <- readLAS(opts[["original"]])
streamed <- readLAS(opts[["streamed"]])
if (is.empty(original) || is.empty(streamed)) stop("Both inputs must contain points.", call. = FALSE)
core <- st_read(opts[["core"]], layer = opts[["layer"]], quiet = TRUE)
if (nrow(core) != 1L) stop("core must contain exactly one feature.", call. = FALSE)
core_stream <- st_transform(core, st_crs(streamed@crs))
b <- st_bbox(core_stream)
streamed <- filter_poi(streamed, X >= b[["xmin"]] & X <= b[["xmax"]] & Y >= b[["ymin"]] & Y <= b[["ymax"]])

summary_row <- function(las, label) {
  d <- las@data
  data.frame(
    source = label,
    point_count = nrow(d),
    density_points_m2 = nrow(d) / as.numeric(st_area(core)),
    z_min = min(d$Z), z_p01 = as.numeric(quantile(d$Z, .01)),
    z_p50 = median(d$Z), z_p99 = as.numeric(quantile(d$Z, .99)), z_max = max(d$Z),
    ground_fraction = mean(d$Classification == 2L),
    first_return_fraction = mean(d$ReturnNumber == 1L),
    point_source_ids = length(unique(d$PointSourceID))
  )
}
out <- rbind(summary_row(original, "original_normalized"), summary_row(streamed, "streamed_tin_normalized"))
dir.create(dirname(opts[["output"]]), recursive = TRUE, showWarnings = FALSE)
write.csv(out, opts[["output"]], row.names = FALSE)
print(out, row.names = FALSE)
