#!/usr/bin/env Rscript

# Compare common metric bands between a newly produced raster and a prior
# production raster.  The reference is projected to the new grid with nearest
# neighbour sampling, preserving categorical/count values where present.
suppressPackageStartupMessages(library(terra))

usage <- "Usage: Rscript scripts/compare_metric_rasters.R --new NEW.tif --reference OLD.tif --output REPORT.csv"
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("new", "reference", "output") %in% names(opts))) stop(usage, call. = FALSE)

new <- rast(opts[["new"]])
reference <- rast(opts[["reference"]])
common <- intersect(names(new), names(reference))
if (!length(common)) stop("The rasters have no metric-band names in common.", call. = FALSE)
new <- new[[common]]
reference <- project(reference[[common]], new, method = "near")

report <- do.call(rbind, lapply(common, function(metric) {
  x <- values(new[[metric]], mat = FALSE)
  y <- values(reference[[metric]], mat = FALSE)
  keep <- is.finite(x) & is.finite(y)
  delta <- x[keep] - y[keep]
  data.frame(
    metric = metric,
    n = sum(keep),
    mean_new = if (length(x[keep])) mean(x[keep]) else NA_real_,
    mean_reference = if (length(y[keep])) mean(y[keep]) else NA_real_,
    mean_bias = if (length(delta)) mean(delta) else NA_real_,
    mae = if (length(delta)) mean(abs(delta)) else NA_real_,
    rmse = if (length(delta)) sqrt(mean(delta^2)) else NA_real_,
    correlation = if (length(delta) > 2 && sd(x[keep]) > 0 && sd(y[keep]) > 0) cor(x[keep], y[keep]) else NA_real_
  )
}))
dir.create(dirname(opts[["output"]]), recursive = TRUE, showWarnings = FALSE)
write.csv(report, opts[["output"]], row.names = FALSE)
message("Compared ", nrow(report), " common bands; wrote ", opts[["output"]])
