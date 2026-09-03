#!/usr/bin/env Rscript

# Diagnose how the QL2 dominant-PointSourceID rule changes after the catalog
# graph filter.  The graph function makes this selection independently by
# pixel and height bin, so it can change graph tails discontinuously.
suppressPackageStartupMessages({ library(lidR); library(data.table) })

usage <- "Usage: Rscript scripts/diagnose_ql2_point_source_selection.R --input NORMALIZED.laz --output REPORT.csv [--res 30]"
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("input", "output") %in% names(opts))) stop(usage, call. = FALSE)
res <- as.numeric(if ("res" %in% names(opts)) opts[["res"]] else "30")

las <- readLAS(opts[["input"]])
if (is.empty(las)) stop("Input contains no points.", call. = FALSE)
d <- as.data.table(las@data)
origin_x <- min(d$X)
origin_y <- min(d$Y)
d[, `:=`(cell_x = floor((X - origin_x) / res), cell_y = floor((Y - origin_y) / res))]

dominant <- function(x, label) {
  if (!nrow(x)) return(data.table())
  x[, .N, by = .(cell_x, cell_y, bin, PointSourceID)][order(cell_x, cell_y, bin, -N)][,
    .(point_source_id = PointSourceID[1], dominant_points = N[1], candidate_points = sum(N)),
    by = .(cell_x, cell_y, bin)
  ][, source := label]
}

with_bins <- function(x) x[Z >= 1 & Z <= 12.1][, bin := fifelse(Z <= 6.1, "understory", "midstory")]
unfiltered <- dominant(with_bins(copy(d)), "unfiltered")
ql2_filtered <- dominant(with_bins(d[
  Classification != 7L & Classification != 9L & !Withheld_flag & !Overlap_flag &
    Intensity >= 5 & ReturnNumber == 1L
]), "ql2_filter")

setkey(unfiltered, cell_x, cell_y, bin)
setkey(ql2_filtered, cell_x, cell_y, bin)
out <- merge(unfiltered, ql2_filtered, by = c("cell_x", "cell_y", "bin"), all = TRUE,
             suffixes = c("_unfiltered", "_ql2_filter"))
out[, dominant_id_changed := point_source_id_unfiltered != point_source_id_ql2_filter]
out[, dominant_fraction_unfiltered := dominant_points_unfiltered / candidate_points_unfiltered]
out[, dominant_fraction_ql2_filter := dominant_points_ql2_filter / candidate_points_ql2_filter]
fwrite(out, opts[["output"]])
message("Wrote ", nrow(out), " cell-bin diagnostics: ", opts[["output"]])
