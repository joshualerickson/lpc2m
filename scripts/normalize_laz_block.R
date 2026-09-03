#!/usr/bin/env Rscript

# Normalize one streamed block using its classified ground points.  The caller
# keeps the raw/norm files in cache and retains only final metrics + manifest.
suppressPackageStartupMessages(library(lidR))
options(lidR.progress = FALSE)
set_lidr_threads(1)

usage <- "Usage: Rscript scripts/normalize_laz_block.R --input RAW.laz --output NORMALIZED.laz"
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("input", "output") %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

las <- readLAS(opts[["input"]])
if (is.empty(las)) stop("Input contains no points.", call. = FALSE)
ground_n <- sum(las$Classification == 2L, na.rm = TRUE)
if (ground_n < 1000L) {
  stop("TIN normalization requires >= 1,000 ground-classified points; found ", ground_n, call. = FALSE)
}

normalized <- normalize_height(las, tin())
dir.create(dirname(opts[["output"]]), recursive = TRUE, showWarnings = FALSE)
writeLAS(normalized, opts[["output"]])
message("Normalized points: ", npoints(normalized), "; ground points: ", ground_n)
