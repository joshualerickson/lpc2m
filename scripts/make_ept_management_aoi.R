#!/usr/bin/env Rscript

# Attribute a dissolved EPT footprint back to management AOIs (for example,
# forests) so production runs can select a management unit without hand-drawing.
suppressPackageStartupMessages(library(sf))
usage <- paste(
  "Usage: Rscript scripts/make_ept_management_aoi.R --aoi ADMIN.gpkg --aoi-layer NAME",
  "--ept EPT.gpkg --ept-layer NAME --output OUTPUT.gpkg --output-layer NAME",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
need <- c("aoi", "aoi-layer", "ept", "ept-layer", "output", "output-layer")
if (!all(need %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)

aoi <- st_read(opt[["aoi"]], layer = opt[["aoi-layer"]], quiet = TRUE)
ept <- st_read(opt[["ept"]], layer = opt[["ept-layer"]], quiet = TRUE)
if (!nrow(aoi) || !nrow(ept) || is.na(st_crs(aoi)) || is.na(st_crs(ept))) stop("Inputs must be non-empty and have CRS.", call. = FALSE)
ept <- st_as_sf(st_union(st_make_valid(st_transform(ept, st_crs(aoi)))))
out <- suppressWarnings(st_intersection(st_make_valid(aoi), ept))
out <- out[as.numeric(st_area(out)) > 0, ]
out$ept_coverage_status <- "ept"
out$ept_area_m2 <- as.numeric(st_area(out))
st_write(out, opt[["output"]], layer = opt[["output-layer"]], quiet = TRUE)
message("Wrote ", nrow(out), " management-attributed EPT AOIs: ", opt[["output"]])
