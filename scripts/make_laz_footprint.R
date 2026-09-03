#!/usr/bin/env Rscript

# Create a one-feature core AOI matching a LAS/LAZ file's exact XY bounds.
suppressPackageStartupMessages({ library(sf); library(jsonlite) })

usage <- "Usage: Rscript scripts/make_laz_footprint.R --input TILE.laz --output AOI.gpkg --layer NAME"
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("input", "output", "layer") %in% names(opts))) stop(usage, call. = FALSE)
if (file.exists(opts[["output"]])) stop("Refusing to overwrite: ", opts[["output"]], call. = FALSE)

pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("PDAL is required; set PDAL_BIN or add it to PATH.", call. = FALSE)
info <- fromJSON(paste(system2(pdal_bin, c("info", "--summary", opts[["input"]]), stdout = TRUE), collapse = "\n"))
bounds <- info$summary$bounds
tile_crs <- st_crs(info$summary$srs$wkt)
if (is.na(tile_crs)) stop("Could not read the input tile CRS.", call. = FALSE)

geom <- st_as_sfc(st_bbox(c(xmin = bounds$minx, ymin = bounds$miny,
                            xmax = bounds$maxx, ymax = bounds$maxy), crs = tile_crs))
out <- st_as_sf(geom)
out$tile_file <- basename(opts[["input"]])
out$point_count <- info$summary$num_points
out$core_area_m2 <- as.numeric(st_area(out))
st_write(out, opts[["output"]], layer = opts[["layer"]], quiet = TRUE)
message("Wrote exact tile footprint: ", opts[["output"]])
