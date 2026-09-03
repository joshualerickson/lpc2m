#!/usr/bin/env Rscript

# Join processing blocks to selected direct-LAZ tile footprints. Jobs are
# grouped by acquisition/work unit and retain every local tile needed by the
# buffered extraction window.
suppressPackageStartupMessages(library(sf))

usage <- paste(
  "Usage: Rscript scripts/make_direct_laz_job_manifest.R --blocks BLOCKS.gpkg --layer NAME",
  "--tiles TILES.gpkg --tile-layer direct_laz_tiles --plan PLAN.csv --output JOBS.gpkg --output-layer NAME",
  "[--buffer-m 60]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("blocks", "layer", "tiles", "tile-layer", "plan", "output", "output-layer")
if (!all(required %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)
buffer_m <- as.numeric(if ("buffer-m" %in% names(opt)) opt[["buffer-m"]] else 60)

blocks <- st_read(opt[["blocks"]], layer = opt[["layer"]], quiet = TRUE)
tiles <- st_read(opt[["tiles"]], layer = opt[["tile-layer"]], quiet = TRUE)
available_layers <- st_layers(opt[["tiles"]])$name
coverage <- if ("direct_laz_coverage" %in% available_layers) {
  st_read(opt[["tiles"]], layer = "direct_laz_coverage", quiet = TRUE)
} else tiles
plan <- read.csv(opt[["plan"]], stringsAsFactors = FALSE, na.strings = c("", "NA"))
needed <- c("workunit", "quality_level", "horiz_crs", "local_path", "acquisition_start", "acquisition_end")
if (!nrow(blocks) || !"block_id" %in% names(blocks) || is.na(st_crs(blocks))) stop("Blocks must be non-empty with block_id and a CRS.", call. = FALSE)
if (!nrow(tiles) || !all(setdiff(needed, "local_path") %in% names(tiles))) stop("Direct-LAZ tiles are missing required fields.", call. = FALSE)
if (!all(c("source_id", "file_name", "local_path") %in% names(plan))) stop("Downloaded direct-LAZ plan is missing source_id, file_name, or local_path.", call. = FALSE)
key <- paste(tiles$source_id, tiles$file_name, sep = "\r")
plan_key <- paste(plan$source_id, plan$file_name, sep = "\r")
tiles$local_path <- plan$local_path[match(key, plan_key)]
tiles <- st_make_valid(st_transform(tiles, st_crs(blocks)))
coverage <- st_make_valid(st_transform(coverage, st_crs(blocks)))
tiles <- tiles[!is.na(tiles$local_path) & file.exists(tiles$local_path), ]
if (!nrow(tiles)) stop("No planned direct-LAZ tile is available locally.", call. = FALSE)

rows <- list(); geoms <- list()
for (i in seq_len(nrow(blocks))) {
  hit <- st_intersects(blocks[i, ], coverage)[[1]]
  if (!length(hit)) next
  for (workunit in unique(coverage$workunit[hit])) {
    source_tiles <- tiles[tiles$workunit == workunit, ]
    source_coverage <- coverage[coverage$workunit == workunit, ]
    # A partially downloaded survey remains usable, but only where an acquired
    # tile actually exists. Failed tiles stay in the acquisition plan and are
    # retried on resume instead of producing doomed block jobs.
    available_coverage <- suppressWarnings(st_intersection(st_union(st_geometry(source_coverage)), st_union(st_geometry(source_tiles))))
    core_piece <- suppressWarnings(st_intersection(st_geometry(blocks[i, ]), available_coverage))
    if (!length(core_piece) || sum(as.numeric(st_area(core_piece))) < 1) next
    core_piece <- st_union(core_piece)
    halo <- st_buffer(st_as_sf(core_piece), buffer_m)
    needed_tiles <- source_tiles[lengths(st_intersects(source_tiles, halo)) > 0, ]
    if (!nrow(needed_tiles)) next
    first <- needed_tiles[1, ]
    rows[[length(rows) + 1L]] <- data.frame(
      block_id = paste(blocks$block_id[i], "direct", workunit, sep = "__"),
      source_block_id = blocks$block_id[i], source_type = "direct_laz",
      source_name = workunit, ept_name = NA_character_, ept_url = NA_character_,
      quality_level = as.character(first$quality_level),
      acquisition_start = as.character(first$acquisition_start),
      acquisition_end = as.character(first$acquisition_end),
      source_area_m2 = sum(as.numeric(st_area(core_piece))),
      source_crs = as.character(first$horiz_crs),
      source_files = paste(unique(needed_tiles$local_path), collapse = "|"),
      stringsAsFactors = FALSE
    )
    geoms[[length(geoms) + 1L]] <- core_piece[[1]]
  }
}
if (!length(rows)) stop("No direct-LAZ tile footprint intersects a processing block.", call. = FALSE)
jobs <- st_sf(do.call(rbind, rows), geometry = st_sfc(geoms, crs = st_crs(blocks)))
st_write(jobs, opt[["output"]], layer = opt[["output-layer"]], quiet = TRUE)
write.csv(st_drop_geometry(jobs), paste0(tools::file_path_sans_ext(opt[["output"]]), "_manifest.csv"), row.names = FALSE)
message("Wrote ", nrow(jobs), " direct-LAZ jobs: ", opt[["output"]])
