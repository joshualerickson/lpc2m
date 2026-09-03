#!/usr/bin/env Rscript

# Make source-specific jobs directly from authoritative EPT source footprints.
# This avoids the National Map LPC index for known EPT coverage.
suppressPackageStartupMessages(library(sf))
usage <- paste(
  "Usage: Rscript scripts/make_ept_source_job_manifest.R --blocks BLOCKS.gpkg --layer NAME",
  "--ept-sources SOURCES.gpkg --ept-layer NAME --profiles SOURCE_PROFILES.csv",
  "--output JOBS.gpkg --output-layer NAME",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
need <- c("blocks", "layer", "ept-sources", "ept-layer", "profiles", "output", "output-layer")
if (!all(need %in% names(opt))) stop(usage, call. = FALSE)
if (file.exists(opt[["output"]])) stop("Refusing to overwrite: ", opt[["output"]], call. = FALSE)

blocks <- st_read(opt[["blocks"]], layer = opt[["layer"]], quiet = TRUE)
sources <- st_read(opt[["ept-sources"]], layer = opt[["ept-layer"]], quiet = TRUE)
if (!nrow(blocks) || !"block_id" %in% names(blocks) || is.na(st_crs(blocks))) stop("Blocks must have block_id and a CRS.", call. = FALSE)
if (!all(c("ept_name", "ept_url") %in% names(sources))) stop("EPT source layer must contain ept_name and ept_url.", call. = FALSE)
sources <- st_make_valid(st_transform(sources, st_crs(blocks)))

if (!file.exists(opt[["profiles"]])) {
  template <- unique(st_drop_geometry(sources)[, c("ept_name", "ept_url")])
  template$quality_level <- NA_character_
  template$acquisition_start <- NA_character_
  template$acquisition_end <- NA_character_
  template_path <- paste0(tools::file_path_sans_ext(opt[["profiles"]]), "_template.csv")
  write.csv(template, template_path, row.names = FALSE)
  stop("EPT source profile file is required. Wrote template: ", template_path, call. = FALSE)
}
profiles <- read.csv(opt[["profiles"]], stringsAsFactors = FALSE, na.strings = c("", "NA"))
if (!all(c("ept_name", "quality_level") %in% names(profiles))) stop("Profiles must contain ept_name and quality_level.", call. = FALSE)
profiles$quality_level <- toupper(gsub("[^A-Za-z0-9]", "", profiles$quality_level))
if (any(!profiles$quality_level %in% c("QL1", "QL2"))) stop("Every profile used must specify QL1 or QL2.", call. = FALSE)
profiles$ept_url <- NULL
sources <- merge(sources, profiles, by = "ept_name", all.x = TRUE, sort = FALSE)
sources <- sources[!is.na(sources$quality_level), ]
if (!nrow(sources)) stop("No EPT sources have a QL1/QL2 profile.", call. = FALSE)
# Newest acquisition wins; QL1 breaks equal-date ties.
date_rank <- suppressWarnings(as.numeric(as.Date(sources$acquisition_end)))
sources <- sources[order(-ifelse(is.na(date_rank), -Inf, date_rank), sources$quality_level != "QL1"), ]

hits <- st_intersects(blocks, sources)
rows <- list(); geoms <- list()
for (i in seq_len(nrow(blocks))) {
  remaining <- st_geometry(blocks[i, ])
  for (j in hits[[i]]) {
    piece <- suppressWarnings(st_intersection(remaining, st_geometry(sources[j, ])))
    if (!length(piece) || sum(as.numeric(st_area(piece))) < 1) next
    piece <- st_union(piece)
    rows[[length(rows) + 1L]] <- data.frame(
      block_id = paste(blocks$block_id[i], sources$ept_name[j], sep = "__"),
      source_block_id = blocks$block_id[i], source_type = "ept", source_name = sources$ept_name[j],
      ept_name = sources$ept_name[j], ept_url = sources$ept_url[j], source_files = NA_character_, source_crs = NA_character_,
      quality_level = sources$quality_level[j], acquisition_start = sources$acquisition_start[j],
      acquisition_end = sources$acquisition_end[j], source_area_m2 = sum(as.numeric(st_area(piece))), stringsAsFactors = FALSE
    )
    geoms[[length(geoms) + 1L]] <- piece[[1]]
    remaining <- suppressWarnings(st_difference(remaining, piece))
    if (!length(remaining) || sum(as.numeric(st_area(remaining))) < 1) break
  }
}
if (!length(rows)) stop("No profiled EPT source intersects the processing blocks.", call. = FALSE)
jobs <- st_sf(do.call(rbind, rows), geometry = st_sfc(geoms, crs = st_crs(blocks)))
st_write(jobs, opt[["output"]], layer = opt[["output-layer"]], quiet = TRUE)
write.csv(st_drop_geometry(jobs), paste0(tools::file_path_sans_ext(opt[["output"]]), "_manifest.csv"), row.names = FALSE)
message("Wrote ", nrow(jobs), " EPT-source jobs: ", opt[["output"]])
