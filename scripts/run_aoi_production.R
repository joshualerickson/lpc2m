#!/usr/bin/env Rscript

# One-command production entry point:
# AOI polygon(s) -> processing grid -> USGS source-specific jobs -> metrics.
suppressPackageStartupMessages({ library(sf) })

usage <- paste(
  "Usage: Rscript scripts/run_aoi_production.R --aoi AOI.gpkg --layer NAME --name STEM",
  "--delivery-template DELIVERY_GRID.tif --normalized-dir DIR --metrics-dir DIR",
  "[--block-m 500] [--stream-workers 8] [--normalize-workers 48] [--metric-workers 64]",
  "[--buffer-m 60] [--families standard,canopy,graph] [--filter-field FIELD --filter-value VALUE]",
  "[--ept-sources FILE --ept-source-layer NAME --ept-profiles FILE]",
  "[--direct-laz-plan PLAN.csv --direct-laz-tiles TILES.gpkg --direct-laz-cache-dir DIR]",
  "[--download-workers 8] [--write-normalized true|false] [--resume true|false] [--preflight-only true|false] [--pdal-bin PATH]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default
is_true <- function(x) tolower(x) %in% c("true", "t", "1", "yes")
preflight_only <- is_true(opt_or("preflight-only", "false"))
required <- c("aoi", "layer", "name", "metrics-dir")
if (!preflight_only) required <- c(required, "delivery-template", "normalized-dir", "metrics-dir")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)

block_m <- as.numeric(opt_or("block-m", "500"))
if (!is.finite(block_m) || block_m <= 0) stop("--block-m must be positive.", call. = FALSE)
name <- opts[["name"]]
metrics_dir <- opts[["metrics-dir"]]
provenance_dir <- file.path(metrics_dir, "provenance")
dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)
blocks_path <- file.path(provenance_dir, paste0(name, "_", as.integer(block_m), "m_blocks.gpkg"))
blocks_layer <- paste0(name, "_blocks")
jobs_path <- file.path(provenance_dir, paste0(name, "_jobs.gpkg"))
jobs_layer <- paste0(name, "_jobs")
coverage_path <- file.path(provenance_dir, paste0(name, "_jobs_coverage.gpkg"))
resume <- is_true(opt_or("resume", "false"))
direct_ept <- all(c("ept-sources", "ept-source-layer", "ept-profiles") %in% names(opts))
direct_laz <- all(c("direct-laz-plan", "direct-laz-tiles", "direct-laz-cache-dir") %in% names(opts))
if (any(c("ept-sources", "ept-source-layer", "ept-profiles") %in% names(opts)) && !direct_ept) {
  stop("Provide all of --ept-sources, --ept-source-layer, and --ept-profiles.", call. = FALSE)
}
if (any(c("direct-laz-plan", "direct-laz-tiles", "direct-laz-cache-dir") %in% names(opts)) && !direct_laz) {
  stop("Provide all of --direct-laz-plan, --direct-laz-tiles, and --direct-laz-cache-dir.", call. = FALSE)
}

run_r <- function(script, script_args, env = character()) {
  # system2() invokes a shell on this platform; quote values such as forest
  # names and paths before forwarding them to the child R script.
  status <- system2("Rscript", c(shQuote(script), shQuote(script_args)), env = env)
  if (status != 0L) stop("Failed: ", script, call. = FALSE)
}

if (file.exists(blocks_path)) {
  if (!resume) stop("Processing grid already exists; use --resume true or choose a new --name: ", blocks_path, call. = FALSE)
  message("Reusing processing grid: ", blocks_path)
} else {
  filter_args <- if (all(c("filter-field", "filter-value") %in% names(opts))) c("--filter-field", opts[["filter-field"]], "--filter-value", opts[["filter-value"]]) else character()
  if (xor("filter-field" %in% names(opts), "filter-value" %in% names(opts))) stop("Provide both --filter-field and --filter-value.", call. = FALSE)
  run_r("scripts/make_processing_grid.R", c(
    "--input", opts[["aoi"]], "--layer", opts[["layer"]], "--block-m", as.character(block_m),
    "--output", blocks_path, "--output-layer", blocks_layer, filter_args
  ))
}

# Tile acquisition is independent of the block manifest. Re-run it during a
# resumed job so missing/previously interrupted source tiles are repaired.
if (direct_laz && !preflight_only) {
  run_r("scripts/download_direct_laz.R", c(
    "--plan", opts[["direct-laz-plan"]], "--output-dir", opts[["direct-laz-cache-dir"]],
    "--workers", opt_or("download-workers", opt_or("stream-workers", "8")), "--resume", opt_or("resume", "false")
  ))
}
available_direct_laz <- FALSE
if (direct_laz && !preflight_only) {
  direct_plan <- read.csv(opts[["direct-laz-plan"]], stringsAsFactors = FALSE, na.strings = c("", "NA"))
  available_direct_laz <- "local_path" %in% names(direct_plan) && any(!is.na(direct_plan$local_path) & nzchar(direct_plan$local_path) & file.exists(direct_plan$local_path))
  if (!available_direct_laz) message("No direct-LAZ source tile is currently available; continuing with other source types.")
}
if (file.exists(jobs_path) && direct_laz && resume &&
    "download_status" %in% names(direct_plan) && any(direct_plan$download_status == "downloaded")) {
  message("New direct-LAZ tiles were acquired; rebuilding the unified job manifest.")
  unlink(jobs_path)
  direct_component <- file.path(provenance_dir, paste0(name, "_direct_laz_jobs.gpkg"))
  if (file.exists(direct_component)) unlink(direct_component)
}

if (file.exists(jobs_path)) {
  if (!resume) stop("USGS job manifest already exists; use --resume true or choose a new --name: ", jobs_path, call. = FALSE)
  message("Reusing USGS job manifest: ", jobs_path)
  if (preflight_only && !file.exists(coverage_path)) {
    stop("The existing job manifest predates coverage footprints. Choose a new --name for a fresh preflight.", call. = FALSE)
  }
} else {
  component_paths <- character()
  if (direct_ept) {
    ept_jobs_path <- file.path(provenance_dir, paste0(name, "_ept_jobs.gpkg"))
    if (!file.exists(ept_jobs_path)) {
      run_r("scripts/make_ept_source_job_manifest.R", c(
        "--blocks", blocks_path, "--layer", blocks_layer,
        "--ept-sources", opts[["ept-sources"]], "--ept-layer", opts[["ept-source-layer"]], "--profiles", opts[["ept-profiles"]],
        "--output", ept_jobs_path, "--output-layer", "ept_jobs"
      ))
    } else message("Reusing EPT component manifest: ", ept_jobs_path)
    component_paths <- c(component_paths, ept_jobs_path)
  }
  if (available_direct_laz) {
    direct_jobs_path <- file.path(provenance_dir, paste0(name, "_direct_laz_jobs.gpkg"))
    run_r("scripts/make_direct_laz_job_manifest.R", c(
      "--blocks", blocks_path, "--layer", blocks_layer,
      "--tiles", opts[["direct-laz-tiles"]], "--tile-layer", "direct_laz_tiles",
      "--plan", opts[["direct-laz-plan"]], "--buffer-m", opt_or("buffer-m", "60"),
      "--output", direct_jobs_path, "--output-layer", "direct_laz_jobs"
    ))
    component_paths <- c(component_paths, direct_jobs_path)
  }
  if (length(component_paths)) {
    components <- lapply(seq_along(component_paths), function(i) st_read(component_paths[i], quiet = TRUE))
    jobs <- do.call(rbind, components)
    st_write(jobs, jobs_path, layer = jobs_layer, quiet = TRUE)
    write.csv(st_drop_geometry(jobs), paste0(tools::file_path_sans_ext(jobs_path), "_manifest.csv"), row.names = FALSE)
    message("Wrote ", nrow(jobs), " unified source jobs: ", jobs_path)
  } else if (!direct_ept && !direct_laz) {
    run_r("scripts/make_usgs_job_manifest.R", c(
      "--blocks", blocks_path, "--layer", blocks_layer,
      "--output", jobs_path, "--output-layer", jobs_layer
    ))
  } else if (!length(component_paths)) {
    stop("No EPT or acquired direct-LAZ job is available to process; inspect provenance and the direct-LAZ plan.", call. = FALSE)
  }
}

if (preflight_only) {
  message("Preflight complete. Inspect EPT coverage: ", coverage_path)
  quit(status = 0L)
}

runner_args <- c(
  "--blocks", jobs_path, "--layer", jobs_layer,
  "--delivery-template", opts[["delivery-template"]], "--name", name,
  "--stream-workers", opt_or("stream-workers", "8"),
  "--normalize-workers", opt_or("normalize-workers", "48"),
  "--metric-workers", opt_or("metric-workers", "64"),
  "--buffer-m", opt_or("buffer-m", "60"), "--families", opt_or("families", "standard,canopy,graph"),
  "--write-normalized", opt_or("write-normalized", "true"),
  "--normalized-dir", opts[["normalized-dir"]], "--metrics-dir", metrics_dir,
  "--resume", opt_or("resume", "false")
)
pdal_bin <- opt_or("pdal-bin", Sys.getenv("PDAL_BIN", unset = Sys.which("pdal")))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("PDAL is required; set PDAL_BIN or add it to PATH.", call. = FALSE)
run_r("scripts/run_block_test.R", runner_args, env = paste0("PDAL_BIN=", pdal_bin))
message("Production AOI run completed: ", name)
