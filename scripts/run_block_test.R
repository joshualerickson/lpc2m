#!/usr/bin/env Rscript

# End-to-end, bounded multi-block run: stream -> normalize -> metrics ->
# native mosaic -> bilinear delivery projection. Normalized halo blocks are
# retained by default so metrics can be rerun without re-streaming EPT data.
suppressPackageStartupMessages({ library(sf); library(terra); library(parallel) })
suppressMessages(sf_use_s2(FALSE))

usage <- paste(
  "Usage: Rscript scripts/run_block_test.R --blocks JOBS.gpkg --layer NAME",
  "--delivery-template FINAL.tif --name STEM",
  "[--stream-workers 8] [--normalize-workers 48] [--metric-workers 64]",
  "[--workers N] [--buffer-m 60] [--families standard,canopy,graph] [--resume true|false]",
  "[--write-normalized true|false] [--normalized-dir DIR] [--metrics-dir DIR]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opts <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
required <- c("blocks", "layer", "delivery-template", "name")
if (!all(required %in% names(opts))) stop(usage, call. = FALSE)
opt_or <- function(name, default) if (name %in% names(opts)) opts[[name]] else default
legacy_workers <- if ("workers" %in% names(opts)) as.integer(opts[["workers"]]) else NA_integer_
stream_workers <- as.integer(opt_or("stream-workers", ifelse(is.na(legacy_workers), "8", legacy_workers)))
normalize_workers <- as.integer(opt_or("normalize-workers", ifelse(is.na(legacy_workers), "48", legacy_workers)))
metric_workers <- as.integer(opt_or("metric-workers", ifelse(is.na(legacy_workers), "64", legacy_workers)))
buffer_m <- as.numeric(opt_or("buffer-m", "60"))
families <- opt_or("families", "standard,canopy,graph")
families_vec <- trimws(strsplit(families, ",", fixed = TRUE)[[1]])
family_suffix <- c(standard = "standard", canopy = "canopy_tree", graph = "graph")
if (!length(families_vec) || any(!families_vec %in% names(family_suffix))) {
  stop("families must be a comma-separated subset of: standard, canopy, graph.", call. = FALSE)
}
resume <- tolower(opt_or("resume", "false")) %in% c("true", "t", "1", "yes")
write_normalized <- tolower(opt_or("write-normalized", "true")) %in% c("true", "t", "1", "yes")
if (any(is.na(c(stream_workers, normalize_workers, metric_workers))) ||
    any(c(stream_workers, normalize_workers, metric_workers) < 1L)) {
  stop("Each worker count must be >= 1.", call. = FALSE)
}

blocks <- st_read(opts[["blocks"]], layer = opts[["layer"]], quiet = TRUE)
if (!"block_id" %in% names(blocks) || !nrow(blocks)) stop("blocks must include block_id values.", call. = FALSE)
if (anyDuplicated(blocks$block_id)) stop("block_id values must be unique in the job layer.", call. = FALSE)
has_job_sources <- "quality_level" %in% names(blocks) &&
  (all(c("source_type", "ept_url", "source_files", "source_crs") %in% names(blocks)) || "ept_url" %in% names(blocks))
has_global_source <- all(c("ept-url", "quality-level") %in% names(opts))
if (!has_job_sources && !has_global_source) {
  stop("Jobs must contain ept_url and quality_level, or supply --ept-url and --quality-level for a single-source test.", call. = FALSE)
}
if (has_job_sources) {
  if (!"source_type" %in% names(blocks)) blocks$source_type <- "ept"
  blocks$source_type[is.na(blocks$source_type) | !nzchar(blocks$source_type)] <- "ept"
  if (any(!blocks$source_type %in% c("ept", "direct_laz"))) stop("source_type must be ept or direct_laz.", call. = FALSE)
  if (any(is.na(blocks$quality_level) | !toupper(blocks$quality_level) %in% c("QL1", "QL2"))) stop("Every job must have QL1 or QL2 quality_level.", call. = FALSE)
  ept <- blocks$source_type == "ept"
  direct <- blocks$source_type == "direct_laz"
  if (any(ept & (is.na(blocks$ept_url) | !nzchar(blocks$ept_url)))) stop("Every EPT job must have ept_url.", call. = FALSE)
  if (any(direct & (is.na(blocks$source_files) | !nzchar(blocks$source_files) | is.na(blocks$source_crs)))) stop("Every direct-LAZ job must have source_files and source_crs.", call. = FALSE)
}
name <- opts[["name"]]
work_dir <- file.path("work", name)
raw_dir <- file.path(work_dir, "raw")
normalized_dir <- opt_or("normalized-dir", file.path("data", "normalized", name))
normalized_scratch_dir <- file.path(work_dir, "normalized_scratch")
metric_root <- opt_or("metrics-dir", file.path("data", "metrics", name))
metric_dir <- file.path(metric_root, "blocks")
log_dir <- file.path(work_dir, "logs")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
if (write_normalized) dir.create(normalized_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(normalized_scratch_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metric_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) stop("PDAL is required; set PDAL_BIN or add it to PATH.", call. = FALSE)

run_stage <- function(id, stage, script, script_args) {
  log <- file.path(log_dir, paste0(id, "_", stage, ".log"))
  time_log <- file.path(log_dir, paste0(id, "_", stage, "_time.txt"))
  started <- Sys.time()
  status <- system2("/usr/bin/time", c("-v", "-o", time_log, "Rscript", script, script_args),
                    stdout = log, stderr = log, env = paste0("PDAL_BIN=", pdal_bin))
  elapsed_s <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  max_rss_kb <- NA_real_
  cpu_percent <- NA_real_
  if (file.exists(time_log)) {
    tl <- readLines(time_log, warn = FALSE)
    rss <- grep("Maximum resident set size", tl, value = TRUE)
    cpu <- grep("Percent of CPU", tl, value = TRUE)
    if (length(rss)) max_rss_kb <- as.numeric(sub(".*: *", "", rss[1]))
    if (length(cpu)) cpu_percent <- as.numeric(gsub("[^0-9.]", "", sub(".*: *", "", cpu[1])))
  }
  data.frame(block_id = id, stage = stage, status = status, elapsed_s = elapsed_s,
             max_rss_gb = max_rss_kb / 1024^2, cpu_percent = cpu_percent)
}

cached_stage <- function(id, stage) {
  data.frame(block_id = id, stage = stage, status = 0L, elapsed_s = 0,
             max_rss_gb = NA_real_, cpu_percent = NA_real_)
}

block_paths <- function(id) {
  raw <- file.path(raw_dir, paste0(id, "_raw.laz"))
  norm_dir <- if (write_normalized) normalized_dir else normalized_scratch_dir
  norm <- file.path(norm_dir, paste0(id, "_norm.laz"))
  metrics_name <- paste0(id, "_metrics")
  metric_files <- setNames(
    file.path(metric_dir, paste0(metrics_name, "_", family_suffix[families_vec], ".tif")),
    families_vec
  )
  list(raw = raw, norm = norm, metrics_name = metrics_name, metric_files = metric_files)
}

source_for_block <- function(id) {
  if (has_job_sources) {
    row <- blocks[match(id, blocks$block_id), ]
    list(source_type = as.character(row$source_type), ept_url = as.character(row$ept_url),
         source_files = as.character(row$source_files), source_crs = as.character(row$source_crs),
         quality_level = toupper(as.character(row$quality_level)))
  } else {
    list(source_type = "ept", ept_url = opts[["ept-url"]], source_files = NA_character_, source_crs = NA_character_,
         quality_level = toupper(opts[["quality-level"]]))
  }
}

# A task runs exactly one stage. The scheduler below allows a small number of
# network streams while independently filling CPU/RAM with normalize/metric work.
run_block_stage <- function(id, stage) {
  paths <- block_paths(id)
  source <- source_for_block(id)
  if (identical(stage, "stream")) {
    # A retained normalized block is sufficient to restart directly at metrics;
    # do not revisit EPT simply because the transient raw extract was removed.
    report <- if (resume && (file.exists(paths$norm) || file.exists(paths$raw))) {
      cached_stage(id, stage)
    } else {
      if (identical(source$source_type, "direct_laz")) {
        run_stage(id, stage, "scripts/extract_laz_block.R", c(
          "--source-files", source$source_files, "--source-crs", source$source_crs,
          "--aoi", opts[["blocks"]], "--layer", opts[["layer"]], "--block-id", id,
          "--output", paths$raw, "--buffer-m", as.character(buffer_m)
        ))
      } else {
        run_stage(id, stage, "scripts/extract_ept_block.R", c(
          "--ept-url", source$ept_url, "--aoi", opts[["blocks"]], "--layer", opts[["layer"]],
          "--block-id", id, "--output", paths$raw, "--buffer-m", as.character(buffer_m), "--requests", "16"
        ))
      }
    }
  } else if (identical(stage, "normalize")) {
    report <- if (resume && file.exists(paths$norm)) cached_stage(id, stage) else run_stage(id, stage, "scripts/normalize_laz_block.R", c(
      "--input", paths$raw, "--output", paths$norm
    ))
  } else if (identical(stage, "metrics")) {
    missing_families <- names(paths$metric_files)[!file.exists(paths$metric_files)]
    families_to_run <- if (resume) missing_families else families_vec
    report <- if (!length(families_to_run)) {
      cached_stage(id, stage)
    } else {
      run_stage(id, stage, "scripts/run_lidar_metrics_block.R", c(
        "--input", paths$norm, "--core", opts[["blocks"]], "--layer", opts[["layer"]], "--block-id", id,
        "--output-dir", metric_dir, "--quality-level", source$quality_level,
        "--families", paste(families_to_run, collapse = ","), "--name", paths$metrics_name
      ))
    }
  } else {
    stop("Unknown stage: ", stage, call. = FALSE)
  }
  list(id = id, stage = stage, report = report)
}

started <- Sys.time()
worker_limits <- c(stream = min(stream_workers, nrow(blocks)),
                   normalize = min(normalize_workers, nrow(blocks)),
                   metrics = min(metric_workers, nrow(blocks)))
pending <- list(stream = as.character(blocks$block_id), normalize = character(), metrics = character())
active <- list(stream = list(), normalize = list(), metrics = list())
report_rows <- list()
failed <- character()
report_path <- file.path(metric_root, paste0(name, "_performance.csv"))

launch_ready <- function(stage) {
  while (length(pending[[stage]]) && length(active[[stage]]) < worker_limits[[stage]]) {
    id <- pending[[stage]][1]
    pending[[stage]] <<- pending[[stage]][-1]
    job <- mcparallel(run_block_stage(id, stage), silent = TRUE)
    active[[stage]][[as.character(job$pid)]] <<- job
  }
}

record_progress <- function(result) {
  report_rows[[length(report_rows) + 1L]] <<- result$report
  if (result$report$status != 0L) {
    failed <<- c(failed, paste(result$id, result$stage, sep = ":"))
    return(invisible())
  }
  if (identical(result$stage, "stream")) pending$normalize <<- c(pending$normalize, result$id)
  if (identical(result$stage, "normalize")) pending$metrics <<- c(pending$metrics, result$id)
  if (identical(result$stage, "metrics")) {
    paths <- block_paths(result$id)
    # Raw EPT extracts are transient. A retained normalized block includes the
    # halo needed to rerun metrics without revisiting the EPT endpoint.
    if (file.exists(paths$raw)) unlink(paths$raw)
    if (!write_normalized && file.exists(paths$norm)) unlink(paths$norm)
  }
}

repeat {
  for (stage in names(worker_limits)) launch_ready(stage)
  for (stage in names(worker_limits)) {
    if (!length(active[[stage]])) next
    done <- mccollect(active[[stage]], wait = FALSE)
    if (!length(done)) next
    for (pid in names(done)) {
      active[[stage]][[pid]] <- NULL
      result <- done[[pid]]
      if (inherits(result, "try-error")) {
        id <- NA_character_
        report_rows[[length(report_rows) + 1L]] <- data.frame(block_id = id, stage = stage, status = 1L,
          elapsed_s = NA_real_, max_rss_gb = NA_real_, cpu_percent = NA_real_)
        failed <- c(failed, paste("unknown", stage, sep = ":"))
      } else {
        record_progress(result)
      }
    }
    if (length(report_rows)) write.csv(do.call(rbind, report_rows), report_path, row.names = FALSE)
  }
  if (!length(unlist(pending, use.names = FALSE)) && sum(lengths(active)) == 0L) break
  Sys.sleep(0.2)
}

report <- do.call(rbind, report_rows)
write.csv(report, report_path, row.names = FALSE)
if (length(failed) || any(report$status != 0L)) {
  message("Some blocks failed and remain resumable; mosaicking completed blocks. See ", log_dir)
}

complete <- vapply(blocks$block_id, function(id) {
  paths <- block_paths(id)
  all(file.exists(paths$metric_files))
}, logical(1))
completed_blocks <- blocks[complete, ]
if (!nrow(completed_blocks)) stop("No blocks completed all requested metric families; see ", log_dir, call. = FALSE)
if (any(!complete)) {
  incomplete <- st_drop_geometry(blocks[!complete, c("block_id")])
  write.csv(incomplete, file.path(metric_root, paste0(name, "_incomplete_blocks.csv")), row.names = FALSE)
}

# The delivery template supplies the historic output CRS, 30 m grid alignment,
# and extent. Metrics remain in native LAS CRS until this final bilinear step.
template_ref <- rast(opts[["delivery-template"]])
delivery_core <- st_transform(st_union(completed_blocks), crs(template_ref))
# A historical delivery raster defines CRS, resolution, and grid alignment,
# not the spatial limit of a new AOI.  Build an aligned delivery grid over the
# completed AOI so a valid Oregon/Idaho/etc. run is not rejected merely because
# the reference raster's existing extent is elsewhere.
delivery_extent <- align(ext(vect(delivery_core)), template_ref[[1]], snap = "out")
template <- rast(delivery_extent, resolution = res(template_ref[[1]]), crs = crs(template_ref[[1]]))
mosaic_rows <- lapply(families_vec, function(family) {
  suffix <- family_suffix[[family]]
  files <- file.path(metric_dir, paste0(completed_blocks$block_id, "_metrics_", suffix, ".tif"))
  native_mosaic <- mosaic(sprc(files))
  native_path <- file.path(metric_root, paste0(name, "_", suffix, "_native_mosaic.tif"))
  writeRaster(native_mosaic, native_path, overwrite = resume)
  delivery <- project(native_mosaic, template, method = "bilinear")
  delivery_path <- file.path(metric_root, paste0(name, "_", suffix, "_delivery_bilinear.tif"))
  writeRaster(delivery, delivery_path, overwrite = resume)
  data.frame(family = family, native_mosaic = native_path, delivery_mosaic = delivery_path)
})
mosaics <- do.call(rbind, mosaic_rows)

manifest <- data.frame(
  name = name, source_mode = if (has_job_sources && any(blocks$source_type == "direct_laz")) "mixed_or_direct_job_manifest" else if (has_job_sources) "ept_job_manifest" else "single_source_test",
  stream_workers = worker_limits[["stream"]], normalize_workers = worker_limits[["normalize"]],
  metric_workers = worker_limits[["metrics"]], blocks = nrow(blocks), completed_blocks = nrow(completed_blocks),
  incomplete_blocks = sum(!complete), buffer_m = buffer_m,
  families = paste(families_vec, collapse = ","),
  write_normalized = write_normalized, normalized_dir = if (write_normalized) normalized_dir else NA_character_,
  metrics_dir = metric_root,
  elapsed_s = as.numeric(difftime(Sys.time(), started, units = "secs"))
)
write.csv(manifest, file.path(metric_root, paste0(name, "_manifest.csv")), row.names = FALSE)
write.csv(mosaics, file.path(metric_root, paste0(name, "_mosaics.csv")), row.names = FALSE)
for (i in seq_len(nrow(mosaics))) {
  message("Wrote ", mosaics$family[i], " native mosaic: ", mosaics$native_mosaic[i])
  message("Wrote ", mosaics$family[i], " bilinear delivery mosaic: ", mosaics$delivery_mosaic[i])
}
message("Wrote performance report: ", report_path)
