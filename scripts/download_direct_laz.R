#!/usr/bin/env Rscript

# Download the direct-USGS LAZ tiles selected by plan_usgs_direct_laz.R.
# Downloads are atomic and resumable at the file level: completed files are
# reused, while interrupted .partial files are retried on the next run.
suppressPackageStartupMessages({ library(curl); library(parallel) })

usage <- paste(
  "Usage: Rscript scripts/download_direct_laz.R --plan PLAN.csv --output-dir DIR",
  "[--workers 8] [--resume true|false] [--retries 3]",
  sep = "\n"
)
args <- commandArgs(trailingOnly = TRUE)
if (!length(args) || length(args) %% 2L != 0L) stop(usage, call. = FALSE)
opt <- setNames(args[seq(2, length(args), 2)], sub("^--", "", args[seq(1, length(args), 2)]))
if (!all(c("plan", "output-dir") %in% names(opt))) stop(usage, call. = FALSE)
opt_or <- function(name, default) if (name %in% names(opt)) opt[[name]] else default
is_true <- function(x) tolower(x) %in% c("true", "t", "1", "yes")
workers <- as.integer(opt_or("workers", "8"))
retries <- as.integer(opt_or("retries", "3"))
resume <- is_true(opt_or("resume", "true"))
if (is.na(workers) || workers < 1L || is.na(retries) || retries < 1L) stop("workers and retries must be positive integers.", call. = FALSE)

plan <- read.csv(opt[["plan"]], stringsAsFactors = FALSE, na.strings = c("", "NA"))
required <- c("workunit", "source_url", "file_name", "bytes")
if (!all(required %in% names(plan))) stop("Direct-LAZ plan must contain: ", paste(required, collapse = ", "), call. = FALSE)
if (!nrow(plan)) {
  message("Direct-LAZ plan is empty; nothing to download.")
  quit(status = 0L)
}
safe_component <- function(x) gsub("[^A-Za-z0-9._-]", "_", x)
target <- file.path(normalizePath(opt[["output-dir"]], mustWork = FALSE), safe_component(plan$workunit), plan$file_name)
local_supplied <- "local_path" %in% names(plan) & !is.na(plan$local_path) & nzchar(plan$local_path) & file.exists(plan$local_path)
target[local_supplied] <- normalizePath(plan$local_path[local_supplied], mustWork = TRUE)

download_one <- function(i) {
  if (local_supplied[i]) return(data.frame(row = i, status = "local", local_path = target[i], bytes_on_disk = file.info(target[i])$size, error = NA_character_))
  dir.create(dirname(target[i]), recursive = TRUE, showWarnings = FALSE)
  expected <- suppressWarnings(as.numeric(plan$bytes[i]))
  if (resume && file.exists(target[i]) && (!is.finite(expected) || file.info(target[i])$size == expected)) {
    return(data.frame(row = i, status = "cached", local_path = target[i], bytes_on_disk = file.info(target[i])$size, error = NA_character_))
  }
  partial <- paste0(target[i], ".partial")
  last_error <- NA_character_
  for (attempt in seq_len(retries)) {
    if (file.exists(partial)) unlink(partial)
    ok <- tryCatch({
      curl_download(plan$source_url[i], partial, quiet = TRUE, mode = "wb")
      actual <- file.info(partial)$size
      if (is.finite(expected) && actual != expected) stop("expected ", expected, " bytes; received ", actual)
      if (file.exists(target[i])) unlink(target[i])
      if (!file.rename(partial, target[i])) stop("could not commit completed download")
      TRUE
    }, error = function(e) { last_error <<- conditionMessage(e); FALSE })
    if (ok) return(data.frame(row = i, status = "downloaded", local_path = target[i], bytes_on_disk = file.info(target[i])$size, error = NA_character_))
  }
  if (file.exists(partial)) unlink(partial)
  data.frame(row = i, status = "failed", local_path = target[i], bytes_on_disk = NA_real_, error = last_error)
}

indices <- seq_len(nrow(plan))
results <- if (.Platform$OS.type == "unix" && workers > 1L) {
  mclapply(indices, download_one, mc.cores = min(workers, length(indices)))
} else lapply(indices, download_one)
results <- do.call(rbind, results)
plan$download_status <- results$status[match(seq_len(nrow(plan)), results$row)]
plan$local_path <- results$local_path[match(seq_len(nrow(plan)), results$row)]
plan$bytes_on_disk <- results$bytes_on_disk[match(seq_len(nrow(plan)), results$row)]
plan$download_error <- results$error[match(seq_len(nrow(plan)), results$row)]
write.csv(plan, opt[["plan"]], row.names = FALSE)
counts <- table(plan$download_status)
message("Direct-LAZ acquisition: ", paste(names(counts), as.integer(counts), sep = "=", collapse = ", "))
if (any(plan$download_status == "failed")) {
  message("Some direct-LAZ tiles failed. Available tiles remain processable and failures stay resumable in the plan CSV.")
}
