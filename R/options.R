#' Create validated LiDAR processing options
#'
#' @param block_m Core processing-block width in metres.
#' @param buffer_m Halo width in metres.
#' @param families Metric families to produce.
#' @param download_workers,stream_workers,normalize_workers,metric_workers Stage worker limits.
#' @param write_normalized Retain normalized halo LAZ blocks.
#' @param resume Reuse completed stages and outputs.
#' @return A named list of validated processing options.
#' @export
lidar_options <- function(block_m = 500, buffer_m = 60,
                          families = c("standard", "canopy", "graph"),
                          download_workers = 8, stream_workers = 8, normalize_workers = 48,
                          metric_workers = 64, write_normalized = TRUE,
                          resume = TRUE) {
  if (!is.numeric(block_m) || length(block_m) != 1L || !is.finite(block_m) || block_m <= 0) stop("block_m must be one positive number.", call. = FALSE)
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || !is.finite(buffer_m) || buffer_m < 0) stop("buffer_m must be one non-negative number.", call. = FALSE)
  families <- unique(as.character(families))
  if (!length(families) || any(!families %in% c("standard", "canopy", "graph"))) stop("families must be a non-empty subset of standard, canopy, graph.", call. = FALSE)
  workers <- c(download_workers = download_workers, stream_workers = stream_workers, normalize_workers = normalize_workers, metric_workers = metric_workers)
  if (any(!is.finite(workers)) || any(workers < 1) || any(workers != as.integer(workers))) stop("Worker limits must be positive integers.", call. = FALSE)
  list(block_m = as.numeric(block_m), buffer_m = as.numeric(buffer_m), families = families,
       download_workers = as.integer(download_workers), stream_workers = as.integer(stream_workers), normalize_workers = as.integer(normalize_workers),
       metric_workers = as.integer(metric_workers), write_normalized = isTRUE(write_normalized), resume = isTRUE(resume))
}
