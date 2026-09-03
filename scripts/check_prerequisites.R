#!/usr/bin/env Rscript

# Read-only environment check for colleagues setting up this project.
required_packages <- c("sf", "terra", "jsonlite", "curl", "lidR", "lidar.metrics")
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

cat("R:", R.version.string, "\n")
for (pkg in required_packages) {
  if (pkg %in% missing) cat("MISSING R package:", pkg, "\n")
  else cat("R package:", pkg, as.character(utils::packageVersion(pkg)), "\n")
}

pdal_bin <- Sys.getenv("PDAL_BIN", unset = Sys.which("pdal"))
if (!nzchar(pdal_bin) || !file.exists(pdal_bin)) {
  cat("MISSING PDAL: set PDAL_BIN or add pdal to PATH.\n")
  quit(status = 1L)
}
cat("PDAL:", pdal_bin, "\n")
drivers <- tryCatch(system2(pdal_bin, "--drivers", stdout = TRUE, stderr = TRUE), error = function(e) character())
has_ept <- any(grepl("readers\\.ept", drivers))
has_las <- any(grepl("writers\\.las", drivers))
cat("PDAL readers.ept:", has_ept, "\n")
cat("PDAL writers.las:", has_las, "\n")

if (length(missing) || !has_ept || !has_las) quit(status = 1L)
cat("Prerequisites OK.\n")
