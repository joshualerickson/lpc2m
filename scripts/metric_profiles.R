# Explicit, versioned settings for the retained LiDAR products.
# QL1 homogenizes density; QL2 selects the dominant PointSourceID inside each
# metric function.  Do not infer quality level from local point density.
metric_profile <- function(name = "production_v1") {
  if (!identical(name, "production_v1")) {
    stop("Unknown metric profile: ", name, call. = FALSE)
  }

  list(
    name = name,
    standard = list(classes = 0:5, zmin = 0, zmax = 70),
    canopy = list(
      classes = 0:5, zmin = 0, zmax = 70,
      ql1_density = 15, ql1_decimation_res = 3,
      tree_window = function(x) x * 0.17 + 3
    ),
    graph = list(
      # Validated QL2 catalog filter. Scan angle remains unrestricted.
      drop_classes = c(7L, 9L), zmin = 1, zmax = 12.1,
      first_returns_only = TRUE, intensity_min = 5,
      drop_withheld = TRUE, drop_overlap = TRUE,
      z_1 = 1, z_20 = 6.1, z_40 = 12.1,
      voxel_res = 3, edge_thresholds = c(3, 3),
      ql1_points_per_voxel = 1, ql1_voxel_res = 3
    )
  )
}
