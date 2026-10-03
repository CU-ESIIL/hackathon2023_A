# Spectral clustering of Sentinel-2 cube
#
# Fits 50 spectral clusters using a reproducible random sample of pixels,
# then assigns every 10 m pixel to its nearest cluster centroid in blocks.
#
# Input:
#   data/cube_df.rds
#
# Outputs:
#   data/kmeans15_sample_fit_100k.rds
#   data/kmeans15_centers_100k.csv
#   data/spectral_clusters_k15.tif

library(terra)

# -------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------

input_file <- "data/cube_df.rds"

k <- 15L
sample_n <- 100000L
seed <- 16338L
nstart <- 100L
iter_max <- 10000L

# Number of raster rows classified at once.
# 20 rows = 351,400 pixels for this cube.
rows_per_block <- 20L

fit_file <- "data/kmeans15_sample_fit_100k.rds"
centers_file <- "data/kmeans15_centers_100k.csv"
cluster_raster_file <- "data/spectral_clusters_k15.tif"

# -------------------------------------------------------------------
# Read cube
# -------------------------------------------------------------------

message("Reading cube: ", Sys.time())

cube <- readRDS(input_file)

message(
  "Cube loaded: ",
  format(nrow(cube), big.mark = ","),
  " rows"
)

band_cols <- setdiff(names(cube), c("x", "y"))

if (length(band_cols) != 12L) {
  stop(
    "Expected 12 spectral bands; found ",
    length(band_cols),
    ": ",
    paste(band_cols, collapse = ", ")
  )
}

if (anyNA(cube)) {
  stop("Cube contains NA values.")
}

message("Spectral bands: ", paste(band_cols, collapse = ", "))

# -------------------------------------------------------------------
# Reconstruct grid geometry and verify row ordering
# -------------------------------------------------------------------

dx <- cube$x[2] - cube$x[1]

xmin_center <- min(cube$x)
xmax_center <- max(cube$x)

nx <- as.integer(
  round((xmax_center - xmin_center) / dx) + 1L
)

if (nrow(cube) %% nx != 0) {
  stop("Cube row count does not form a complete regular grid.")
}

ny <- as.integer(nrow(cube) / nx)

ymin_center <- min(cube$y)
ymax_center <- max(cube$y)

dy <- (ymax_center - ymin_center) / (ny - 1L)

message("Grid: ", nx, " columns x ", ny, " rows")
message("Resolution: ", dx, " m x ", dy, " m")

# Confirm the data frame is ordered raster-row-wise:
# x increases across a row and first row is the northernmost row.

expected_first_x <- xmin_center + (0:(nx - 1L)) * dx

if (!isTRUE(all.equal(
  cube$x[1:nx],
  expected_first_x,
  tolerance = 1e-6
))) {
  stop("Unexpected x ordering in cube.")
}

if (length(unique(cube$y[1:nx])) != 1L) {
  stop("Unexpected y ordering in first raster row.")
}

if (cube$y[1] <= cube$y[nrow(cube)]) {
  stop("Expected cube rows to run from north to south.")
}

# -------------------------------------------------------------------
# Fit k-means on a reproducible spectral sample
# -------------------------------------------------------------------

set.seed(seed)

sample_n <- min(sample_n, nrow(cube))
sample_idx <- sample.int(nrow(cube), sample_n)

sample_values <- as.matrix(
  cube[sample_idx, band_cols, drop = FALSE]
)

message(
  "Fitting k = ",
  k,
  " using ",
  format(sample_n, big.mark = ","),
  " sampled pixels: ",
  Sys.time()
)

fit <- kmeans(
  sample_values,
  centers = k,
  iter.max = iter_max,
  nstart = nstart
)

message("K-means fit complete: ", Sys.time())

# Save enough information to reproduce/inspect the fit
saveRDS(
  list(
    fit = fit,
    sample_idx = sample_idx,
    bands = band_cols,
    seed = seed,
    sample_n = sample_n,
    nstart = nstart,
    iter_max = iter_max
  ),
  fit_file
)

centers_out <- data.frame(
  cluster = seq_len(k),
  fit$centers,
  check.names = FALSE
)

write.csv(
  centers_out,
  centers_file,
  row.names = FALSE
)

print(table(fit$cluster))

centers <- fit$centers
center_ss <- rowSums(centers^2)

rm(sample_values)
gc()

# -------------------------------------------------------------------
# Create output 10 m cluster raster
# -------------------------------------------------------------------

r <- rast(
  nrows = ny,
  ncols = nx,
  xmin = xmin_center - dx / 2,
  xmax = xmax_center + dx / 2,
  ymin = ymin_center - dy / 2,
  ymax = ymax_center + dy / 2,
  crs = "EPSG:32610"
)

names(r) <- "kmeans15"

write_info <- writeStart(
  r,
  cluster_raster_file,
  overwrite = TRUE,
  datatype = "INT1U",
  gdal = c(
    "COMPRESS=LZW",
    "TILED=YES"
  )
)

message("Classifying full cube: ", Sys.time())

# -------------------------------------------------------------------
# Assign every pixel to nearest centroid, block by block
#
# Squared Euclidean distance:
# ||x-c||^2 = ||x||^2 + ||c||^2 - 2*x*c
#
# ||x||^2 is identical for all candidate centroids for a given pixel,
# so nearest centroid = maximum of:
# 2*x*c - ||c||^2
# -------------------------------------------------------------------

for (row_start in seq(1L, ny, by = rows_per_block)) {
  
  nrows_this <- min(
    rows_per_block,
    ny - row_start + 1L
  )
  
  first_cell <- (row_start - 1L) * nx + 1L
  last_cell <- (row_start + nrows_this - 1L) * nx
  
  block_values <- as.matrix(
    cube[first_cell:last_cell, band_cols, drop = FALSE]
  )
  
  scores <- 2 * (block_values %*% t(centers))
  
  scores <- sweep(
    scores,
    MARGIN = 2,
    STATS = center_ss,
    FUN = "-"
  )
  
  cluster_id <- max.col(
    scores,
    ties.method = "first"
  )
  
  writeValues(
    r,
    cluster_id,
    start = row_start,
    nrows = nrows_this
  )
  
  rm(block_values, scores, cluster_id)
  
  if (
    row_start == 1L ||
    row_start %% 1000L == 1L ||
    row_start + nrows_this - 1L == ny
  ) {
    pct <- 100 *
      (row_start + nrows_this - 1L) /
      ny
    
    message(
      sprintf(
        "Completed %.1f%% (%s)",
        pct,
        Sys.time()
      )
    )
    
    gc()
  }
}

writeStop(r)

message("Finished successfully: ", Sys.time())
message("Cluster raster: ", cluster_raster_file)