# Moving-window spectral cluster richness
#
# Counts the number of unique spectral clusters in a centered
# moving window over the k = 15 classification.
#
# Input:
#   data/spectral_clusters_k15.tif
#
# Output for window_n = 3:
#   data/spectral_richness_k15_w3.tif

library(terra)
library(Rcpp)

input_file <- "data/spectral_clusters_k15.tif"

# Change this to 3, 5, 7, 11, etc. for other spatial scales.
args <- commandArgs(trailingOnly = TRUE)

window_n <- if (length(args) > 0) {
  as.integer(args[1])
} else {
  3L
}

if (window_n < 3 || window_n %% 2 == 0) {
  stop("Window size must be an odd integer >= 3.")
}

output_file <- sprintf(
  "data/spectral_richness_k15_w%d.tif",
  window_n
)

message("Reading cluster raster: ", Sys.time())

clusters <- rast(input_file)

# Correct stale layer name from original k=50 script
names(clusters) <- sprintf(
  "spectral_richness_k15_w%d",
  window_n
)

message(clusters)

# ------------------------------------------------------------
# C++ function to count unique cluster IDs in each window.
#
# Because cluster IDs are 1...15, we can represent presence
# of each cluster with one bit in an integer.
#
# A complete window is required. Cells around the outer raster
# edge therefore become NA.
# ------------------------------------------------------------

cppFunction('
NumericVector count_unique_clusters(
    NumericVector x,
    size_t ni,
    size_t nw) {

  NumericVector out(ni);

  size_t start = 0;

  for (size_t i = 0; i < ni; i++) {

    unsigned int mask = 0;
    bool complete = true;

    size_t end = start + nw;

    for (size_t j = start; j < end; j++) {

      if (NumericVector::is_na(x[j])) {
        complete = false;
        break;
      }

      int v = static_cast<int>(x[j]);

      if (v >= 1 && v <= 15) {
        mask |= (1u << (v - 1));
      }
    }

    if (!complete) {
      out[i] = NA_REAL;
    } else {
      out[i] = __builtin_popcount(mask);
    }

    start = end;
  }

  return out;
}
')

# ------------------------------------------------------------
# Small self-test
# ------------------------------------------------------------

test <- rast(nrows = 3, ncols = 3)
values(test) <- c(
  1, 1, 2,
  1, 3, 3,
  4, 4, 4
)

test_out <- focalCpp(
  test,
  w = 3,
  fun = count_unique_clusters,
  fillvalue = NA
)

if (values(test_out)[5] != 4) {
  stop("Moving-window self-test failed.")
}

message("Self-test passed.")

# ------------------------------------------------------------
# Full moving-window calculation
# ------------------------------------------------------------

message(
  "Calculating ",
  window_n, " x ", window_n,
  " spectral richness: ",
  Sys.time()
)

richness <- focalCpp(
  clusters,
  w = window_n,
  fun = count_unique_clusters,
  fillvalue = NA,
  filename = output_file,
  overwrite = TRUE,
  wopt = list(
    datatype = "INT1U",
    gdal = c(
      "COMPRESS=LZW",
      "TILED=YES"
    )
  )
)

names(richness) <- sprintf(
  "spectral_richness_k15_w%d",
  window_n
)

message(richness)

message("Finished successfully: ", Sys.time())
message("Output: ", output_file)