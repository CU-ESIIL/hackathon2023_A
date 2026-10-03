# Evaluate number of spectral clusters
#
# Uses a reproducible 10,000-pixel sample from the Sentinel-2 cube.
# Diagnostics:
#   - Calinski-Harabasz index (higher = better separation/compactness)
#   - Total within-cluster sum of squares (look for elbow)
#   - Minimum cluster size
#
# This script does NOT classify the full cube.

input_file <- "data/cube_df.rds"

seed <- 16338L
sample_n <- 10000L
k_values <- 2:50

nstart <- 20L
iter_max <- 1000L

message("Reading cube: ", Sys.time())

cube <- readRDS(input_file)

message(
  "Cube loaded: ",
  format(nrow(cube), big.mark = ","),
  " rows"
)

band_cols <- setdiff(names(cube), c("x", "y"))

if (length(band_cols) != 12L) {
  stop("Expected 12 spectral bands; found ", length(band_cols))
}

# ------------------------------------------------------------
# Draw one fixed sample used for every value of k
# ------------------------------------------------------------

set.seed(seed)

sample_idx <- sample.int(
  nrow(cube),
  sample_n,
  replace = FALSE
)

x <- as.matrix(
  cube[sample_idx, band_cols, drop = FALSE]
)

# Full 397-million-row cube is no longer needed
rm(cube)
gc()

message(
  "Validation sample created: ",
  format(nrow(x), big.mark = ","),
  " pixels"
)

# ------------------------------------------------------------
# Fit k = 2 ... 50
# ------------------------------------------------------------

results <- data.frame(
  k = k_values,
  CH = NA_real_,
  tot_withinss = NA_real_,
  between_ss = NA_real_,
  min_cluster_n = NA_integer_,
  min_cluster_pct = NA_real_,
  max_cluster_n = NA_integer_,
  ifault = NA_integer_
)

fits <- vector("list", length(k_values))
names(fits) <- as.character(k_values)

for (j in seq_along(k_values)) {
  
  k <- k_values[j]
  
  # Separate reproducible seed for each k
  set.seed(seed + k)
  
  message(
    "Fitting k = ", k,
    " (", Sys.time(), ")"
  )
  
  fit <- kmeans(
    x,
    centers = k,
    iter.max = iter_max,
    nstart = nstart,
    algorithm = "Hartigan-Wong"
  )
  
  fits[[j]] <- fit
  
  n <- nrow(x)
  
  # Calinski-Harabasz:
  # (between SS / (k - 1)) / (within SS / (n - k))
  ch <- (
    fit$betweenss / (k - 1)
  ) / (
    fit$tot.withinss / (n - k)
  )
  
  cluster_sizes <- table(fit$cluster)
  
  results$CH[j] <- ch
  results$tot_withinss[j] <- fit$tot.withinss
  results$between_ss[j] <- fit$betweenss
  results$min_cluster_n[j] <- min(cluster_sizes)
  results$min_cluster_pct[j] <-
    100 * min(cluster_sizes) / n
  results$max_cluster_n[j] <- max(cluster_sizes)
  
  results$ifault[j] <-
    if (is.null(fit$ifault)) NA_integer_ else fit$ifault
  
  message(
    sprintf(
      "  CH = %.2f | min cluster = %d (%.3f%%) | ifault = %s",
      ch,
      min(cluster_sizes),
      100 * min(cluster_sizes) / n,
      results$ifault[j]
    )
  )
}

# ------------------------------------------------------------
# Save diagnostics
# ------------------------------------------------------------

write.csv(
  results,
  "data/k_selection_diagnostics.csv",
  row.names = FALSE
)

saveRDS(
  list(
    fits = fits,
    results = results,
    sample_idx = sample_idx,
    bands = band_cols,
    seed = seed,
    sample_n = sample_n,
    nstart = nstart
  ),
  "data/k_selection_diagnostics.rds"
)

# ------------------------------------------------------------
# Plots
# ------------------------------------------------------------

png(
  "data/k_selection_CH.png",
  width = 1200,
  height = 800,
  res = 130
)

plot(
  results$k,
  results$CH,
  type = "o",
  pch = 16,
  xlab = "Number of clusters (k)",
  ylab = "Calinski-Harabasz index",
  main = "Spectral cluster-number diagnostic"
)

dev.off()

png(
  "data/k_selection_WSS.png",
  width = 1200,
  height = 800,
  res = 130
)

plot(
  results$k,
  results$tot_withinss,
  type = "o",
  pch = 16,
  xlab = "Number of clusters (k)",
  ylab = "Total within-cluster sum of squares",
  main = "K-means within-cluster sum of squares"
)

dev.off()

best_ch_k <- results$k[which.max(results$CH)]

message("")
message("Maximum CH occurs at k = ", best_ch_k)

bad_fits <- results[!is.na(results$ifault) &
                      results$ifault != 0, ]

if (nrow(bad_fits) > 0) {
  message(
    "WARNING: nonzero ifault for k = ",
    paste(bad_fits$k, collapse = ", ")
  )
} else {
  message("All k-means fits returned ifault = 0.")
}

message("Finished: ", Sys.time())