# Generate 10 m Sentinel-2 spectral data cube
# Background version of get-satellite-imagery.qmd

# Environment:
#   mamba env create -f environment.yml
#   conda activate sentinel-cube
#
# Run:
#   Rscript get-satellite-imagery-background.R
#
# Successful CyVerse run:
#   R 4.5.3
#   4 cores, 128 GiB RAM


# Sys.setenv("PROJ_LIB" = "/opt/conda/share/proj")
conda_prefix <- Sys.getenv("CONDA_PREFIX")

if (nzchar(conda_prefix)) {
  Sys.setenv(
    "PROJ_LIB" = file.path(conda_prefix, "share", "proj")
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(gdalcubes)
  library(rstac)
  library(sf)
  library(stars)
  library(tidyr)
})

message("Started: ", Sys.time())

# -----------------------------
# Sentinel-2 search parameters
# -----------------------------

date_start <- "2020-07-08"
date_end   <- "2020-07-14"

date_interval <- paste0(
  c(date_start, date_end),
  "T00:00:00Z",
  collapse = "/"
)

# Proof-of-concept study area
bbox <- c(
  xmin = -122,
  ymin = 39,
  xmax = -120,
  ymax = 41
)

# -----------------------------
# Retrieve Sentinel-2 items
# -----------------------------

message("Connecting to Earth Search...")

mystac <- stac(
  "https://earth-search.aws.element84.com/v1"
)

items <-
  mystac %>%
  stac_search(
    collections = "sentinel-2-l2a",
    bbox = c(
      bbox[["xmin"]],
      bbox[["ymin"]],
      bbox[["xmax"]],
      bbox[["ymax"]]
    ),
    datetime = date_interval
  ) %>%
  post_request() %>%
  items_fetch(progress = FALSE)

items <- items_filter(
  items,
  properties[["eo:cloud_cover"]] < 20
)

message(
  "Sentinel items after cloud filter: ",
  items_length(items)
)

# Keep TIFF assets, not duplicate JP2 assets
asset_names <- names(items$features[[1]]$assets)
asset_names <- asset_names[!grepl("jp2", asset_names)]

items <- assets_select(
  items,
  asset_names = asset_names
)

# -----------------------------
# Construct 10 m cube
# -----------------------------

message("Creating gdalcubes image collection...")

imgs <- stac_image_collection(
  items$features
)

# Transform study extent into UTM Zone 10N
bbox_utm <-
  st_bbox(
    bbox,
    crs = st_crs("EPSG:4326")
  ) %>%
  st_as_sfc() %>%
  st_transform("EPSG:32610") %>%
  st_bbox()

v <- cube_view(
  srs = "EPSG:32610",
  dx = 10,
  dy = 10,
  dt = "P1D",
  aggregation = "mean",
  resampling = "near",
  extent = list(
    left   = bbox_utm["xmin"],
    right  = bbox_utm["xmax"],
    bottom = bbox_utm["ymin"],
    top    = bbox_utm["ymax"],
    t0     = date_start,
    t1     = date_end
  )
)

message("Cube view:")
print(v)

bands <- c(
  "coastal",
  "blue",
  "green",
  "red",
  "rededge1",
  "rededge2",
  "rededge3",
  "nir",
  "nir08",
  "nir09",
  "swir16",
  "swir22"
)

message("Building and temporally averaging cube: ", Sys.time())

cube <-
  imgs %>%
  raster_cube(v) %>%
  select_bands(bands) %>%
  reduce_time(
    paste0("mean(", bands, ")")
  ) %>%
  st_as_stars()

message("Cube materialized: ", Sys.time())

# -----------------------------
# Convert to pixel table
# -----------------------------

message("Converting cube to data frame...")

cube_df <- as.data.frame(cube)

message(
  "Rows before removing NA values: ",
  format(nrow(cube_df), big.mark = ",")
)

cube_df <-
  cube_df %>%
  drop_na() %>%
  select(-any_of("time"))

message(
  "Rows after removing NA values: ",
  format(nrow(cube_df), big.mark = ",")
)

# -----------------------------
# Save
# -----------------------------

dir.create(
  "data",
  showWarnings = FALSE,
  recursive = TRUE
)

# Remove the old coarse-resolution version so that a failed
# run cannot be mistaken for a successful new result.
if (file.exists("data/cube_df.rds")) {
  unlink("data/cube_df.rds")
}

message("Saving data/cube_df.rds: ", Sys.time())

saveRDS(
  cube_df,
  "data/cube_df.rds"
)

message("Finished successfully: ", Sys.time())
message(
  "Output size: ",
  round(file.info("data/cube_df.rds")$size / 1024^3, 2),
  " GiB"
)