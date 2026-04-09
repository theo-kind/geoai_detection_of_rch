# ================================= Notes =================================
#
# This script performs the following steps:
# 1. Loads the final prediction raster from script 04-3.
# 2. Optionally clips to a CLC layer or other mask.
# 3. Binarizes the prediction (threshold > 0.5).
# 4. Vectorizes the binary raster into polygons.
# 5. Converts polygons to centroids.
# 6. Saves all outputs as GeoPackages.
#
# Output:
#   data/modelling/prediction/<INPUT_LAYER_NAME>/vectors/
#
# ================================= Setup =================================
library(envimaR)

rootDir <- paste0(getwd(), "/project_folder")

rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)

path <- file.path(rootDir, "src", "00_geoAI_setup.R")
source(path, echo = FALSE)

# ================================= Parameters =================================

INPUT_LAYER_NAME <- "DEM_SL_SVF_OP" 
MODEL_ID <- "DEM_SL_SVF_OP_20_5_F"

# Optional: Set to NULL to skip CLC clipping, or provide MASK_FILE path
USE_MASK <- TRUE
MASK_FILE <- "clc_2018_sel_311-312-313-324_clipped.gpkg"
MASK_FOLDER <- envrmt$path_corine

# Binarization threshold
THRESHOLD <- 0.5

# ================================= Setup Output Directory =================================

output_base <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME, "vectors")
if (!dir.exists(output_base)) {
  dir.create(output_base, recursive = TRUE)
}

# ================================= Load Final Prediction Raster =================================

result_file <- file.path(
  envrmt$path_prediction,
  INPUT_LAYER_NAME,
  "final_mosaic.tif"
)

if (!file.exists(result_file)) {
  stop("Final mosaic raster not found: ", result_file,
       "\nPlease run script 04-3 first.")
}

final_rst <- terra::rast(result_file)

# ================================= Optional: Clip to Mask =================================

if (USE_MASK && !is.null(MASK_FILE) && !is.null(MASK_FOLDER)) {
  
  mask_path <- file.path(MASK_FOLDER, MASK_FILE)
  
  if (!file.exists(mask_path)) {
    stop("Mask file not found: ", mask_path)
  }
  
  aoi <- sf::st_read(mask_path, quiet = TRUE)
  
  # Reproject mask to raster CRS if necessary
  if (!sf::st_crs(aoi) == sf::st_crs(terra::crs(final_rst))) {
    aoi <- sf::st_transform(aoi, terra::crs(final_rst))
  }
  
  clipped_rst <- terra::mask(
    terra::crop(final_rst, terra::vect(aoi)),
    terra::vect(aoi)
  )
  
} else {
  
  # Load AOI for reference
  aoi <- sf::st_read(
    file.path(envrmt$path_aoi, "harz_boundary.shp"),
    quiet = TRUE
  )
  
  # Reproject AOI to raster CRS if necessary
  if (!sf::st_crs(aoi) == sf::st_crs(terra::crs(final_rst))) {
    aoi <- sf::st_transform(aoi, terra::crs(final_rst))
  }
  
  clipped_rst <- final_rst
}

# ================================= Binarize Prediction =================================

binary_rst <- terra::classify(
  clipped_rst,
  matrix(c(-Inf, THRESHOLD, NA,
           THRESHOLD, Inf, 1), ncol = 3, byrow = TRUE)
)

# ================================= Vectorize to Polygons =================================

pred_vector <- binary_rst |>
  terra::as.polygons(dissolve = TRUE) |>
  sf::st_as_sf() |>
  sf::st_cast("POLYGON") |>
  terra::vect()

# ================================= Save Polygon Vector =================================

vector_file <- file.path(
  output_base,
  "prediction_polygons.gpkg"
)

terra::writeVector(
  pred_vector,
  vector_file,
  filetype = "GPKG",
  overwrite = TRUE
)

# ================================= Convert to Centroids =================================

pred_centroids <- terra::centroids(pred_vector)

centroids_file <- file.path(
  output_base,
  "prediction_centroids.gpkg"
)

terra::writeVector(
  pred_centroids,
  centroids_file,
  filetype = "GPKG",
  overwrite = TRUE
)

# ================================= Save Clipped Raster (Optional) =================================

raster_file <- file.path(
  output_base,
  "prediction_raster_clipped.tif"
)

terra::writeRaster(
  clipped_rst,
  raster_file,
  overwrite = TRUE
)

# ================================= Save Processing Metadata =================================

metadata <- data.frame(
  timestamp = Sys.time(),
  model_id = MODEL_ID,
  input_layer = INPUT_LAYER_NAME,
  threshold = THRESHOLD,
  mask_applied = USE_MASK,
  mask_file = if (USE_MASK) MASK_FILE else NA,
  n_polygons = nrow(pred_vector),
  n_centroids = nrow(pred_centroids)
)

metadata_file <- file.path(
  output_base,
  "processing_metadata.csv"
)

write.csv(metadata, metadata_file, row.names = FALSE)