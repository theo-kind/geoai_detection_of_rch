# ================================= Notes =================================
#
# This script performs full-AOI inference and reassembles georeferenced mosaics.
# Set INPUT_LAYER_NAME and MODEL_ID to the best run from experiment_results.csv.
# Chunks with existing mosaic.tif are skipped (safe to interrupt and resume).
#
# Input:  data/modelling/models/unet_<MODEL_ID>.hdf5
#         data/modelling/prediction/<INPUT_LAYER_NAME>/dem_tiles/<chunk>/
#         data/aoi/harz_boundary.shp
#
# Output folder structure:
#   data/modelling/prediction/<INPUT_LAYER_NAME>/
#         ├── prediction_tiles/<MODEL_ID>/<chunk>/mosaic.tif       (intermediate)
#         ├── vectors/prediction_*.gpkg                   (polygons, centroids)
#         ├── final_mosaic.tif                           (full prediction)
#         ├── final_mosaic_clipped.tif                   (clipped to mask)
#         └── processing_metadata.csv
#
# ================================= Set up =================================
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

Sys.setenv(GDAL_TMPDIR = envrmt$path_tmp)

# ================================= Parameters =================================

INPUT_LAYER_NAME <- "DEM_SL_SVF_OP"
MODEL_ID <- "DEM_SL_SVF_OP_20_5_F"
batch_size <- 8
THRESHOLD <- 0.5

# Optional: Clip to mask (set to FALSE to skip)
USE_MASK <- TRUE
MASK_FILE <- "clc_2018_sel_311-312-313-324_clipped.gpkg"

# ================================= Setup Directory Structure =================================

layer_prediction_root <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME)

# Verify prediction tiles directory exists
tiles_root <- file.path(layer_prediction_root, "dem_tiles")
if (!dir.exists(tiles_root)) {
  stop("Prediction tiles directory not found: ", tiles_root,
       "\nPlease run script 04-2 with INPUT_LAYER_NAME = '", 
       INPUT_LAYER_NAME, "' first.")
}

# Setup output directories
model_chunks_root <- file.path(layer_prediction_root, paste0("prediction_", MODEL_ID))
if (!dir.exists(model_chunks_root)) {
  dir.create(model_chunks_root, recursive = TRUE)
}

vectors_dir <- file.path(layer_prediction_root, "vectors")
if (!dir.exists(vectors_dir)) {
  dir.create(vectors_dir, recursive = TRUE)
}

# ================================= Load Model =================================

model_path <- file.path(envrmt$path_models, paste0("unet_", MODEL_ID, ".hdf5"))

if (!file.exists(model_path)) {
  stop("Model file not found: ", model_path,
       "\nPlease check your MODEL_ID.")
}

unet_model <- keras::load_model_hdf5(model_path, compile = TRUE)

# ================================= Collect Chunk Subfolders =================================

chunk_dirs <- list.dirs(tiles_root, full.names = TRUE, recursive = FALSE)

if (length(chunk_dirs) == 0) {
  stop("No chunk subfolders found in: ", tiles_root,
       "\nPlease run script 04-2 first.")
}

# ================================= Predict Per Chunk =================================

for (chunk_dir in chunk_dirs) {
  
  chunk_name <- basename(chunk_dir)
  chunk_out_dir <- file.path(model_chunks_root, chunk_name)
  mosaic_file <- file.path(chunk_out_dir, "mosaic.tif")
  
  # Skip if already processed
  if (file.exists(mosaic_file)) {
    next
  }
  
  # Verify chunk completeness
  target_file <- file.path(chunk_dir, "target.tif")
  if (!file.exists(target_file)) {
    warning("Skipping ", chunk_name, ": target.tif not found.")
    next
  }
  
  # Verify PNG tiles exist
  png_files <- list.files(chunk_dir, pattern = "\\.png$")
  if (length(png_files) == 0) {
    warning("No PNG tiles found in ", chunk_dir)
    next
  }
  
  # Build prediction dataset and run inference
  prediction_dataset <- prepare_ds(
    train = FALSE,
    predict = TRUE,
    subsets_path = chunk_dir,
    batch_size = batch_size
  )
  
  pred_subsets <- predict(object = unet_model, x = prediction_dataset)
  
  # Rebuild georeferenced raster from tile predictions
  target_rst <- terra::rast(target_file)
  
  rebuild_img(
    pred_subsets = pred_subsets,
    out_path = model_chunks_root,
    target_rst = target_rst,
    model_name = chunk_name
  )
  
  # Free memory before next chunk
  rm(pred_subsets, prediction_dataset, target_rst)
  gc()
}

# ================================= Merge All Chunk Mosaics =================================

chunk_mosaics <- list.files(
  model_chunks_root,
  pattern = "mosaic\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(chunk_mosaics) == 0) {
  stop("No chunk mosaics found in: ", model_chunks_root)
}

final_vrt <- file.path(layer_prediction_root, "final_mosaic.vrt")
final_tif <- file.path(layer_prediction_root, "final_mosaic.tif")

sf::gdal_utils(
  util = "buildvrt",
  source = chunk_mosaics,
  destination = final_vrt
)

sf::gdal_utils(
  util = "warp",
  source = final_vrt,
  destination = final_tif
)

# ================================= Load and Prepare Mask/AOI =================================

if (USE_MASK) {
  
  mask_path <- file.path(envrmt$path_corine, MASK_FILE)
  
  if (!file.exists(mask_path)) {
    stop("Mask file not found: ", mask_path)
  }
  
  aoi <- sf::st_read(mask_path, quiet = TRUE)
  
} else {
  
  aoi <- sf::st_read(
    file.path(envrmt$path_aoi, "harz_boundary.shp"),
    quiet = TRUE
  )
}

final_rst <- terra::rast(final_tif)

# Reproject to raster CRS if necessary
if (!sf::st_crs(aoi) == sf::st_crs(terra::crs(final_rst))) {
  aoi <- sf::st_transform(aoi, terra::crs(final_rst))
}

# Clip to mask
clipped_rst <- terra::mask(
  terra::crop(final_rst, terra::vect(aoi)),
  terra::vect(aoi)
)

clipped_file <- file.path(layer_prediction_root, "final_mosaic_clipped.tif")
terra::writeRaster(clipped_rst, clipped_file, overwrite = TRUE)

# ================================= Binarize and Vectorize =================================

# Binarize: pixels > threshold become 1, everything else NA
binary_rst <- terra::classify(
  clipped_rst,
  matrix(c(-Inf, THRESHOLD, NA,
           THRESHOLD, Inf, 1), ncol = 3, byrow = TRUE)
)

# Vectorize to polygons
pred_vector <- binary_rst |>
  terra::as.polygons(dissolve = TRUE) |>
  sf::st_as_sf() |>
  sf::st_cast("POLYGON") |>
  terra::vect()

# Save polygons
vector_file <- file.path(vectors_dir, "prediction_polygons.gpkg")
terra::writeVector(pred_vector, vector_file, filetype = "GPKG", overwrite = TRUE)

# Convert to centroids and save
pred_centroids <- terra::centroids(pred_vector)
centroids_file <- file.path(vectors_dir, "prediction_centroids.gpkg")
terra::writeVector(pred_centroids, centroids_file, filetype = "GPKG", overwrite = TRUE)

# ================================= Save Processing Metadata =================================

metadata <- data.frame(
  timestamp = Sys.time(),
  model_id = MODEL_ID,
  input_layer = INPUT_LAYER_NAME,
  threshold = THRESHOLD,
  mask_applied = USE_MASK,
  mask_file = if (USE_MASK) MASK_FILE else NA,
  n_polygons = nrow(pred_vector),
  n_centroids = nrow(pred_centroids),
  chunks_processed = length(chunk_mosaics)
)

metadata_file <- file.path(layer_prediction_root, "processing_metadata.csv")
write.csv(metadata, metadata_file, row.names = FALSE)