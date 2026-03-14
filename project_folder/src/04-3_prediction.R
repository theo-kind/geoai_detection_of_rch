# ================================= Notes =================================
#
# Runs full-AOI inference, reassembles georeferenced mosaics, and derives
# vector outputs. Set MODEL_ID to the best run from experiment_results.csv.
# Chunks with an existing mosaic.tif are skipped (safe to interrupt and resume).
#
# Input:  data/modelling/models/unet_<MODEL_ID>.hdf5
#         data/modelling/prediction/prediction_dem_tiles/<chunk>/
#         data/aoi/harz_boundary.shp
# Output: data/modelling/prediction/<MODEL_ID>/<chunk>/mosaic.tif
#         data/modelling/prediction/<MODEL_ID>/final_mosaic.vrt
#         data/modelling/prediction/<MODEL_ID>/final_mosaic.tif
#         results/prediction_<MODEL_ID>_clipped.tif
#         results/prediction_<MODEL_ID>_clipped.gpkg
#         results/prediction_<MODEL_ID>_centroids.gpkg
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
MODEL_ID   <- "DEM_HS_SL_SVF_25_2_F"
batch_size <- 8

# Root folder containing one subfolder per chunk (output of script 04-2)
tiles_root <- file.path(envrmt$path_prediction, "prediction_dem_tiles")

# Output root: path_prediction/<MODEL_ID>/<chunk_name>/mosaic.tif
model_out_root <- file.path(envrmt$path_prediction, MODEL_ID)
if (!dir.exists(model_out_root)) {
  dir.create(model_out_root, recursive = TRUE)
}

# ================================= Load model =================================
unet_model <- keras::load_model_hdf5(
  file.path(envrmt$path_models, paste0("unet_", MODEL_ID, ".hdf5")),
  compile = TRUE
)

# ================================= Collect chunk subfolders =================================
chunk_dirs <- list.dirs(tiles_root, full.names = TRUE, recursive = FALSE)

if (length(chunk_dirs) == 0) {
  stop("No chunk subfolders found in: ", tiles_root,
       "\nPlease run script 04-2 first.")
}

message("Found ", length(chunk_dirs), " chunk(s) to predict.")

# ================================= Predict per chunk =================================
for (chunk_dir in chunk_dirs) {
  
  chunk_name <- basename(chunk_dir)
  
  # ------------------------------------------------------------------
  # Skip logic: mosaic.tif inside the chunk output folder marks a
  # successfully completed chunk. Safe to interrupt and resume.
  # ------------------------------------------------------------------
  chunk_out_dir <- file.path(model_out_root, chunk_name)
  mosaic_file   <- file.path(chunk_out_dir, "mosaic.tif")
  
  if (file.exists(mosaic_file)) {
    message("  [SKIP] ", chunk_name, " (mosaic.tif already exists)")
    next
  }
  
  # Check that this chunk was fully prepared (target.tif is the
  # completion marker written last by script 04-2)
  target_file <- file.path(chunk_dir, "target.tif")
  if (!file.exists(target_file)) {
    warning("  [WARN] Skipping ", chunk_name,
            ": target.tif not found – chunk may be incomplete.")
    next
  }
  
  message("  [PRED] ", chunk_name, " ...")
  
  # ------------------------------------------------------------------
  # Sanity check: make sure tiles actually exist in the chunk folder
  # ------------------------------------------------------------------
  png_files <- list.files(chunk_dir, pattern = "\\.png$")
  
  if (length(png_files) == 0) {
    warning("  [WARN] No PNG tiles found in ", chunk_dir, " – skipping.")
    next
  }
  
  # ------------------------------------------------------------------
  # Build prediction dataset and run inference.
  # prepare_ds() handles numeric sorting of tiles internally when
  # predict = TRUE, so we only pass the directory path via subsets_path.
  # ------------------------------------------------------------------
  prediction_dataset <- prepare_ds(
    train        = FALSE,
    predict      = TRUE,
    subsets_path = chunk_dir,
    batch_size   = batch_size
  )
  
  pred_subsets <- predict(object = unet_model, x = prediction_dataset)
  
  # ------------------------------------------------------------------
  # Rebuild georeferenced raster from tile predictions.
  # rebuild_img() saves its output to file.path(out_path, model_name)/,
  # so we pass model_out_root as out_path and chunk_name as model_name
  # -> chunk_out_dir = path_prediction/<MODEL_ID>/<chunk_name>/
  # ------------------------------------------------------------------
  target_rst <- terra::rast(target_file)
  
  rebuild_img(
    pred_subsets = pred_subsets,
    out_path     = model_out_root,
    target_rst   = target_rst,
    model_name   = chunk_name     
  )
  
  message("    -> Saved: ", mosaic_file)
  
  # Free memory before next chunk
  rm(pred_subsets, prediction_dataset, target_rst)
  gc()
}

# ================================= Merge all chunk mosaics =================================
message("\nMerging chunk mosaics into final prediction raster ...")

chunk_mosaics <- list.files(
  model_out_root,
  pattern     = "mosaic\\.tif$",
  full.names  = TRUE,
  recursive   = TRUE   # one level deep into chunk subfolders
)

if (length(chunk_mosaics) == 0) {
  stop("No chunk mosaics found in: ", model_out_root)
}

message("  Found ", length(chunk_mosaics), " chunk mosaic(s).")

final_vrt <- file.path(model_out_root, "final_mosaic.vrt")
final_tif <- file.path(model_out_root, "final_mosaic.tif")

sf::gdal_utils(
  util        = "buildvrt",
  source      = chunk_mosaics,
  destination = final_vrt
)

sf::gdal_utils(
  util        = "warp",
  source      = final_vrt,
  destination = final_tif
)

message("Done.")
message("Final prediction raster: ", final_tif)



# ================================= Clip to AOI =================================

MODEL_ID   <- "DEM_HS_SL_SVF_25_2_F"

if (!exists("final_tif")) {
  final_tif <- file.path(envrmt$path_prediction, MODEL_ID, "final_mosaic.tif")
}
if (!file.exists(final_tif)) {
  stop("final_mosaic.tif not found: ", final_tif,
       "\nPlease run the prediction loop first (or check MODEL_ID).")
}

aoi <- sf::st_read(
  file.path(envrmt$path_aoi, "harz_boundary.shp"),
  quiet = TRUE
)

final_rst <- terra::rast(final_tif)

# Reproject AOI to raster CRS if necessary
if (!sf::st_crs(aoi) == sf::st_crs(terra::crs(final_rst))) {
  aoi <- sf::st_transform(aoi, terra::crs(final_rst))
}

clipped_rst <- terra::mask(
  terra::crop(final_rst, terra::vect(aoi)),
  terra::vect(aoi)
)

result_file <- file.path(
  envrmt$path_results,
  paste0("prediction_", MODEL_ID, "_clipped.tif")
)

terra::writeRaster(
  clipped_rst,
  result_file,
  overwrite = TRUE
)

message("Clipped prediction saved to: ", result_file)



# ================================= Vectorize prediction =================================

MODEL_ID   <- "DEM_HS_SL_SVF_25_2_F"

if (!exists("clipped_rst")) {
  result_file <- file.path(
    envrmt$path_results,
    paste0("prediction_", MODEL_ID, "_clipped.tif")
  )
  if (!file.exists(result_file)) {
    stop("Clipped raster not found: ", result_file,
         "\nPlease run the clipping section first.")
  }
  clipped_rst <- terra::rast(result_file)
}

# Binarize: pixels > 0.5 become 1, everything else NA
binary_rst <- terra::classify(
  clipped_rst,
  matrix(c(-Inf, 0.5, NA,
           0.5,  Inf,  1), ncol = 3, byrow = TRUE)
)

  terra::as.polygons(dissolve = TRUE) |>
  sf::st_as_sf() |>
  sf::st_cast("POLYGON") |>
  terra::vect()

vector_file <- file.path(
  envrmt$path_results,
  paste0("prediction_", MODEL_ID, "_clipped.gpkg")
)

terra::writeVector(
  pred_vector,
  vector_file,
  filetype = "GPKG",
  overwrite = TRUE
)

message("Vectorized prediction saved to: ", vector_file)

# ================================= Polygons to centroids =================================

pred_centroids <- terra::centroids(pred_vector)

centroids_file <- file.path(
  envrmt$path_results,
  paste0("prediction_", MODEL_ID, "_centroids.gpkg")
)

terra::writeVector(
  pred_centroids,
  centroids_file,
  filetype = "GPKG",
  overwrite = TRUE
)

message("Centroids saved to: ", centroids_file)