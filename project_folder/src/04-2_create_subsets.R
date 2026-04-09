# ================================= Notes =================================
#
# Splits each prediction chunk into 128 x 128 px PNG tiles and saves a
# spatial reference raster (target.tif) for reassembly.
# Chunks with an existing target.tif are skipped (safe to interrupt and resume).
#
# INPUT_LAYER_NAME: Must match the layer used in script 04-1
#
# Input:  data/modelling/prediction/<INPUT_LAYER_NAME>/chunks/dem_tile_<row>_<col>.tif
# Output: data/modelling/prediction/<INPUT_LAYER_NAME>/prediction_dem_tiles/<chunk>/<n>.png
#         data/modelling/prediction/<INPUT_LAYER_NAME>/prediction_dem_tiles/<chunk>/target.tif
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

# ================================= Parameters =================================

INPUT_LAYER_NAME <- "DEM_SL_SVF_OP" 

model_input_shape <- c(128, 128)

# ================================= Layer-specific directory structure =================================
layer_prediction_root <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME)
chunks_dir <- file.path(layer_prediction_root, "chunks")

# Verify that chunks directory exists
if (!dir.exists(chunks_dir)) {
  stop("Chunks directory not found: ", chunks_dir,
       "\nPlease run script 04-1 with INPUT_LAYER_NAME = '", 
       INPUT_LAYER_NAME, "' first.")
}

# Root output folder for all chunk tile subfolders
tiles_root <- file.path(layer_prediction_root, "dem_tiles")
if (!dir.exists(tiles_root)) {
  dir.create(tiles_root, recursive = TRUE)
}

# ================================= Collect chunks =================================
chunk_files <- list.files(
  chunks_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

if (length(chunk_files) == 0) {
  stop("No .tif chunk files found in: ", chunks_dir,
       "\nPlease run script 04-1 first.")
}

message("Found ", length(chunk_files), " chunk(s) to process.")

# ================================= Process each chunk =================================
for (chunk_path in chunk_files) {
  
  # Derive a clean subfolder name from the chunk filename
  chunk_name <- tools::file_path_sans_ext(basename(chunk_path))
  chunk_out_dir <- file.path(tiles_root, chunk_name)
  
  # Skip logic: treat presence of target.tif as a completion marker
  target_file <- file.path(chunk_out_dir, "target.tif")
  
  if (file.exists(target_file)) {
    message("  [SKIP] ", chunk_name, " (target.tif already exists)")
    next
  }
  
  message("  [PROC] ", chunk_name, " ...")
  
  # Create subfolder
  if (!dir.exists(chunk_out_dir)) {
    dir.create(chunk_out_dir, recursive = TRUE)
  }
  
  # Load chunk raster
  chunk_rst <- terra::rast(chunk_path)
  
  # Generate 128 x 128 tiles with normalization
  target_rst <- subset_ds(
    input_raster   = chunk_rst,
    model_input_shape = model_input_shape,
    path           = chunk_out_dir,
    targetname     = "",
    mask           = FALSE,
    normalize      = TRUE
  )
  
  # Save the cropped reference raster (needed for spatial reassembly)
  terra::writeRaster(
    target_rst,
    target_file,
    overwrite = TRUE
  )
  
  n_tiles <- length(list.files(chunk_out_dir, pattern = "\\.png$"))
  message("    -> ", n_tiles, " tile(s) written to: ", chunk_out_dir)
  
  # Free memory before next iteration
  rm(chunk_rst, target_rst)
  gc()
}






# Direkter Weg:
tiles_root <- file.path(envrmt$path_prediction, "DEM_SL_SVF_OP", "prediction_dem_tiles")

total <- length(list.files(tiles_root, pattern = "\\.png$", recursive = TRUE))

cat("Gesamtzahl Tiles:", total, "\n")