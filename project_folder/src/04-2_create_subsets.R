# ================================= Notes =================================
#
# Splits each prediction chunk into 128 x 128 px PNG tiles and saves a
# spatial reference raster (target.tif) for reassembly.
# Chunks with an existing target.tif are skipped (safe to interrupt and resume).
#
# Input:  data/modelling/prediction/chunks/dem_tile_<row>_<col>.tif
# Output: data/modelling/prediction/prediction_dem_tiles/<chunk>/<n>.png
#         data/modelling/prediction/prediction_dem_tiles/<chunk>/target.tif
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
model_input_shape <- c(128, 128)

# Root output folder for all chunk tile subfolders
tiles_root <- file.path(envrmt$path_prediction, "prediction_dem_tiles")
if (!dir.exists(tiles_root)) {
  dir.create(tiles_root, recursive = TRUE)
}

# ================================= Collect chunks =================================
chunk_files <- list.files(
  envrmt$path_chunks,
  pattern = "\\.tif$",
  full.names = TRUE
)

if (length(chunk_files) == 0) {
  stop("No .tif chunk files found in: ", envrmt$path_chunks,
       "\nPlease run script 04-1 first.")
}

message("Found ", length(chunk_files), " chunk(s) to process.")

# ================================= Process each chunk =================================
for (chunk_path in chunk_files) {
  
  # Derive a clean subfolder name from the chunk filename, e.g.
  # "dem_tile_01_02.tif" -> "chunk_01_02"
  chunk_name <- tools::file_path_sans_ext(basename(chunk_path))
  chunk_out_dir <- file.path(tiles_root, chunk_name)
  
  # ------------------------------------------------------------------
  # Skip logic: treat presence of target.tif as a completion marker.
  # This allows safe interruption and resumption of the loop.
  # ------------------------------------------------------------------
  target_file <- file.path(chunk_out_dir, "target.tif")
  
  if (file.exists(target_file)) {
    message("  [SKIP] ", chunk_name, " (target.tif already exists)")
    next
  }
  
  message("  [PROC] ", chunk_name, " ...")
  
  # Create subfolder (may not exist yet, or may be incomplete from a
  # previous interrupted run – tiles will simply be overwritten)
  if (!dir.exists(chunk_out_dir)) {
    dir.create(chunk_out_dir, recursive = TRUE)
  }
  
  # Load chunk raster
  chunk_rst <- terra::rast(chunk_path)
  
  # ------------------------------------------------------------------
  # Generate 128 x 128 tiles.
  # targetname = "" produces purely numeric filenames: 1.png, 2.png, ...
  # This is required by rebuild_img() which expects sequential indexing.
  # subset_ds() returns the spatially cropped raster (excess pixels
  # trimmed to be a clean multiple of model_input_shape).
  # ------------------------------------------------------------------
  target_rst <- subset_ds(
    input_raster   = chunk_rst,
    model_input_shape = model_input_shape,
    path           = chunk_out_dir,
    targetname     = "",
    mask           = FALSE,
    normalize      = TRUE
  )
  
  # ------------------------------------------------------------------
  # Save the cropped reference raster.
  # rebuild_img() needs this to recover the correct spatial extent,
  # CRS, and pixel grid of the predictions.
  # Writing this file LAST makes it the reliable completion marker.
  # ------------------------------------------------------------------
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

message("\nDone. All chunks processed.")
message("Tile root: ", tiles_root)