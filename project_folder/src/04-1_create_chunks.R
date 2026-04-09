# ================================= Notes =================================
#
# Tiles the 4-band DEM derivative raster into 5120 x 5120 m chunks for
# large-area model inference. Tile size is a multiple of 128 px (model
# input size). Tiles containing only NA values are skipped.
#
# INPUT_LAYER_NAME: Specify which layer combination to use
#   Options: "DEM_HS_SL_SVF", "HS_SL_SVF_OP", "VAT_DEM_HS_SL", etc.
#
# Input:  data/dem1/dem_harz_<INPUT_LAYER_NAME>.tif
# Output: data/modelling/prediction/<INPUT_LAYER_NAME>/chunks/dem_tile_<row>_<col>.tif
#
# ================================= Set up =================================
library(envimaR)

# Define the project root folder
rootDir <- paste0(getwd(),"/project_folder")

# Get the root folder actually used
rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)

# MANDATORY: calling the setup script
path <- file.path(rootDir, "src", "00_geoAI_setup.R")
source(path, echo = TRUE)

# ================================= Parameters =================================
# Options: "DEM_HS_SL_SVF", "HS_SL_SVF_OP", "VAT_DEM_HS_SL", "VAT4b", etc.

INPUT_LAYER_NAME <- "DEM_SL_SVF_OP"

# ================================= Create layer-specific directory structure =================================
# Create a dedicated subfolder for each input layer
layer_prediction_root <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME)
if (!dir.exists(layer_prediction_root)) {
  dir.create(layer_prediction_root, recursive = TRUE)
}

# ================================= create tiles of dem-derivative =================================
# Load DEM-derivative multiband-raster
dem_filepath <- file.path(envrmt$path_dem1, paste0("dem_harz_", INPUT_LAYER_NAME, ".tif"))

if (!file.exists(dem_filepath)) {
  stop("DEM file not found: ", dem_filepath, 
       "\nMake sure the input layer '", INPUT_LAYER_NAME, "' exists.")
}

dem <- rast(dem_filepath)

# Define tile size in meters (factor of 128 to ensure compatibility with model input shape)
tile_size <- 5120

# Get DEM extent
dem_extent <- ext(dem)
xmin <- dem_extent$xmin
xmax <- dem_extent$xmax
ymin <- dem_extent$ymin
ymax <- dem_extent$ymax

# Calculate number of tiles
n_tiles_x <- ceiling((xmax - xmin) / tile_size)
n_tiles_y <- ceiling((ymax - ymin) / tile_size)

# Create output directory for chunks
chunks_output_dir <- file.path(layer_prediction_root, "chunks")
if (!dir.exists(chunks_output_dir)) {
  dir.create(chunks_output_dir, recursive = TRUE)
}

# Generate tiles
tile_list <- list()
counter <- 1

for (i in 1:n_tiles_x) {
  for (j in 1:n_tiles_y) {
    x_start <- xmin + (i - 1) * tile_size
    x_end <- min(x_start + tile_size, xmax)
    y_start <- ymin + (j - 1) * tile_size
    y_end <- min(y_start + tile_size, ymax)
    
    tile_extent <- ext(x_start, x_end, y_start, y_end)
    tile <- crop(dem, tile_extent)
    
    if (!all(is.na(values(tile)))) {
      filename <- file.path(
        chunks_output_dir, 
        sprintf("dem_tile_%02d_%02d.tif", i, j)
      )
      
      writeRaster(tile, filename, overwrite = TRUE)
      
      tile_list[[counter]] <- data.frame(
        tile_id = counter,
        tile_row = i,
        tile_col = j,
        xmin = x_start,
        xmax = x_end,
        ymin = y_start,
        ymax = y_end,
        filename = filename,
        n_cells = length(na.omit(values(tile)))
      )
      
      counter <- counter + 1
    }
  }
}