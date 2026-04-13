# ================================= Notes =================================
#
# Splits the merged Harz DEM into 5 km x 5 km tiles for visual inspection
# and secondary processing. Tiles containing only NA values are skipped.
#
# Input:  data/dem1/dem_harz.tif
# Output: data/dem1/dem_tiles_5km/dem_tile_<row>_<col>.tif 
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

# ================================= create chunks of dem =================================
# Load DEM
dem <- rast(file.path(envrmt$path_dem1, "dem_harz.tif"))

# Define tile size in meters 
tile_size <- 5000

# Get DEM extent
dem_extent <- ext(dem)
xmin <- dem_extent$xmin
xmax <- dem_extent$xmax
ymin <- dem_extent$ymin
ymax <- dem_extent$ymax

# Calculate number of tiles
n_tiles_x <- ceiling((xmax - xmin) / tile_size)
n_tiles_y <- ceiling((ymax - ymin) / tile_size)

# Create output directory
tiles_output_dir <- file.path(envrmt$path_dem_tiles_5km)
if (!dir.exists(tiles_output_dir)) {
  dir.create(tiles_output_dir, recursive = TRUE)
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
        tiles_output_dir, 
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
