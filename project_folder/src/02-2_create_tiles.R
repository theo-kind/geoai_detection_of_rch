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

# ================================= create tiles of dem =================================
# Load DEM
dgm <- rast(file.path(envrmt$path_dgm1, "dem_harz.tif"))

# Define tile size in meters (10 x 10 km)
tile_size <- 10000

# Get DEM extent
dgm_extent <- ext(dgm)
xmin <- dgm_extent$xmin
xmax <- dgm_extent$xmax
ymin <- dgm_extent$ymin
ymax <- dgm_extent$ymax

# Calculate number of tiles
n_tiles_x <- ceiling((xmax - xmin) / tile_size)
n_tiles_y <- ceiling((ymax - ymin) / tile_size)

# Create output directory
tiles_output_dir <- file.path(envrmt$path_dgm_tiles)
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
    tile <- crop(dgm, tile_extent)
    
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

# ================================= metadata csv =================================
# Convert list to data frame
tiles_info <- do.call(rbind, tile_list)
rownames(tiles_info) <- NULL

# Save metadata
metadata_file <- file.path(tiles_output_dir, "tiles_metadata.csv")
write.csv(tiles_info, metadata_file, row.names = FALSE)

cat("Created", nrow(tiles_info), "tiles in", tiles_output_dir, "\n")