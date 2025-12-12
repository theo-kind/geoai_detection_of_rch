# ======================== set environment ========================
library(envimaR)

# define the project root folder
rootDir <- paste0(getwd(),"/geoAI_proj") # this is the mandantory foldername of the whole project

# get the root folder actually used
rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,             # if it exists, this is the root folder
  alt_env_id = "COMPUTERNAME",       # check the environment varialbe "COMPUTERNAME"
  alt_env_value = "PCRZP",           # if it contains the string "PCRZP" (e.g. PUM-Pool-PC)
  alt_env_root_folder = "F:/BEN/edu" # use the alternative root folder
)

# MANDANTORY: calling the setup script
path <- file.path(rootDir, "src", "00_geoAI_setup.R")
source(path, echo = TRUE)

# ======================== DGM1 Analysis ========================
# Calculate Slope, Aspect and Hillshade from DGM1

# Load DGM
dgm_path <- file.path(envrmt$path_dgm1, "dgm1_merged.tif")
dgm <- terra::rast(dgm_path)

# Check loaded DGM
cat("DGM1 Information:\n")
print(dgm)

# ======================== Calculate Slope ========================
# Slope in degrees (0-90°)
slope <- terra::terrain(dgm, v = "slope", unit = "degrees")

# ======================== Calculate Aspect ========================
# Aspect in degrees (0-360°)
# 0/360° = North, 90° = East, 180° = South, 270° = West
aspect <- terra::terrain(dgm, v = "aspect", unit = "degrees")

# ======================== Calculate Hillshade ========================
# Hillshade with standard parameters (Azimuth=315°, Elevation=45°)
hillshade <- terra::shade(terra::terrain(dgm, v = "slope", unit = "radians"),
                          terra::terrain(dgm, v = "aspect", unit = "radians"),
                          angle = 45,
                          direction = 315)

# ======================== Save Results ========================
# Save the calculated rasters
terra::writeRaster(slope,
                   file.path(envrmt$path_dgm1, "dgm1_slope.tif"),
                   overwrite = TRUE)

terra::writeRaster(aspect,
                   file.path(envrmt$path_dgm1, "dgm1_aspect.tif"),
                   overwrite = TRUE)

terra::writeRaster(hillshade,
                   file.path(envrmt$path_dgm1, "dgm1_hillshade.tif"),
                   overwrite = TRUE)

# ======================== Visualization ========================
# Create a simple visualization
par(mfrow = c(2, 2))

plot(slope, main = "Slope", col = terrain.colors(100))
plot(aspect, main = "Aspect", col = hcl.colors(100, "spectral"))
plot(hillshade, main = "Hillshade", col = gray.colors(100))
plot(dgm, main = "Original DGM1")

par(mfrow = c(1, 1))