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

# ======================== Load data ========================
dgm_path <- file.path(envrmt$path_dgm1, "dem_harz.tif")
dgm <- terra::rast(dgm_path)
cat("DGM1 Information:\n")
print(dgm)

# ======================== Calculate Slope ========================
slope <- terra::terrain(dgm, v = "slope", unit = "degrees")

# ======================== Calculate Hillshade ========================
hillshade <- terra::shade(
  terra::terrain(dgm, v = "slope", unit = "radians"),
  terra::terrain(dgm, v = "aspect", unit = "radians"),
  angle = 45,
  direction = 315
)
# ======================== Save Results ========================
terra::writeRaster(slope,
                   file.path(envrmt$path_dgm1, "dem_harz_slope.tif"),
                   overwrite = TRUE)
terra::writeRaster(hillshade,
                   file.path(envrmt$path_dgm1, "dem_harz_hillshade.tif"),
                   overwrite = TRUE)

