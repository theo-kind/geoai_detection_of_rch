# ================================= Notes =================================
#
# Computes hillshade and slope from the merged DEM using SAGA GIS.
# Positive Openness, Sky-View Factor, and VAT are calculated separately
# in QGIS (Raster Visualization Toolbox) and saved to the tile folders.
#
# Note: Adjust saga_cmd path for your OS (macOS / Windows, see inline).
#
# Input:  data/dem1/dem_harz.tif
# Output: data/dem1/dem_harz_hillshade.tif          (this script, SAGA)
#         data/dem1/dem_harz_slope.tif              (this script, SAGA)
#         data/dem1/dem_tiles_5km/svf/              (QGIS, manual)
#         data/dem1/dem_tiles_5km/open_pos/         (QGIS, manual)
#         data/dem1/dem_tiles_5km/vat_4b/           (QGIS, manual)
#
#
# ================================= Set up ================================= 
library(envimaR) 

rootDir <- paste0(getwd(),"/project_folder") 

rootDir <- envimaR::alternativeEnvi( 
  root_folder = rootDir, 
  alt_env_id = "COMPUTERNAME", 
  alt_env_value = "PCRZP", 
  alt_env_root_folder = "F:/BEN/edu" 
) 

path <- file.path(rootDir, "src", "00_geoAI_setup.R") 
source(path, echo = FALSE) 


# ======================== Load DEM data ======================== 
dem_path <- file.path(envrmt$path_dem1, "dem_harz.tif")
dem <- terra::rast(dem_path)

# ======================== SAGA Path ======================== 
saga_cmd <- "/Applications/SAGA.app/Contents/MacOS/saga_cmd" # for MacOS
# saga_cmd <- "C:/Program Files (x86)/SAGA-GIS/saga_cmd.exe" # for Windows

# ======================== Calculate Hillshade (SAGA) ======================== 
hillshade_output <- file.path(envrmt$path_dem1, "dem_harz_hillshade.tif")
system(paste(
  saga_cmd,
  "ta_lighting 0",
  "-ELEVATION", dem_path,
  "-SHADE", hillshade_output,
  "-AZIMUTH 315",
  "-DECLINATION 35",
  "-EXAGGERATION 2.0",
  "-METHOD 6"
))
hillshade <- terra::rast(hillshade_output)

# ======================== Calculate Slope (SAGA) ========================
slope_output <- file.path(envrmt$path_dem1, "dem_harz_slope.tif")
system(paste(
  saga_cmd, 
  "ta_morphometry 0", 
  "-ELEVATION", dem_path, 
  "-SLOPE", slope_output, 
  "-METHOD 4" 
))
slope_saga <- terra::rast(slope_output)