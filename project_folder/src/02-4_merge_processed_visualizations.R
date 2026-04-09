# ================================= Notes =================================
#
# Mosaics the QGIS-processed derivative tiles and assembles 4-band
# GeoTIFFs as model input composites.
#
# Input:  data/dem1/dem_harz.tif
#         data/dem1/dem_harz_hillshade.tif
#         data/dem1/dem_harz_slope.tif
#         data/dem1/dem_tiles_5km/svf/
#         data/dem1/dem_tiles_5km/open_pos/
#         data/dem1/dem_tiles_5km/vat_3b/
#         data/dem1/dem_tiles_5km/vat_4b/
# Output: data/dem1/dem_harz_svf.tif
#         data/dem1/dem_harz_open_pos.tif
#         data/dem1/dem_harz_vat_4b.tif
#         data/dem1/dem_harz_DEM_HS_SL_SVF.tif     (DEM | HS | Slope | SVF)
#         data/dem1/dem_harz_HS_SL_SVF_OP.tif      (HS | Slope | SVF | OpenPos)
#         data/dem1/dem_harz_VAT_DEM_HS_SL.tif     (VAT | DEM | HS | Slope)
#         data/dem1/dem_harz_DEM_SL_SVF_OP.tif     (DEM | SL | SVF| OpenPos)
#
# ================================= Set up =================================
library(envimaR)
library(parallel)

rootDir <- paste0(getwd(),"/project_folder")

rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)

path <- file.path(rootDir, "src", "00_geoAI_setup.R")
source(path, echo = FALSE)

# ================================= Merge tiles =================================
paths_to_merge <- list(
  open_pos = envrmt$path_open_pos,
  svf = envrmt$path_svf,
  vat_4b = envrmt$path_vat_4b
)

for (layer_name in names(paths_to_merge)) {
  input_path <- paths_to_merge[[layer_name]]
  
  tif_files <- list.files(
    input_path,
    pattern = "\\.tif$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(tif_files) == 0) {
    warning(paste("No TIF files found in", layer_name))
    next
  }
  
  tiles <- list()
  for (i in seq_along(tif_files)) {
    tryCatch({
      tiles[[i]] <- terra::rast(tif_files[i])
    }, error = function(e) {
      NULL
    })
  }
  
  tiles <- Filter(Negate(is.null), tiles)
  
  if (length(tiles) == 0) {
    warning(paste("No tiles successfully loaded for", layer_name))
    next
  }
  
  output_path <- file.path(envrmt$path_dem1, paste0("dem_harz_", layer_name, ".tif"))
  
  if (file.exists(output_path)) {
    next
  }
  
  merged <- do.call(terra::mosaic, c(tiles, fun = mean))
  terra::writeRaster(merged, output_path, overwrite = FALSE)
}



# ================================= Merge tif-layers in multiple banded tif =================================

# load layer
files <- c(
  b1 = file.path(envrmt$path_dem1, "dem_harz.tif"),
  b2 = file.path(envrmt$path_dem1, "dem_harz_hillshade.tif"),
  b3 = file.path(envrmt$path_dem1, "dem_harz_slope.tif"),
  b4 = file.path(envrmt$path_dem1, "dem_harz_svf.tif"),
  b5 = file.path(envrmt$path_dem1, "dem_harz_open_pos.tif"),
  b6 = file.path(envrmt$path_dem1, "dem_harz_VAT4b.tif")
)


# create "dem_harz_DEM_HS_SL_SVF.tif":
r_4 <- rast(files[c("b1", "b2", "b3", "b4")])

# define output path
r_4_out_file <- file.path(
  envrmt$path_dem1,
  "dem_harz_DEM_HS_SL_SVF.tif"
)

# write multi-band GeoTIFF
writeRaster(
  r_4,
  r_4_out_file,
  overwrite = TRUE
)


# create dem_harz_HS_SL_SVF_OP.tif:
r_4<- rast(files[c("b2", "b3", "b4", "b5")])

# define output path
r_4_out_file <- file.path(
  envrmt$path_dem1,
  "dem_harz_HS_SL_SVF_OP.tif"
)

# write multi-band GeoTIFF
writeRaster(
  r_4,
  r_4_out_file,
  overwrite = TRUE
)


# create dem_harz_VAT_DEM_HS_SL.tif:
r_4<- rast(files[c("b6", "b1", "b2", "b3")])

# define output path
r_4_out_file <- file.path(
  envrmt$path_dem1,
  "dem_harz_VAT_DEM_HS_SL.tif"
)

# write multi-band GeoTIFF
writeRaster(
  r_4,
  r_4_out_file,
  overwrite = TRUE
)


# create dem_harz_VAT_DEM_HS_SL.tif:
r_4<- rast(files[c("b1", "b3", "b4", "b5")])

# define output path
r_4_out_file <- file.path(
  envrmt$path_dem1,
  "dem_harz_DEM_SL_SVF_OP.tif"
)

# write multi-band GeoTIFF
writeRaster(
  r_4,
  r_4_out_file,
  overwrite = TRUE
)

