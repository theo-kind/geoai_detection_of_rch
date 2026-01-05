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

# Read AOI
aoi <- st_read(file.path(envrmt$path_aoi, "harz_boundary.shp"))

# ================================= Lower Saxony: DGM1 Download, Merge & Clip =================================
# Define URL
geojson_url <- "https://arcgis-geojson.s3.eu-de.cloud-object-storage.appdomain.cloud/dgm1/lgln-opengeodata-dgm1.geojson"

# Download and parse GeoJSON
response <- httr::GET(geojson_url)
httr::stop_for_status(response)
tiles_geojson <- jsonlite::fromJSON(httr::content(response, as = "text"))

# Convert to SF object for easier handling
tiles_sf <- st_as_sf(
  st_collection_extract(
    st_read(geojson_url),
    "POLYGON"
  )
)

# Reproject AOI to WGS84 to match tiles
aoi_wgs84 <- st_transform(aoi, st_crs(tiles_sf))

# Find intersecting tiles
intersecting_indices <- st_intersects(aoi_wgs84, tiles_sf)
tile_indices <- unique(unlist(intersecting_indices))

if (length(tile_indices) == 0) {
  stop("No intersecting tiles found. Check your AOI and tile extent.")
}

# Extract download URLs from dgm1 column
download_urls <- tiles_sf$dgm1[tile_indices]
download_urls <- download_urls[!is.na(download_urls)]

# Download tiles
dgm_tiles <- list()

for (i in seq_along(download_urls)) {
  url <- download_urls[i]
  filename <- basename(url)
  filepath <- file.path(envrmt$path_dgm1_ni, filename)
  
  tryCatch({
    download.file(url, filepath, mode = "wb", quiet = FALSE)
    dgm_tiles[[i]] <- rast(filepath)
  }, error = function(e) {
    NULL
  })
}

# Remove NULL entries from failed downloads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No tiles were successfully downloaded.")
}

# Merge tiles
dgm_merged <- do.call(terra::mosaic, c(dgm_tiles, list(fun = mean)))


# Reproject AOI to match DGM if necessary
if (st_crs(aoi)$proj4string != crs(dgm_merged)) {
  aoi_dgm_crs <- st_transform(aoi, crs(dgm_merged))
} else {
  aoi_dgm_crs <- aoi
}

# Convert AOI to vect for terra operations
aoi_vect <- vect(aoi_dgm_crs)

# Clip DGM to AOI
dgm_clipped <- crop(dgm_merged, aoi_vect, mask = TRUE)

# Save clipped DGM
output_path <- file.path(envrmt$path_raw, "dgm1_merged_ni.tif")
writeRaster(dgm_clipped, output_path, overwrite = TRUE)


# ================================= DGM1 Saxony-Anhalt Merge & Clip =================================
# (1) Manually download "teil 1" and "teil 2" from https://www.lvermgeo.sachsen-anhalt.de/de/gdp-dgm1-landesweit.html
# (2) Save both .zip files in the folder "data/raw/dgm1_st" 
# (3) Unzip the files

# Find all TIF files in path_dgm1_st
tif_files <- list.files(
  envrmt$path_dgm1_st,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(tif_files) == 0) {
  stop("No TIF files found in path_dgm1_st. Check the path and file extensions.")
}

# Transform AOI to match DGM CRS
temp_tile <- rast(tif_files[1])
aoi_dgm_crs <- st_transform(aoi, 25832)
aoi_vect <- vect(aoi_dgm_crs)

# Load and clip all TIF files
dgm_tiles <- list()

for (i in seq_along(tif_files)) {
  tryCatch({
    tile <- rast(tif_files[i])
    # Clip immediately after loading to reduce data size
    tile_clipped <- crop(tile, aoi_vect, mask = TRUE)
    
    if (!is.null(tile_clipped) && nlyr(tile_clipped) > 0) {
      dgm_tiles[[i]] <- tile_clipped
    }
  }, error = function(e) {
    NULL
  })
}

# Remove NULL entries from failed loads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No DGM tiles with overlap to AOI were found.")
}

# Merge all tiles
dgm_merged <- do.call(terra::mosaic, c(dgm_tiles, fun = mean))

# Save clipped DGM
output_path <- file.path(envrmt$path_raw, "dgm1_merged_st.tif")
writeRaster(dgm_merged, output_path, overwrite = TRUE)


# ================================= Thuringia: DGM1 Extract, Merge & Clip =================================
# (1) Manually download DGM1 data from: https://geoportal.thueringen.de/gdi-th/download-offene-geodaten/download-hoehendaten
# each file is approximitly 10 GB large
# (2) Save .zip file (e.g., "Mehrfachdownload_wKrrO6_UrBfm4.zip") in path_dgm1_th folder for further processing

unzipped <- file.path(envrmt$path_dgm1_th, "unzipped_tiles")
dir_create(unzipped)

outer_zips <- dir_ls(envrmt$path_dgm1_th, regexp = "Mehrfachdownload_.*\\.zip$")
walk(outer_zips, ~ unzip(.x, exdir = unzipped))

inner_zip_dir <- file.path(unzipped, "inner")
dir_create(inner_zip_dir)

inner_zips <- dir_ls(unzipped, recurse = TRUE, regexp = "dgm1_.*\\.zip$")
walk(inner_zips, ~ unzip(.x, exdir = inner_zip_dir))

tif_files <- dir_ls(inner_zip_dir, regexp = "\\.tif$", recurse = TRUE)

# Load and merge TIF files
collection <- sprc(tif_files)
mosaic <- mosaic(collection)


# Ensure CRS matches
if (!same.crs(mosaic, aoi)) {
  aoi <- st_transform(aoi, crs(mosaic))
}

# Clip (crop + mask)
mosaic_clipped <- crop(mosaic, aoi) |> mask(aoi)

# Save clipped DGM
outfile <- file.path(envrmt$path_raw, "dgm1_merged_th.tif")
writeRaster(mosaic_clipped, outfile, overwrite = TRUE)

# ================================= DGM1 Final Merge =================================

# Find all TIF files in path_dgm1
tif_files <- list.files(
  envrmt$path_raw,
  pattern = "^dgm1_merged_.*\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(tif_files) == 0) {
  stop("No TIF files found in path_dgm1. Check the path and file extensions.")
}

# Load all TIF files
dgm_tiles <- list()

for (i in seq_along(tif_files)) {
  tryCatch({
    dgm_tiles[[i]] <- rast(tif_files[i])
  }, error = function(e) {
    NULL
  })
}

# Remove NULL entries from failed loads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No DGM tiles were successfully loaded.")
}

# Merge all tiles
dgm_merged <- do.call(terra::mosaic, c(dgm_tiles, fun = mean))

# Save merged DGM as GeoTIFF
output_path <- file.path(envrmt$path_dgm1, "dem_harz.tif")
terra::writeRaster(dgm_merged, output_path, overwrite = TRUE)
