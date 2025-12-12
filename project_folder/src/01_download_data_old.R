# ================================= set environment =================================
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



# ================================= Niedersachsen: DGM1 Download, Merge & Clip =================================

# Load necessary libraries
library(sf)
library(terra)
library(httr)
library(jsonlite)
library(purrr)
library(dplyr)

# Read AOI
aoi <- st_read(file.path(envrmt$path_aoi, "harz_boundary_2000m_buffer.shp"))

# Define URLs
geojson_url <- "https://arcgis-geojson.s3.eu-de.cloud-object-storage.appdomain.cloud/dgm1/lgln-opengeodata-dgm1.geojson"

cat("Downloading GeoJSON with DGM1 tile information...\n")

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

cat("Total tiles available:", nrow(tiles_sf), "\n")

# Reproject AOI to WGS84 to match tiles
aoi_wgs84 <- st_transform(aoi, st_crs(tiles_sf))

# Find intersecting tiles
intersecting_indices <- st_intersects(aoi_wgs84, tiles_sf)
tile_indices <- unique(unlist(intersecting_indices))

cat("Tiles intersecting AOI:", length(tile_indices), "\n")

if (length(tile_indices) == 0) {
  stop("No intersecting tiles found. Check your AOI and tile extent.")
}

# Extract download URLs from dgm1 column
download_urls <- tiles_sf$dgm1[tile_indices]
download_urls <- download_urls[!is.na(download_urls)]

cat("Downloading", length(download_urls), "tile(s)...\n")

# Download tiles
dgm_tiles <- list()

for (i in seq_along(download_urls)) {
  url <- download_urls[i]
  filename <- basename(url)
  filepath <- file.path(envrmt$path_dgm1_ni, filename)
  
  cat("Downloading tile", i, "of", length(download_urls), ":", filename, "\n")
  
  # Download file
  tryCatch({
    download.file(url, filepath, mode = "wb", quiet = FALSE)
    
    # Read raster
    dgm_tiles[[i]] <- rast(filepath)
    cat("  ✓ Successfully loaded\n")
  }, error = function(e) {
    cat("  ✗ Error downloading:", conditionMessage(e), "\n")
  })
}

# Remove NULL entries from failed downloads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No tiles were successfully downloaded.")
}

cat("\nMerging", length(dgm_tiles), "tile(s)...\n")

# Merge tiles
dgm_merged <- do.call(mosaic, c(dgm_tiles, list(fun = "mean")))

cat("Merged DGM extent:", ext(dgm_merged), "\n")

# Reproject AOI to match DGM if necessary
if (st_crs(aoi)$proj4string != crs(dgm_merged)) {
  aoi_dgm_crs <- st_transform(aoi, crs(dgm_merged))
} else {
  aoi_dgm_crs <- aoi
}

# Convert AOI to vect for terra operations
aoi_vect <- vect(aoi_dgm_crs)

cat("Clipping DGM to AOI...\n")

# Clip DGM to AOI
dgm_clipped <- crop(dgm_merged, aoi_vect, mask = TRUE)

# Save clipped DGM
output_path <- file.path(envrmt$path_dgm1, "dgm1_niedersachsen_clipped.tif")
writeRaster(dgm_clipped, output_path, overwrite = TRUE)

cat("✓ Clipped DGM saved to:", output_path, "\n")



# ================================= DGM1 Sachsen-Anhalt Merge & Clip =================================
# (1) manually download "teil 1" and "teil 2" from https://www.lvermgeo.sachsen-anhalt.de/de/gdp-dgm1-landesweit.html
# (2) save both .zip in the folder "/data/raw/dgm1_st"
# (3) unzip the files

library(terra)
library(sf)
library(dplyr)

cat("Starting DGM1 Sachsen-Anhalt merge and clipping...\n\n")

# Find all TIF files in path_dgm1_st
tif_files <- list.files(
  envrmt$path_dgm1_st,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

cat("Found", length(tif_files), "TIF file(s) in path_dgm1_st\n")

if (length(tif_files) == 0) {
  stop("No TIF files found in path_dgm1_st. Check the path and file extensions.")
}

# Print found files
cat("\nFiles found:\n")
for (f in tif_files) {
  cat("  -", basename(f), "\n")
}

# Transform AOI to match DGM CRS if necessary (do this early)
cat("=== Transforming AOI ===\n")
# First, load one tile to get the CRS
temp_tile <- rast(tif_files[1])
aoi_dgm_crs <- st_transform(aoi, crs(temp_tile))
aoi_vect <- vect(aoi_dgm_crs)
cat("AOI transformed to CRS:", crs(temp_tile), "\n")

# Load and clip all TIF files
cat("\n=== Loading and clipping tiles ===\n")
dgm_tiles <- list()

for (i in seq_along(tif_files)) {
  cat("Processing tile", i, "of", length(tif_files), ":", basename(tif_files[i]), "...")
  
  tryCatch({
    tile <- rast(tif_files[i])
    # Clip immediately after loading to reduce data size
    tile_clipped <- crop(tile, aoi_vect, mask = TRUE)
    
    # Only keep tile if it has data (not completely outside AOI)
    if (!is.null(tile_clipped) && nlyr(tile_clipped) > 0) {
      dgm_tiles[[i]] <- tile_clipped
      cat(" ✓ (clipped)\n")
    } else {
      cat(" - (no overlap with AOI)\n")
    }
  }, error = function(e) {
    cat(" ✗ Error:", conditionMessage(e), "\n")
  })
}

# Remove NULL entries from failed loads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No DGM tiles with overlap to AOI were found.")
}

cat("\n=== Merging tiles ===\n")
cat("Total tiles to merge:", length(dgm_tiles), "\n")

cat("\n=== Merging clipped tiles ===\n")
cat("Tiles to merge:", length(dgm_tiles), "\n")
cat("CRS of tiles:", crs(dgm_tiles[[1]]), "\n")
cat("Resolution:", res(dgm_tiles[[1]]), "\n")

# Merge all tiles at once using mosaic
dgm_merged <- do.call(mosaic, c(dgm_tiles, fun = "mean"))

cat("✓ Tiles merged successfully\n")
cat("Final DGM extent:", as.vector(ext(dgm_merged)), "\n")
cat("Final DGM resolution:", res(dgm_merged), "\n")

# Use merged result as clipped DGM
dgm_clipped <- dgm_merged

# Save clipped DGM
output_filename <- "dgm1_st_clipped.tif"
output_path <- file.path(envrmt$path_dgm1, output_filename)
writeRaster(dgm_clipped, output_path, overwrite = TRUE)

# ================================= Thüringen: DGM1 Extract, Merge & Clip =================================
# manually download dgm1-data from: https://geoportal.thueringen.de/gdi-th/download-offene-geodaten/download-hoehendaten
# save .zip Data (i.e. "Mehrfachdownload_wKrrO6_UrBfm4.zip") in path_dgm1_th folder for further processing.

library(fs)
library(purrr)
library(terra)

unzipped <- file.path(envrmt$path_dgm1_th, "unzipped_tiles")
dir_create(unzipped)

outer_zips <- dir_ls(envrmt$path_dgm1_th, regexp = "Mehrfachdownload_.*\\.zip$")
walk(outer_zips, ~ unzip(.x, exdir = unzipped))

inner_zip_dir <- file.path(unzipped, "inner")
dir_create(inner_zip_dir)

inner_zips <- dir_ls(unzipped, recurse = TRUE, regexp = "dgm1_.*\\.zip$")

walk(inner_zips, ~ unzip(.x, exdir = inner_zip_dir))

tif_files <- dir_ls(inner_zip_dir, regexp = "\\.tif$")
tif_files

# TIF-Liste
tif_files <- dir_ls(inner_zip_dir, regexp = "\\.tif$")

collection <- sprc(tif_files)   
mosaic    <- mosaic(collection) 

tif_files <- dir_ls(inner_zip_dir, regexp = "\\.tif$", recurse = TRUE)
length(tif_files)

# AOI laden
aoi_path <- file.path(envrmt$path_aoi, "harz_boundary_2000m_buffer.shp")
aoi <- vect(aoi_path)

# sicherstellen, dass CRS passt
if (!same.crs(mosaic, aoi)) {
  aoi <- project(aoi, mosaic)
}

# clippen (crop + mask)
mosaic_clipped <- crop(mosaic, aoi) |> mask(aoi)


outfile <- file.path(envrmt$path_dgm1, "dgm1_thueringen_merged_clipped.tif")
writeRaster(mosaic_clipped, outfile, overwrite = TRUE)



# ================================= DGM1 Final Merge =================================

library(terra)
library(sf)

cat("Starting DGM1 final merge...\n\n")

# Find all TIF files in path_dgm1
tif_files <- list.files(
  envrmt$path_dgm1,
  pattern = "\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

cat("Found", length(tif_files), "TIF file(s) in path_dgm1\n")

if (length(tif_files) == 0) {
  stop("No TIF files found in path_dgm1. Check the path and file extensions.")
}

# Print found files
cat("\nFiles found:\n")
for (f in tif_files) {
  cat("  -", basename(f), "\n")
}

# Load all TIF files
cat("\n=== Loading tiles ===\n")
dgm_tiles <- list()

for (i in seq_along(tif_files)) {
  cat("Loading tile", i, "of", length(tif_files), ":", basename(tif_files[i]), "...")
  
  tryCatch({
    dgm_tiles[[i]] <- rast(tif_files[i])
    cat(" ✓\n")
  }, error = function(e) {
    cat(" ✗ Error:", conditionMessage(e), "\n")
  })
}

# Remove NULL entries from failed loads
dgm_tiles <- Filter(Negate(is.null), dgm_tiles)

if (length(dgm_tiles) == 0) {
  stop("No DGM tiles were successfully loaded.")
}

cat("\n=== Merging tiles ===\n")
cat("Total tiles to merge:", length(dgm_tiles), "\n")

# Check CRS and resolution
cat("CRS of tiles:", crs(dgm_tiles[[1]]), "\n")
cat("Resolution:", res(dgm_tiles[[1]]), "\n")

# Merge all tiles at once
dgm_merged <- do.call(mosaic, c(dgm_tiles, fun = "mean"))

cat("✓ Tiles merged successfully\n")
cat("Merged DGM extent:", as.vector(ext(dgm_merged)), "\n")
cat("Merged DGM resolution:", res(dgm_merged), "\n")

# Save merged DGM as GeoTIFF
output_filename <- "dgm1_merged.tif"
output_path <- file.path(envrmt$path_dgm1, output_filename)
cat("\nWriting merged DGM to GeoTIFF...\n")
writeRaster(dgm_merged, output_path, overwrite = TRUE)
