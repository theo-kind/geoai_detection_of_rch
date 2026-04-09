# ================================= Notes =================================
#
# Downloads the Harz region boundary from OpenStreetMap, reprojects it to
# UTM Zone 32N (EPSG:32632), and saves it as a shapefile.
# Additionally downloads landuse=forest and natural=wood within the Harz AOI
# from OSM and saves them to path_osm.
#
# Input:  - (OpenStreetMap query)
# Output: data/aoi/harz_boundary.shp
#         data/osm/harz_landuse_forest.shp
#         data/osm/harz_natural_wood.shp
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
library(osmdata)
library(dplyr)

# ================================= download and save AOI (Harz region) =================================
# download harz boundary
harz_query <- opq(bbox = c(10.20, 51.40, 11.55, 51.95)) %>%  # xmin, ymin, xmax, ymax (WGS84)
  add_osm_feature(key = "name", value = "Harz") %>%
  add_osm_feature(key = "place", value = "region")
harz <- osmdata_sf(harz_query)
harz_boundary <- harz$osm_multipolygons
harz_boundary_clean <- harz_boundary %>%
  select(osm_id, name) %>%
  st_cast("POLYGON")  
harz_utm <- st_transform(harz_boundary_clean, crs = 32632)

# save file
harz_path <- file.path(envrmt$path_aoi, "harz_boundary.shp", fsep = "/")
st_write(harz_utm, harz_path, append = FALSE)
cat("file saved:", harz_path, "\n")

# ================================= download OSM forest data within AOI =================================
# derive bounding box from Harz AOI (in WGS84, as required by osmdata)
harz_wgs84 <- st_transform(harz_utm, crs = 4326)
harz_bbox  <- st_bbox(harz_wgs84)

# --- landuse=forest ---
forest_query <- opq(bbox = harz_bbox) %>%
  add_osm_feature(key = "landuse", value = "forest")
forest_osm <- osmdata_sf(forest_query)

# --- natural=wood ---
wood_query <- opq(bbox = harz_bbox) %>%
  add_osm_feature(key = "natural", value = "wood")
wood_osm <- osmdata_sf(wood_query)

# --- combine both into one layer ---
forest_wood_poly <- bind_rows(
  forest_osm$osm_polygons,
  forest_osm$osm_multipolygons,
  wood_osm$osm_polygons,
  wood_osm$osm_multipolygons
) %>%
  select(osm_id, name, landuse, natural) %>%
  st_cast("POLYGON") %>%
  st_transform(crs = 32632) %>%
  st_filter(harz_utm)                            # clip to exact AOI boundary

# save combined layer
forest_wood_path <- file.path(envrmt$path_osm, "harz_forest.shp", fsep = "/")
st_write(forest_wood_poly, forest_wood_path, append = FALSE)
cat("file saved:", forest_wood_path, "\n")

# ================================= plot =================================
plot(st_geometry(harz_utm),        main = "Area of interest (AOI)", col = "lightgrey")
plot(st_geometry(forest_wood_poly), add = TRUE, col = "darkgreen")
