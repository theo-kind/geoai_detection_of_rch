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
harz_query <- opq(bbox = "Germany") %>%
  add_osm_feature(key = "name", value = "Harz") %>%
  add_osm_feature(key = "place", value = "region")
harz <- osmdata_sf(harz_query)
harz_boundary <- harz$osm_multipolygons

harz_boundary_clean <- harz_boundary %>%
  select(osm_id, name) %>%
  st_cast("POLYGON")  
harz_utm <-st_transform(harz_boundary_clean, crs = 32632)

# save file
harz_path <- file.path(envrmt$path_aoi, "harz_boundary.shp", fsep = "/")
st_write(harz_utm, harz_path, append = FALSE)
cat("file saved:", harz_path, "\n")

# ================================= plot =================================
plot(st_geometry(harz_utm), main = "Area of interest (AOI)", col = "darkgreen")

