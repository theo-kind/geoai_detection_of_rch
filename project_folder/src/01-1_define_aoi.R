# ================================= set enviroment =================================
library(envimaR)

# define the project root folder
rootDir <- paste0(getwd(),"/geoai_project") # this is the mandantory foldername of the whole project

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


# ================================= AOI Harz speichern =================================
library(osmdata)
library(sf)
library(dplyr)

# Harz Boundary downloaden
harz_query <- opq(bbox = "Germany") %>%
  add_osm_feature(key = "name", value = "Harz") %>%
  add_osm_feature(key = "place", value = "region")
harz <- osmdata_sf(harz_query)
harz_boundary <- harz$osm_multipolygons

# Feldnamen bereinigen (Shapefile unterstützt keine Sonderzeichen wie ":")
# Nur die Geometrie behalten oder wichtige Felder selektieren
harz_boundary_clean <- harz_boundary %>%
  select(osm_id, name) %>%  # nur diese Felder behalten
  st_cast("POLYGON")  # MultiPolygon in Polygon umwandeln (optional, aber oft praktisch)

# Ursprüngliche Harz-Boundary speichern
harz_path <- file.path(envrmt$path_aoi, "harz_boundary.shp", fsep = "/")
st_write(harz_boundary_clean, harz_path, append = FALSE)
cat("Harz Boundary gespeichert unter:", harz_path, "\n")

# ================================= 2000 m Puffer erstellen =================================

# Koordinatenreferenzsystem auf metrisches System transformieren (für genaue 500m)
# EPSG:31256 ist UTM Zone 33N (für Harz-Region geeignet)
harz_utm <- st_transform(harz_boundary_clean, crs = 31256)

# 2000 m Puffer erstellen
harz_buffer <- st_buffer(harz_utm, dist = 2000)

# Zurück zum Original-CRS transformieren (falls gewünscht)
harz_buffer <- st_transform(harz_buffer, crs = st_crs(harz_boundary))

# Gepufferte Version speichern
buffer_path <- file.path(envrmt$path_aoi, "harz_boundary_2000m_buffer.shp", fsep = "/")
st_write(harz_buffer, buffer_path, append = FALSE)
cat("Harz Boundary mit 500m Puffer gespeichert unter:", buffer_path, "\n")

# ================================= Visualisierung =================================
plot(st_geometry(harz_buffer), main = "Harz mit 2000 m Puffer", col = "lightgreen")
plot(st_geometry(harz_boundary_clean), col = "darkgreen", border = "darkgreen", add = TRUE)
legend("topright", legend = c("2000 m Puffer", "Original Boundary"), 
       fill = c("lightgreen", "darkgreen"))
