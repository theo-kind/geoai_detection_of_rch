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

# ================================= Create Training Points =================================
# Define training points as a data frame
training_points <- data.frame(
  x = c(637202.34, 603195.8, 595520.0, 611377.50, 637064.95, 591551.5, 617579.09, 653158.43),
  y = c(5732966.53, 5743360.2, 5729341.3, 5722351.32, 5717617.61, 5748581.7, 5732842.36, 5722810.56)
)

# Convert to sf object (EPSG:32632 - UTM Zone 32N)
training_points_sf <- st_as_sf(
  training_points,
  coords = c("x", "y"),
  crs = 32632
)


# ================================= Create Training Areas =================================
# Function to create rectangular training areas around points
create_training_area <- function(point, size = 750) {
  # size is the half side length so the point lies in the center
  half_size <- size / 2
  
  # Get point coordinates
  coords <- st_coordinates(point)
  x <- coords[1, 1]
  y <- coords[1, 2]
  
  # Create rectangle as polygon
  rect <- st_polygon(list(
    matrix(c(
      x - half_size, y - half_size,
      x + half_size, y - half_size,
      x + half_size, y + half_size,
      x - half_size, y + half_size,
      x - half_size, y - half_size
    ), ncol = 2, byrow = TRUE)
  ))
  
  return(st_sfc(rect, crs = 32632))
}

# Create training areas for all points
training_areas <- st_sfc(
  lapply(
    1:nrow(training_points_sf),
    function(i) {
      create_training_area(training_points_sf[i, ])[[1]]
    }
  ),
  crs = 32632
)

# Convert to sf object with ID
training_areas_sf <- st_sf(
  id = 1:length(training_areas),
  geometry = training_areas
)

# Load and transform AOI boundary
harz_boundary <- st_read(file.path(envrmt$path_aoi, "harz_boundary.shp"))
harz_utm <- st_transform(harz_boundary, crs = 32632)


# ================================= Visualization =================================
# Create plot
plot <- ggplot() +
  # Harz AOI
  geom_sf(
    data = harz_utm,
    fill = "forestgreen",
    color = "black",
    alpha = 1,
    linewidth = 0
  ) +
  # Training areas
  geom_sf(
    data = training_areas_sf,
    fill = "steelblue",
    color = "darkblue",
    alpha = 0.5,
    linewidth = 1
  ) +
  # Labels for training areas
  geom_sf_label(
    data = training_areas_sf,
    aes(label = id),
    size = 4,
    color = "darkblue"
  ) +
  theme_minimal() +
  labs(
    title = "Training Areas in the Harz Region",
    x = "Easting",
    y = "Northing"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    panel.grid.major = element_line(color = "grey95")
  )

# Print and save visualization
print(plot)

ggsave(
  filename = file.path(envrmt$path_run, "training_areas_visualization.png"),
  plot = plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Visualization saved.\n\n")


# ================================= Save Geopackage =================================
# Define output path for training areas
output_path <- file.path(envrmt$path_modelling, "training_areas.gpkg")

# Write training areas to GeoPackage
st_write(
  training_areas_sf,
  dsn = output_path,
  layer = "training_areas",
  driver = "GPKG",
  append = FALSE
)

cat("Training areas saved to:", output_path, "\n\n")


# ================================= Clip DEM to Training Areas =================================
# Transform training areas to DEM CRS (EPSG:25832 - UTM Zone 32N)
training_areas_utm <- st_transform(training_areas_sf, crs = 25832)

# Load DEM (Digital Elevation Model)
dgm <- rast(file.path(envrmt$path_dgm1, "dem_harz.tif"))

# Clip DEM for each training area and save
for (i in 1:nrow(training_areas_utm)) {
  # Get current training area
  training_area <- training_areas_utm[i, ]
  area_id <- training_area$id
  
  # Clip and mask DEM to training area
  dgm_clipped <- crop(dgm, training_area) %>%
    mask(training_area)
  
  # Define output filename
  output_filename <- paste0("dgm_training_area_", area_id, ".tif")
  output_path <- file.path(envrmt$path_processed, output_filename)
  
  # Save clipped DEM
  writeRaster(dgm_clipped, output_path, overwrite = TRUE)
  
  cat("Training area", area_id, "- DEM clipped and saved\n")
}

cat("\nAll DEM files have been clipped to training areas and saved.\n")