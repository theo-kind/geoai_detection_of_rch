# ================================= Notes =================================
#
# Creates rectangular training areas (multiples of 128 m) around manually
# defined point locations within the Harz region.
#
# Input:  data/aoi/harz_boundary.shp
# Output: data/modelling_data/training_areas.gpkg
#         docs/training_areas_visualization.png
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

# ================================= Create Training Points =================================
# Define training points as a data frame
training_points <- data.frame(
  x = c(637202.34, 603195.8, 595520.0, 611377.50, 637064.95, 591551.5, 617579.09, 653158.43,
        623685.79, 649964.12, 638804.30, 659551.30, 632296.4, 586179.2),
  y = c(5732966.53, 5743360.2, 5729341.3, 5722351.32, 5717617.61, 5748581.7, 5732842.36, 5722810.56,
        5726716.54, 5715040.02, 5728974.10, 5715595.70, 5713596.9, 5744411.9)
)




# Convert to sf object (EPSG:32632 - UTM Zone 32N)
training_points_sf <- st_as_sf(
  training_points,
  coords = c("x", "y"),
  crs = 32632
)

# ================================= Create Training Areas =================================
# Function to create rectangular training areas around points

## choose a size which is a multiple of 128 (tile size used in model)
## possible sizes: 640, 768, 896, 1024, 

create_training_area <- function(point, size = 640) {
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
  filename = file.path(envrmt$path_docs, "training_areas_visualization.png"),
  plot = plot,
  width = 12,
  height = 7,
  dpi = 300
)

cat("Visualization saved.\n\n")


# ================================= Save Geopackage =================================
# Define output path for training areas
output_path <- file.path(envrmt$path_modelling_data, "training_areas.gpkg")

# Write training areas to GeoPackage
st_write(
  training_areas_sf,
  dsn = output_path,
  layer = "training_areas",
  driver = "GPKG",
  append = FALSE
)

cat("Training areas saved to:", output_path, "\n\n")