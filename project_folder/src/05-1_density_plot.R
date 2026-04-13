# ================================= Notes =================================
#
# This script performs the following steps:
# 1. Loads prediction centroids from script 04-3.
# 2. Creates a regular grid with configurable cell size.
# 3. Calculates point density (kilns per km²) for each grid cell.
# 4. Saves density grid and visualization to organized folder structure.
#
# Output:
#   data/modelling/prediction/<INPUT_LAYER_NAME>/density/<GRID_SIZE_METERS>/
#
# ================================= Setup =================================
library(envimaR)

rootDir <- paste0(getwd(), "/project_folder")

rootDir <- envimaR::alternativeEnvi(
  root_folder = rootDir,
  alt_env_id = "COMPUTERNAME",
  alt_env_value = "PCRZP",
  alt_env_root_folder = "F:/BEN/edu"
)

path <- file.path(rootDir, "src", "00_geoAI_setup.R")
source(path, echo = FALSE)

# ================================= Parameters =================================

INPUT_LAYER_NAME <- "DEM_SL_SVF_OP"  # Change as needed
MODEL_ID <- "DEM_SL_SVF_OP_20_5_F"   # Best model from training
GRID_SIZE_METERS <- 1000             # Change to test different grid sizes

# ================================= Setup Output Directories =================================

output_base <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME, "density", GRID_SIZE_METERS)
if (!dir.exists(output_base)) {
  dir.create(output_base, recursive = TRUE)
}

# ================================= Load Data =================================

centroids_file <- file.path(envrmt$path_prediction, INPUT_LAYER_NAME, "vectors", "prediction_centroids.gpkg")

if (!file.exists(centroids_file)) {
  stop("Centroids file not found: ", centroids_file,
       "\n Run script 04-3 first.")
}

points_sf <- sf::st_read(centroids_file, quiet = TRUE)

aoi <- sf::st_read(file.path(envrmt$path_aoi, "harz_boundary.shp"), quiet = TRUE)
aoi <- sf::st_transform(aoi, st_crs(points_sf))

# ================================= Calculate Area =================================

aoi_area_km2 <- as.numeric(sf::st_area(aoi)) / 1000000

# ================================= Create Grid =================================

grid <- sf::st_as_sf(
  sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(aoi)),
    cellsize = GRID_SIZE_METERS,
    square = TRUE
  )
)

grid <- grid[aoi, ]

# ================================= Calculate Density =================================

total_points <- nrow(points_sf)
grid$point_count <- lengths(sf::st_intersects(grid, points_sf))



grid$actual_area_km2 <- as.numeric(sf::st_area(grid)) / 1e6
grid$density_per_km2 <- grid$point_count / grid$actual_area_km2

# ================================= Summary Statistics =================================

cells_with_data <- sum(grid$point_count > 0)
cells_empty <- sum(grid$point_count == 0)

grid_with_values <- grid$density_per_km2[grid$point_count > 0]

min_density <- if (length(grid_with_values) > 0) min(grid_with_values) else NA
max_density <- max(grid$density_per_km2)
mean_density <- if (length(grid_with_values) > 0) mean(grid_with_values) else NA
median_density <- if (length(grid_with_values) > 0) median(grid_with_values) else NA

# ================================= Create Visualization =================================

grid_with_data <- grid[grid$point_count > 0, ]

plot_density <- ggplot(grid_with_data) +
  geom_sf(aes(fill = density_per_km2), color = NA) +
  geom_sf(data = aoi, fill = NA, color = "black", linewidth = 0.5) +
  scale_fill_viridis_c(
    name = "Kilns/km²",
    option = "viridis"
  ) +
  labs(
    x = "Easting",
    y = "Northing"
  ) +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    legend.position = c(0.925, 0.755),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.margin = margin(2, 2, 2, 2)
  )
# ================================= Save Outputs =================================

# Prepare grid for export (keep only necessary columns)
grid_export <- grid %>%
  select(point_count, density_per_km2) %>%
  mutate(point_count = as.integer(point_count))

# Save visualization
output_png <- file.path(
  output_base,
  paste0("density_map.png")
)
ggplot2::ggsave(output_png, plot_density, width = 7, height = 4, dpi = 300)

# Save grid as GeoPackage
grid_gpkg <- file.path(
  output_base,
  "density_grid.gpkg"
)
sf::st_write(grid_export, grid_gpkg, append = FALSE, quiet = TRUE)

# Save grid as Shapefile
grid_shp <- file.path(
  output_base,
  "density_grid.shp"
)
sf::st_write(grid_export, grid_shp, append = FALSE, quiet = TRUE)

plot_density

# ================================= Export Statistics =================================

statistics <- data.frame(
  timestamp = Sys.time(),
  model_id = MODEL_ID,
  input_layer = INPUT_LAYER_NAME,
  grid_size_meters = GRID_SIZE_METERS,
  aoi_area_km2 = round(aoi_area_km2, 4),
  total_kilns = total_points,
  grid_cells_total = nrow(grid),
  grid_cells_with_data = cells_with_data,
  grid_cells_empty = cells_empty,
  density_min_per_km2 = round(min_density, 4),
  density_max_per_km2 = round(max_density, 4),
  density_mean_per_km2 = round(mean_density, 4),
  density_median_per_km2 = round(median_density, 4)
)

stats_file <- file.path(
  output_base,
  "density_statistics.csv"
)
write.csv(statistics, stats_file, row.names = FALSE)
