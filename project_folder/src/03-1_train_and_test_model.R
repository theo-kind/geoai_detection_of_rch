# ================================= Notes =================================
#
# Grid-search training pipeline. Trains and evaluates one U-Net model per
# parameter combination (input layer, epochs, buffer, remove-NA flag)
# and records all results in a single CSV.
#
# Input:  data/dem1/dem_harz_<INPUT_LAYER>.tif
#         data/modelling_data/training_areas.gpkg
#         data/modelling_data/rch_sites.gpkg
# Output: data/modelling/model_training_data/dem/dem_train_area_<id>_tile_<n>.png
#         data/modelling/model_training_data/mask/mask_train_area_<id>_tile_<n>.png
#         data/modelling/model_testing_data/dem/dem_test_area_<id>_tile_<n>.png
#         data/modelling/model_testing_data/mask/mask_test_area_<id>_tile_<n>.png
#         data/modelling/models/unet_<ID>.hdf5
#         data/modelling/models/<ID>_training_history.RDS
#         docs/grid_search/hist/<ID>_training_history.png
#         docs/grid_search/test/<ID>_test_pred.png
#         docs/grid_search/GRID_PARAMS.csv
#         docs/grid_search/experiment_results.csv
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

# ================================= Define Grid Parameters =================================

GRID_PARAMS <- expand.grid(
  INPUT_LAYER = c("DEM_HS_SL_SVF","HS_SL_SVF_OP","VAT4b","VAT_DEM_HS_SL"),
  EPOCHS = c(15, 20, 25),
  BUFFER = c(0,2,5),
  REMOVE_NA = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)
# Save GRID_PARAMS as CSV
write.csv(
  GRID_PARAMS,
  file = file.path(envrmt$path_grid_search, "GRID_PARAMS.csv"),
  row.names = FALSE
)

# ================================= GRID SEARCH LOOP =================================

for (run in seq(1, nrow(GRID_PARAMS))) {
  
  INPUT_LAYER_NAME <- GRID_PARAMS[run, "INPUT_LAYER"]
  EPOCHS <- GRID_PARAMS[run, "EPOCHS"]
  BUFFER <- GRID_PARAMS[run, "BUFFER"]
  REMOVE_NA <- GRID_PARAMS[run, "REMOVE_NA"]
  
  REMOVE_FLAG <- ifelse(REMOVE_NA, "T", "F")
  
  ID <- paste(
    INPUT_LAYER_NAME,
    EPOCHS,
    BUFFER,
    REMOVE_FLAG,
    sep = "_"
  )
  
  INPUT_RASTER_PATH <- file.path(
    envrmt$path_dem1,
    paste0("dem_harz_", INPUT_LAYER_NAME, ".tif")
  )
  
  RCH_FILE_PATH <- file.path(
    envrmt$path_modelling_data,
    "rch_sites.gpkg"
  )
  
  # ================= Clean folders =================
  # remove files in training and testing folders
  clean_dir_files_only <- function(path) {
    if (dir.exists(path)) {
      files <- list.files(path, 
                          full.names = TRUE, 
                          recursive = TRUE)
      files <- files[file.info(files)$isdir == FALSE]
      unlink(files, force = TRUE)
    }
  }
  
  clean_dir_files_only(envrmt$path_model_training_data)
  clean_dir_files_only(envrmt$path_model_testing_data)
  
  # ================================= Read data =================================
  # Load DEM multiband raster
  dem_multiband <- terra::rast(INPUT_RASTER_PATH)
  
  LAYER <- terra::nlyr(dem_multiband)
  
  # Load training areas and rch data
  training_areas <- sf::st_read(file.path(envrmt$path_modelling_data, "training_areas.gpkg"))
  rch <- sf::st_read(RCH_FILE_PATH)
  
  # Ensure same CRS
  rch <- sf::st_transform(rch, crs(dem_multiband))
  training_areas <- sf::st_transform(training_areas, crs(dem_multiband))
  
  # ================================= Create masks for each training area =================================
  # Process each training area separately
  for (area_id in training_areas$id) {
    
    # Extract single training area
    current_area <- training_areas[training_areas$id == area_id, ]
    
    # Crop DEM to this training area
    dem_cropped <- terra::crop(dem_multiband, current_area)
    
    # Crop rch to this training area
    rch_area <- sf::st_crop(rch, sf::st_bbox(current_area))
    
    # Add buffer to rch polygons
    rch_buffered <- sf::st_buffer(rch_area, dist = BUFFER)
    
    # Rasterize the buffered rch polygons to match DEM resolution (1m)
    rasterized_mask <- terra::rasterize(
      rch_buffered,
      dem_cropped[[1]],  # Use first layer as template
      background = 0
    )
    
    # Reclassify to 0 and 1
    rasterized_mask[is.na(rasterized_mask[])] <- 0
    rasterized_mask[rasterized_mask > 1] <- 1
    
    # Save complete mask for reference
    terra::writeRaster(
      rasterized_mask,
      file.path(envrmt$path_modelling, paste0("mask_area_", area_id, ".tif")),
      overwrite = TRUE
    )
    
    # ================================= 80/20 split for this area =================================
    # Split horizontally (x-direction): 80% for training, 20% for testing
    xmin <- terra::ext(rasterized_mask)[1]
    xmax <- terra::ext(rasterized_mask)[1] + 
      round(terra::ncol(rasterized_mask) * 0.8, 0) * terra::res(rasterized_mask)[1]
    ymin <- terra::ext(rasterized_mask)[3]
    ymax <- terra::ext(rasterized_mask)[4]
    e_train <- terra::ext(xmin, xmax, ymin, ymax)
    
    # Rest for testing
    xmin_test <- xmax
    xmax_test <- terra::ext(rasterized_mask)[2]
    e_test <- terra::ext(xmin_test, xmax_test, ymin, ymax)
    
    # Crop mask and DEM for training
    mask_train <- terra::crop(rasterized_mask, e_train)
    dem_train <- terra::crop(dem_cropped, e_train)
    
    # Crop mask and DEM for testing
    mask_test <- terra::crop(rasterized_mask, e_test)
    dem_test <- terra::crop(dem_cropped, e_test)
    
    # Save training data
    terra::writeRaster(
      mask_train,
      file.path(envrmt$path_model_training_data, 
                paste0("mask_train_area_", area_id, ".tif")),
      overwrite = TRUE
    )
    terra::writeRaster(
      dem_train,
      file.path(envrmt$path_model_training_data, 
                paste0("dem_train_area_", area_id, ".tif")),
      overwrite = TRUE
    )
    
    # Save testing data
    terra::writeRaster(
      mask_test,
      file.path(envrmt$path_model_testing_data, 
                paste0("mask_test_area_", area_id, ".tif")),
      overwrite = TRUE
    )
    terra::writeRaster(
      dem_test,
      file.path(envrmt$path_model_testing_data, 
                paste0("dem_test_area_", area_id, ".tif")),
      overwrite = TRUE
    )
  }
  
  
  # ================================= Helper functions =================================
  
  # Remove tiles with only background or only foreground
  remove_empty_tiles <- function(df) {
    removed_count <- 0
    
    df <- df[order(basename(df$list_dops)), ]
    
    lapply(seq(1, nrow(df)), function(i) {
      local({
        mask_file <- df$list_masks[i]
        dem_file <- df$list_dops[i]
        
        # Check if files exist
        if(!file.exists(mask_file) || !file.exists(dem_file)) {
          return(NULL)
        }
        
        # Read mask and check if it's empty or full
        tryCatch({
          png_mask <- png::readPNG(mask_file)
          mask_vec <- as.vector(png_mask)
          
          # Count unique values (should be 0 and 1 for valid masks)
          unique_vals <- unique(mask_vec[!is.na(mask_vec)])
          
          # If all 0 (background only) or all 1 (foreground only), remove BOTH files
          if(length(unique_vals) == 1) {
            if(file.exists(dem_file)) file.remove(dem_file)
            if(file.exists(mask_file)) file.remove(mask_file)
            removed_count <<- removed_count + 1
          }
        }, error = function(e) {
          cat("Error reading file", mask_file, ":", e$message, "\n")
        })
      })
    })
    
    return(paste("Removed", removed_count, "file pairs"))
  }
  
  # ================================= Process training data =================================
  # List all training data files created in previous step
  training_mask_files <- list.files(
    envrmt$path_model_training_data,
    pattern = "mask_train_area_[0-9]+\\.tif$",
    full.names = TRUE
  )
  
  training_dem_files <- list.files(
    envrmt$path_model_training_data,
    pattern = "dem_train_area_[0-9]+\\.tif$",
    full.names = TRUE
  )
  
  # Sort by area ID
  sort_by_area_id <- function(files) {
    area_ids <- as.numeric(gsub(".*area_([0-9]+)\\.tif$", "\\1", files))
    files[order(area_ids)]
  }
  
  training_mask_files <- sort_by_area_id(training_mask_files)
  training_dem_files <- sort_by_area_id(training_dem_files)
  
  # Process each training area
  for (i in seq_along(training_mask_files)) {
    
    mask_train <- terra::rast(training_mask_files[i])
    dem_train <- terra::rast(training_dem_files[i])
    
    # Extract area ID from filename
    area_id <- as.numeric(gsub(".*area_([0-9]+)\\.tif$", "\\1", training_mask_files[i]))
    
    # Set tile size
    model_input_shape <- c(128, 128)
    
    # Create tiles from mask
    subset_ds(
      input_raster = mask_train,
      model_input_shape = model_input_shape,
      path = envrmt$path_model_training_data_mask,
      targetname = paste0("mask_train_area_", area_id, "_tile_"),
      mask = TRUE,
      normalize = FALSE
    )
    
    # Create tiles from DEM
    subset_ds(
      input_raster = dem_train,
      model_input_shape = model_input_shape,
      path = envrmt$path_model_training_data_dem,
      targetname = paste0("dem_train_area_", area_id, "_tile_"),
      mask = FALSE,
      normalize = TRUE  
      )
  }
  
  # ================================= Process testing data =================================
  # List all testing data files
  testing_mask_files <- list.files(
    envrmt$path_model_testing_data,
    pattern = "mask_test_area_[0-9]+\\.tif$",
    full.names = TRUE
  )
  
  testing_dem_files <- list.files(
    envrmt$path_model_testing_data,
    pattern = "dem_test_area_[0-9]+\\.tif$",
    full.names = TRUE
  )
  
  testing_mask_files <- sort_by_area_id(testing_mask_files)
  testing_dem_files <- sort_by_area_id(testing_dem_files)
  
  # Process each testing area
  for (i in seq_along(testing_mask_files)) {
    
    mask_test <- terra::rast(testing_mask_files[i])
    dem_test <- terra::rast(testing_dem_files[i])
    
    # Extract area ID from filename
    area_id <- as.numeric(gsub(".*area_([0-9]+)\\.tif$", "\\1", testing_mask_files[i]))
    
    # Set tile size
    model_input_shape <- c(128, 128)
    
    # Create tiles from mask
    subset_ds(
      input_raster = mask_test,
      model_input_shape = model_input_shape,
      path = envrmt$path_model_testing_data_mask,
      targetname = paste0("mask_test_area_", area_id, "_tile_"),
      mask = TRUE,
      normalize = FALSE
    )
    
    # Create tiles from DEM with PROPER normalization
    subset_ds(
      input_raster = dem_test,
      model_input_shape = model_input_shape,
      path = envrmt$path_model_testing_data_dem,
      targetname = paste0("dem_test_area_", area_id, "_tile_"),
      mask = FALSE,
      normalize = TRUE
    )
  }
  
  # ================================= Remove tiles with no variance =================================
  # Training data
  list_dems_train <- list.files(
    envrmt$path_model_training_data_dem,
    full.names = TRUE,
    pattern = "*.png"
  )
  
  list_masks_train <- list.files(
    envrmt$path_model_training_data_mask,
    full.names = TRUE,
    pattern = "*.png"
  )
  
  df_train <- data.frame(list_dops = list_dems_train, list_masks = list_masks_train)
  
  if (REMOVE_NA) {
    result_train <- remove_empty_tiles(df_train)
  }
  
  # Testing data
  list_dems_test <- list.files(
    envrmt$path_model_testing_data_dem,
    full.names = TRUE,
    pattern = "*.png"
  )
  
  list_masks_test <- list.files(
    envrmt$path_model_testing_data_mask,
    full.names = TRUE,
    pattern = "*.png"
  )
  
  df_test <- data.frame(list_dops = list_dems_test, list_masks = list_masks_test)
  if (REMOVE_NA) {
    result_test  <- remove_empty_tiles(df_test)
  }
  
  
  
  # ================================= Load and prepare data =================================
  
  files <- data.frame(
    img = list.files(
      envrmt$path_model_training_data_dem,
      full.names = TRUE,
      pattern = "*.png"
    ),
    mask = list.files(
      envrmt$path_model_training_data_mask,
      full.names = TRUE,
      pattern = "*.png"
    )
  )
  
  tensorflow::set_random_seed(7)
  data <- rsample::initial_split(files, prop = 0.8)
  
  batch_size <- 8
  
  training_dataset <- prepare_ds(
    rsample::training(data),
    train = TRUE,
    predict = FALSE,
    batch_size = batch_size
  )
  
  validation_dataset <- prepare_ds(
    rsample::testing(data),
    train = FALSE,
    predict = FALSE,
    batch_size = batch_size
  )
  
  
  
  # ================================= Model Training =================================
  unet_model <- get_unet_128(input_shape = c(128, 128, LAYER))
  
  # compile the model
  unet_model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.0001),
    loss = "binary_crossentropy",
    metrics = list(
      "accuracy",
      keras::metric_recall(name = "recall"),
      keras::metric_precision(name = "precision")
    )
  )
  
  # train the model for fixed number of epochs
  hist <- unet_model %>% fit(
    training_dataset,
    validation_data = validation_dataset,
    epochs = EPOCHS,
    verbose = 1
  )
  
  # save the model
  unet_model %>% 
    save_model_hdf5(file.path(envrmt$path_models, paste0("unet_", ID, ".hdf5")))
  
  # plot training history
  plot(hist)
  save_plot <- file.path(envrmt$path_hist, paste0(ID, "_training_history.png"))
  ggplot2::ggsave(save_plot, width = 8, height = 6)
  
  
  # save training history
  path <- file.path(envrmt$path_models, paste0(ID, "_training_history.RDS"))
  saveRDS(hist, file = path)
  
  # ================================= Test prediction =================================
  
  # Set up parameters
  batch_size <- 8
  
  # ================================= Prepare test data =================================
  
  # List and prepare the files (bereits vorhandene PNGs aus allen Testgebieten)
  test_file <- data.frame(
    img = list.files(
      envrmt$path_model_testing_data_dem,
      full.names = TRUE,
      pattern = "*.png"
    ),
    mask = list.files(
      envrmt$path_model_testing_data_mask,
      full.names = TRUE,
      pattern = "*.png"
    )
  )
  
  # Prepare testing dataset
  testing_dataset <- prepare_ds(
    test_file,
    train = FALSE,
    predict = FALSE,
    batch_size = batch_size
  )
  
  # ================================= Load model and evaluate =================================
  
  # Load the U-Net model
  unet_model <- keras::load_model_hdf5(
    file.path(envrmt$path_models, paste0("unet_", ID, ".hdf5")),
    compile = TRUE
  )
  
  # Evaluate the model with test set
  ev <- unet_model$evaluate(testing_dataset)
  
  # ================================= Visual comparison =================================
  set.seed(101)
  
  # Get sample of data from testing data 
  t_sample <- floor(runif(n = 10, min = 1, max = nrow(test_file)))
  
  # Create PNG file for visualization
  png_file <- file.path(envrmt$path_test, paste0(ID, "_test_pred.png"))
  png(png_file, width = 1000, height = 1000)
  
  layout(matrix(1:10, ncol = 2))
  par(mar = c(2, 2, 2, 2))
  
  # Simple visual comparison of mask, image and prediction
  for (idx in seq_along(t_sample)) {
    i <- t_sample[idx]
    png_path <- test_file[i, ]
    
    img <- magick::image_read(png_path[, 1])
    mask <- magick::image_read(png_path[, 2])
    pred <- magick::image_read(
      terra::as.raster(predict(object = unet_model, testing_dataset)[i, , , ])
    )
    
    out <- magick::image_append(c(
      magick::image_annotate(
        mask,
        "Mask",
        size = 10,
        color = "black",
        boxcolor = "white"
      ),
      magick::image_annotate(
        img,
        "Original Image",
        size = 10,
        color = "black",
        boxcolor = "white"
      ),
      magick::image_annotate(
        pred,
        "Prediction",
        size = 10,
        color = "black",
        boxcolor = "white"
      )
    ))
    
    plot(out)
  }
  
  dev.off()
  
  # ================================= Create/Update Results Table =================================
  
  results_file <- file.path(envrmt$path_grid_search, "experiment_results.csv")
  
  new_result <- data.frame(
    Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    ExperimentID = ID,
    InputLayer = INPUT_LAYER_NAME,
    Epochs = EPOCHS,
    Buffer = BUFFER,
    RemoveNA = REMOVE_FLAG,
    ValLoss = round(ev[1], 6),
    ValAccuracy = round(ev[2], 6),
    ValRecall = round(ev[3], 6),
    ValPrecision = round(ev[4], 6),
    stringsAsFactors = FALSE
  )
  
  if (file.exists(results_file)) {
    results_df <- read.csv(results_file, stringsAsFactors = FALSE)
    results_df <- rbind(results_df, new_result)
  } else {
    results_df <- new_result
  }
  
  write.csv(results_df, file = results_file, row.names = FALSE)
  
  rm(dem_multiband, unet_model, hist)
  gc()
}