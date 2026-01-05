# Corrected subset function for normalized DGM data (0-1 range)
subset_ds<- function(input_raster, model_input_shape, path, targetname = "") {
  targetSizeX <- model_input_shape[1]
  targetSizeY <- model_input_shape[2]
  inputX <- terra::ncol(input_raster)
  inputY <- terra::nrow(input_raster)
  
  diffX <- inputX %% targetSizeX
  diffY <- inputY %% targetSizeY
  
  newXmin <- terra::ext(input_raster)[1] + ceiling(diffX / 2) * terra::res(input_raster)[1]
  newXmax <- terra::ext(input_raster)[2] - floor(diffX / 2) * terra::res(input_raster)[1]
  newYmin <- terra::ext(input_raster)[3] + ceiling(diffY / 2) * terra::res(input_raster)[2]
  newYmax <- terra::ext(input_raster)[4] - floor(diffY / 2) * terra::res(input_raster)[2]
  rst_cropped <- terra::crop(input_raster, terra::ext(newXmin, newXmax, newYmin, newYmax))
  
  agg <- terra::aggregate(rst_cropped[[1]], c(targetSizeX, targetSizeY))
  agg[] <- 1:ncell(agg)
  agg_poly <- terra::as.polygons(agg)
  names(agg_poly) <- "polis"
  
  lapply(seq_along(agg_poly), FUN = function(i) {
    subs <- terra::crop(rst_cropped, terra::ext(agg_poly[agg_poly$polis == i, ]))
    
    # Convert to array
    subs_array <- terra::as.array(subs)
    
    # Check data range and convert appropriately
    data_min <- min(subs_array, na.rm = TRUE)
    data_max <- max(subs_array, na.rm = TRUE)
    
    # If data is in 0-1 range (normalized), multiply by 255 for PNG (8-bit)
    # If data is already in 0-255 range, keep as is
    if (data_max <= 1.0) {
      subs_array <- subs_array * 255
    }
    
    # Handle NAs
    subs_array[is.na(subs_array)] <- 0
    
    png::writePNG(subs_array, target = file.path(path, paste0(targetname, i, ".png")))
  })
  
  rm(agg, agg_poly); gc()
  return(rst_cropped)
}