subset_ds <- function(
    input_raster,
    model_input_shape,
    mask = FALSE,
    path,
    targetname = "",
    normalize = TRUE) {
  
  targetSizeX <- model_input_shape[1]
  targetSizeY <- model_input_shape[2]
  inputX <- terra::ncol(input_raster)
  inputY <- terra::nrow(input_raster)
  
  # Calculate difference between input and target size
  diffX <- inputX %% targetSizeX
  diffY <- inputY %% targetSizeY
  
  # Determine new dimensions of raster and crop
  # Cut evenly on all sides if possible
  newXmin <- terra::ext(input_raster)[1] + ceiling(diffX / 2) * terra::res(input_raster)[1]
  newXmax <- terra::ext(input_raster)[2] - floor(diffX / 2) * terra::res(input_raster)[1]
  newYmin <- terra::ext(input_raster)[3] + ceiling(diffY / 2) * terra::res(input_raster)[2]
  newYmax <- terra::ext(input_raster)[4] - floor(diffY / 2) * terra::res(input_raster)[2]
  
  rst_cropped <- terra::crop(
    input_raster,
    terra::ext(newXmin, newXmax, newYmin, newYmax)
  )
  
  # Create grid for splitting
  agg <- terra::aggregate(
    rst_cropped[[1]],
    c(targetSizeX, targetSizeY)
  )
  agg[] <- 1:terra::ncell(agg)
  agg_poly <- terra::as.polygons(agg)
  names(agg_poly) <- "polis"
  
  # Split raster into tiles and save
  lapply(seq_along(agg_poly), FUN = function(i) {
    subs <- local({
      e1 <- terra::ext(agg_poly[agg_poly$polis == i, ])
      subs <- terra::crop(rst_cropped, e1)
      
      # ==================== NORMALIZATION SECTION ====================
      if(mask == FALSE && normalize == TRUE) {
        # For DEM data (multi-band): Min-Max normalization per band
        subs_array <- terra::as.array(subs)
        
        # Normalize each band individually to [0, 1] range
        for(b in 1:dim(subs_array)[3]) {
          band_data <- subs_array[, , b]
          band_min <- min(band_data, na.rm = TRUE)
          band_max <- max(band_data, na.rm = TRUE)
          
          # Avoid division by zero
          if(band_max > band_min) {
            subs_array[, , b] <- (band_data - band_min) / (band_max - band_min)
          } else {
            subs_array[, , b] <- 0
          }
        }
        
        # Convert back to SpatRaster for PNG storage
        subs <- terra::rast(subs_array, 
                            extent = terra::ext(subs),
                            crs = terra::crs(subs))
      } else if(mask == TRUE) {
        # For mask data: already in [0, 1] range
        # But ensure values are clamped to [0, 1] range
        subs <- terra::clamp(subs, 0, 1)
      }
      
      return(subs)
    })
    
    png::writePNG(
      terra::as.array(subs),
      target = file.path(path, paste0(targetname, i, ".png"))
    )
  })
  
  # Free memory
  rm(agg, agg_poly)
  gc()
  
  return(rst_cropped)
}