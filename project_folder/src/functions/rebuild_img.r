# function to rebuild your image
rebuild_img <- function(pred_subsets,
                        out_path,
                        target_rst,
                        model_name) {
  subset_pixels_x <- terra::ncol(pred_subsets[1, , , ])
  subset_pixels_y <- terra::nrow(pred_subsets[1, , , ])
  tiles_rows <- terra::nrow(target_rst) %/% subset_pixels_y
  tiles_cols <- terra::ncol(target_rst) %/% subset_pixels_x
  
  # load target image to determine dimensions
  target_stars <- stars::st_as_stars(target_rst, proxy = F)
  
  # prepare subfolder for output
  result_folder <- file.path(out_path, model_name)
  if (dir.exists(result_folder)) {
    unlink(result_folder, recursive = T)
  }
  dir.create(path = result_folder)

  # for each tile, create a stars from corresponding predictions,
  # assign dimensions using original/target image, and save as tif:
  for (crow in 1:tiles_rows) {
    for (ccol in 1:tiles_cols) {
      i <- (crow - 1) * tiles_cols + (ccol - 1) + 1

      dimx <-
        c(((ccol - 1) * subset_pixels_x + 1), (ccol * subset_pixels_x))
      dimy <-
        c(((crow - 1) * subset_pixels_y + 1), (crow * subset_pixels_y))
      cstars <- stars::st_as_stars(t(pred_subsets[i, , , 1]))
      attr(cstars, "dimensions")[[2]]$delta <- -1

      # set dimensions using original raster
      st_dimensions(cstars) <-
        stars::st_dimensions(target_stars[, dimx[1]:dimx[2], dimy[1]:dimy[2]])[1:2]

      stars::write_stars(cstars, dsn = paste0(result_folder, "/_out_", i, ".tif"))
    }
  }

  starstiles <-
    as.vector(list.files(result_folder, full.names = T), mode = "character")
  
  sf::gdal_utils(
    util = "buildvrt",
    source = starstiles,
    destination = file.path(result_folder, "mosaic.vrt")
  )

  sf::gdal_utils(
    util = "warp",
    source = file.path(result_folder, "mosaic.vrt"),
    destination = file.path(result_folder, "mosaic.tif")
  )
}