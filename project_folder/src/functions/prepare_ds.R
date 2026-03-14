# function to prepare your data set for all further processes
prepare_ds <- function(files = NULL,
                       train,
                       predict = FALSE,
                       subsets_path = NULL,
                       batch_size = batch_size) {
  if (!predict) {
    # create a tf_dataset from the input data.frame
    # right now still containing only paths to images
    dataset <- tfdatasets::tensor_slices_dataset(files)
    
    # use tfdatasets::dataset_map to apply function on each record of the dataset
    # (each record being a list with two items: img and mask), the
    # function is purrr::list_modify, which modifies the list items
    # 'img' and 'mask' by using the results of applying decode_png on the img and the mask
    # -> i.e. pngs are loaded and placed where the paths to the files were (for each record in dataset)
    dataset <-
      tfdatasets::dataset_map(dataset, function(.x) {
        purrr::list_modify(
          .x,
          img = tf$image$decode_png(tf$io$read_file(.x$img)),
          mask = tf$image$decode_png(tf$io$read_file(.x$mask))
        )
      })
    
    # convert to float32:
    # for each record in dataset, both its list items are modified
    # by the result of applying convert_image_dtype to them
    dataset <-
      tfdatasets::dataset_map(dataset, function(.x) {
        purrr::list_modify(
          .x,
          img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
          mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
        )
      })
    
    # data augmentation performed on training set only
    if (train) {
      # augmentation 1: flip left right
      augmentation <-
        tfdatasets::dataset_map(dataset, function(.x) {
          purrr::list_modify(
            .x,
            img = tf$image$flip_left_right(.x$img),
            mask = tf$image$flip_left_right(.x$mask)
          )
        })
      
      dataset_augmented <-
        tfdatasets::dataset_concatenate(augmentation, dataset)
      
      # augmentation 2: flip up down
      augmentation <-
        tfdatasets::dataset_map(dataset, function(.x) {
          purrr::list_modify(
            .x,
            img = tf$image$flip_up_down(.x$img),
            mask = tf$image$flip_up_down(.x$mask)
          )
        })
      
      dataset_augmented <-
        tfdatasets::dataset_concatenate(augmentation, dataset_augmented)
      
      # augmentation 3: flip left right AND up down
      augmentation <-
        tfdatasets::dataset_map(dataset, function(.x) {
          purrr::list_modify(
            .x,
            img = tf$image$flip_left_right(.x$img),
            mask = tf$image$flip_left_right(.x$mask)
          )
        })
      
      augmentation <-
        tfdatasets::dataset_map(augmentation, function(.x) {
          purrr::list_modify(
            .x,
            img = tf$image$flip_up_down(.x$img),
            mask = tf$image$flip_up_down(.x$mask)
          )
        })
      
      dataset_augmented <-
        tfdatasets::dataset_concatenate(augmentation, dataset_augmented)
      
      # augmentation 4: rotation (90, 180, 270 Grad)
      augmentation <-
        tfdatasets::dataset_map(dataset, function(.x) {
          k <- tf$random$uniform(shape = list(), minval = 1L, maxval = 4L, dtype = tf$int32)
          purrr::list_modify(
            .x,
            img = tf$image$rot90(.x$img, k = k),
            mask = tf$image$rot90(.x$mask, k = k)
          )
        })
      
      dataset_augmented <-
        tfdatasets::dataset_concatenate(augmentation, dataset_augmented)
      
      # shuffling
      dataset <-
        tfdatasets::dataset_shuffle(dataset_augmented, buffer_size = batch_size * 256)
    }
    
    # train in batches; batch size might need to be adapted depending on
    # available memory
    dataset <- tfdatasets::dataset_batch(dataset, batch_size)
    
    # output needs to be unnamed
    dataset <- tfdatasets::dataset_map(dataset, unname)
  } else {
    # make sure subsets are read in in correct order
    # so that they can later be reassembled correctly
    # needs files to be named accordingly (only number)
    o <-
      order(as.numeric(tools::file_path_sans_ext(basename(
        list.files(subsets_path, pattern = "\\.png$")
      ))))
    subset_list <- list.files(subsets_path, pattern = "\\.png$", full.names = T)[o]
    
    dataset <- tfdatasets::tensor_slices_dataset(subset_list)
    
    dataset <-
      tfdatasets::dataset_map(dataset, function(.x) {
        tf$image$decode_png(tf$io$read_file(.x))
      })
    
    dataset <-
      tfdatasets::dataset_map(dataset, function(.x) {
        tf$image$convert_image_dtype(.x, dtype = tf$float32)
      })
    
    dataset <- tfdatasets::dataset_batch(dataset, batch_size)
    dataset <- tfdatasets::dataset_map(dataset, unname)
  }
}