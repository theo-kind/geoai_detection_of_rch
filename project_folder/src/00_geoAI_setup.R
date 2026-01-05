# list of packages to load
packagesToLoad <- c(
  'terra',
  'png',
  'tensorflow',
  'keras',
  'tfdatasets',
  'reticulate',
  'sf',
  'rsample',
  'tfdatasets',
  'purrr',
  'stars',
  'magick',
  'fs',
  'ggplot2'
)

# VAT-specific packages
appendPackagesToLoad <- c(
  'jsonlite',
  'yaml',
  'glue'
)

# mandantory folder structure
projectDirList <- c(
  'data/',
  'data/modelling/',
  'data/aoi/',
  'data/raw/',
  'data/raw/dgm1_ni/',
  'data/raw/dgm1_st/',
  'data/raw/dgm1_th/',
  'data/dgm1/',
  'data/dgm1/dgm_tiles/',
  'data/dgm1/processed',
  'data/dgm1/processed/VAT',
  'data/modelling/model_training_data/',
  'data/modelling/model_training_data/dgm/',
  'data/modelling/model_training_data/mask/',
  'data/modelling/models/',
  'data/modelling/prediction/',
  'data/modelling/validation/',
  'data/modelling/model_testing_data/',
  'data/modelling/model_testing_data/dgm/',
  'data/modelling/model_testing_data/mask/',
  'docs/',
  'run/',
  'tmp',
  'src/',
  'src/functions/'
)

# VAT-specific folders
appendProjectDirList <- c(
  'data/modelling/dgm_vat/',
  'data/modelling/dgm_vat/individual_layers/',
  'data/modelling/dgm_vat/metadata/'
)


# append additional folders if defined by calling script
if (exists("appendProjectDirList") && appendProjectDirList[[1]] != "") {
  projectDirList <- append(projectDirList, appendProjectDirList)
}

# append additional packages if defined by calling script
if (exists("appendPackagesToLoad") && appendPackagesToLoad[[1]] != "") {
  packagesToLoad <- append(packagesToLoad, appendPackagesToLoad)
}
library(envimaR)

# Automatically set root direcory, folder structure and load libraries
envrmt <- envimaR::createEnvi(
  root_folder = rootDir,             # if it exists, this is the root folder
  folders = projectDirList,          # mandantory folder structure
  path_prefix = "path_",             # prefix to all path variables that are created
  libs = packagesToLoad,             # list of R packages to be loaded
  alt_env_id = "COMPUTERNAME",       # check the environment variable "COMPUTERNAME"
  alt_env_value = "PCRZP",           # check if it contains the string "PCRZP" (e.g. local PC pools)
  alt_env_root_folder = "F:/BEN/edu" # use the alternative root folder
)

# set temp path to speed up terra package operations
terraOptions(tempdir = envrmt$path_tmp)


# load functions
lapply(
  list.files(
    file.path(envrmt$path_functions),
    pattern = "\\.[Rr]$",
    full.names = TRUE,
    recursive = TRUE
  ),
  source
)