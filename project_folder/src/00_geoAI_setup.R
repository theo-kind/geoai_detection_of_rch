# ================================= Notes =================================
#
# Central setup script called at the start of every pipeline script.
# Configures the Python environment, loads R packages, creates the project
# folder structure via envimaR, and sources all helper functions.
#
# Used Software versions:
#   R 4.5.2  |  Python 3.10.19  |  TensorFlow 2.15.0  |  Keras 2.15.0
#
# ================================= Set up =================================

library(reticulate)

assignInNamespace('is_conda_python', function(x){ return(FALSE) }, ns='reticulate')
reticulate::use_python("/opt/homebrew/Cellar/micromamba/2.3.3_1/envs/geoai_metal/bin/python") 
py_config()
keras::py_require_legacy_keras()


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
  'purrr',
  'stars',
  'magick',
  'fs',
  'ggplot2'
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
  'data/dem1/',
  'data/dem1/dem_tiles_5km/',
  'data/dem1/dem_tiles_5km/open_pos',
  'data/dem1/dem_tiles_5km/svf',
  'data/dem1/dem_tiles_5km/vat_4b',
  'data/dem1/dem_tiles_5km/vat_3b',
  'data/modelling_data',
  'data/modelling/model_training_data/',
  'data/modelling/model_training_data/dem/',
  'data/modelling/model_training_data/mask/',
  'data/modelling/models/',
  'data/modelling/prediction/',
  'data/modelling/prediction/chunks',
  'data/modelling/validation/',
  'data/modelling/model_testing_data/',
  'data/modelling/model_testing_data/dem/',
  'data/modelling/model_testing_data/mask/',
  'docs/',
  'docs/grid_search/',
  'docs/grid_search/test',
  'docs/grid_search/hist',
  'run/',
  'results/',
  'tmp',
  'src/',
  'src/functions/'
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


# load functions from the functions folder
lapply(
  list.files(
    file.path(envrmt$path_functions),
    pattern = "\\.[Rr]$",
    full.names = TRUE,
    recursive = TRUE
  ),
  source
)