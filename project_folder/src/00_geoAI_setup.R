library(reticulate)
assignInNamespace("is_conda_python", function(x){ return(FALSE) }, ns="reticulate")
reticulate::use_python("/opt/homebrew/Cellar/micromamba/2.3.3_1/envs/geoai_metal/bin/python")
py_config()

# list of packages to load
packagesToLoad <- c(
  "terra",
  "png",
  "tensorflow", 
  "keras",      
  "tfdatasets", 
  "reticulate",
  "sf",
  "osmdata",
  "rsample",
  "tfdatasets",
  "purrr",
  "stars",
  "magick",
  "fs"
)


# mandantory folder structure
projectDirList <- c(
  "data/",
  "data/modelling/",
  "data/aoi/",
  "data/raw/",
  "data/raw/dgm1_ni/",
  "data/raw/dgm1_st/",
  "data/raw/dgm1_th/",
  "data/dgm1/",
  "data/modelling/model_training_data/",
  "data/modelling/model_training_data/dop/",
  "data/modelling/model_training_data/bui/",
  "data/modelling/models/",
  "data/modelling/prediction/",
  "data/modelling/validation/",
  "data/modelling/model_testing_data/",
  "data/modelling/model_testing_data/dop/",
  "data/modelling/model_testing_data/bui/",
  "docs/",
  "run/",
  "tmp",
  "src/",
  "src/functions/"
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

