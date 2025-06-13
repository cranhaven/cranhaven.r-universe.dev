# global reference (will be initialized in .onLoad)

os <- NULL
builtins <- NULL
cv2 <- NULL
imutils <- NULL
numpy <- NULL
#utilities <- NULL
#measure_dim <- NULL


.onLoad <- function(libname, pkgname) {

  #python_path <- system.file("python", package = "forImage")

  os <<- reticulate::import("os", delay_load = TRUE)
  builtins <<- reticulate::import("builtins", delay_load = TRUE)
  cv2 <<- reticulate::import("cv2", delay_load = TRUE)
  imutils <<- reticulate::import("imutils", delay_load = TRUE)
  numpy <<- reticulate::import("numpy", delay_load = TRUE)
 # utilities <<- reticulate::import_from_path("cv_utilities", path = python_path)
 # measure_dim <<- reticulate::import_from_path("measure_dim", path = python_path)

}


