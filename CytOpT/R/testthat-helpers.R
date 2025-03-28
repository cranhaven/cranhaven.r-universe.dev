#'@importFrom reticulate import py_available py_module_available py_numpy_available
#'@import testthat

clean_version <- function(version) {
  gsub("\\.$", "", gsub("[A-Za-z_+].*$", "", version))
}

skip_on_cran <- function() {
  testthat::skip_on_cran()
}

skip_if_no_python <- function() {

  if (!py_available(initialize = TRUE))
    testthat::skip("Python bindings not available for testing")
  
}

skip_if_no_scipy <- function() {
  
  skip_on_cran()
  skip_if_no_python()
  
  if (!py_module_available("scipy"))
    testthat::skip("scipy not available for testing")
  
  scipy <- import("scipy", delay_load = TRUE)
  if (clean_version(scipy$`__version__`) < "1.0")
    testthat::skip("scipy version is less than v1.0")
  
}


skip_if_no_numpy <- function() {
  
  skip_on_cran()
  skip_if_no_python()
  
  if (!reticulate::py_numpy_available())
    skip("NumPy not available for testing")
  
}

skip_if_no_sklearn <- function() {
  
  skip_on_cran()
  skip_if_no_python()
  
  if (!reticulate::py_module_available("sklearn"))
    skip("SkLearn not available for testing")
  
}

skip_if_notall_pythondeps <- function() {
  
  skip_on_cran()
  skip_if_no_python()
  skip_if_no_numpy()
  skip_if_no_scipy()
  skip_if_no_sklearn()

}
