## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  prompt = TRUE
)

library(docreview)

## -----------------------------------------------------------------------------
library(docreview)
pkg_path <- system.file("testpkg", package = "docreview")
package_review(path = pkg_path)

## -----------------------------------------------------------------------------
func_only_config <- system.file("configs/just_functions.yml", package = "docreview")
package_review(path = pkg_path, config = get_config(func_only_config))

## -----------------------------------------------------------------------------
stricter_thresholds <- system.file("configs/thresholds.yml", package = "docreview")
package_review(path = pkg_path, config = get_config(stricter_thresholds))

## ---- error=TRUE--------------------------------------------------------------
error <- system.file("configs/error.yml", package = "docreview")
docreview::package_review(path = pkg_path, get_config(error))

