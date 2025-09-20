## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(greta)

## -----------------------------------------------------------------------------
#  install_greta_deps()

## ----install_tensorflow, eval = FALSE-----------------------------------------
#  reticulate::install_miniconda()
#  reticulate::conda_create(
#          envname = "greta-env-tf2",
#          python_version = "3.10"
#        )
#  reticulate::conda_install(
#          envname = "greta-env-tf2",
#          packages = c(
#            "tensorflow-probability==0.23.0",
#            "tensorflow==2.15.0"
#          )
#        )

## ----install-deps-plain, eval = FALSE-----------------------------------------
#  reticulate::install_miniconda()
#  reticulate::conda_install(
#          packages = c(
#            "tensorflow-probability==0.23.0",
#            "tensorflow==2.15.0"
#          )
#        )

