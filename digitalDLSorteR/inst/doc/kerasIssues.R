## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## ----eval = FALSE-------------------------------------------------------------
#  installTFpython(install.conda = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  reticulate::install_miniconda()
#  reticulate::conda_create(
#    envname = "digitaldlsorter-env",
#    packages = "python==3.7.11"
#  )

## ----eval = FALSE-------------------------------------------------------------
#  tensorflow::install_tensorflow(
#    method = "conda",
#    conda = reticulate::conda_binary("auto"),
#    envname = "digitaldlsorter-env",
#    version = "2.6.0-cpu"
#  )

## -----------------------------------------------------------------------------
#  tensorflow::use_condaenv("digitaldlsorter-env")

