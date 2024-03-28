## ----include = FALSE----------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, 
                      comment = "#>", 
                      width = 68)

orig_options <- options()
options(width = 68, 
        cli.unicode = FALSE, 
        cli.width = 68)

## ----eval = FALSE-------------------------------------------------
#  install.packages("abn")

## ----eval = FALSE-------------------------------------------------
#  devtools::install_github("furrer-lab/abn")

## ----include=FALSE------------------------------------------------------------
options(orig_options)

