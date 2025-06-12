## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE, echo = TRUE------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  install_github("ropensci/ramlegacy")

## ---- download_ramlegacy_example1, eval=FALSE, echo = T------------------
#  # download version 1.0
#  download_ramlegacy(version = "1.0")
#  
#  # download version 2.0
#  download_ramlegacy(version = "2.0")
#  
#  # download version 2.5
#  download_ramlegacy(version = "2.5")
#  
#  # download version 3.0
#  download_ramlegacy(version = "3.0")
#  
#  # download version 4.3
#  download_ramlegacy(version = "4.3")
#  
#  # download version 4.44
#  download_ramlegacy(version "4.44")

## ---- download_ramlegacy_example2, echo = T, eval = F--------------------
#  # downloads latest version (currently 4.44)
#  download_ramlegacy()

## ---- load_ramlegacy_example1, echo = T, eval = F------------------------
#  # returns a list containing area and bioparams tables from version 4.44 database
#  load_ramlegacy(version = "4.44", tables = c("area", "bioparams"))

## ---- ram_dir_example1, echo = T, eval = F-------------------------------
#  # view the path where version 4.3 of the database was cached
#  ram_dir(vers = "4.3")

