## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NEONiso)

## ----eval = FALSE-------------------------------------------------------------
# manage_local_EC_archive(file_dir = "~/Desktop",
#                         get = TRUE,
#                         unzip_files = TRUE,
#                         sites = "ONAQ")

## ----eval = FALSE-------------------------------------------------------------
# for (i in seq_along(fnames_out)) {
#   calibrate_carbon_bymonth(fnames[i],
#                            fnames_out[i],
#                            site = site_code[i],
#                            method = "Bowling_2003")
# }

## ----eval = FALSE-------------------------------------------------------------
# data_dir <- "/your/path/here/DP4_00200_001/"
# 
# fnames <- list.files(path = data_dir,
#                      pattern = ".h5",
#                      recursive = TRUE,
#                      full.names = TRUE)
# 
# # unselect gz files.
# fnames <- fnames[!grepl(".gz", fnames)]
# 
# fname_byfolder <- strsplit(fnames, split = ".", fixed = TRUE)
# site_code  <- sapply(fname_byfolder, "[[", 3)
# 
# # inspect site.code in the environment: is it a vector with repeated "ONAQ"?
# fnames_tmp <- gsub(".h5", ".calibrated.h5", fnames)
# fnames_spt <- strsplit(fnames_tmp, split = "/")
# fnames_out <- sapply(fnames_spt, "[[", 7)
# 
# # create new output directory
# outpaths   <- paste0(your_path, "/ONAQ/output/")
# # apply function used here to generalize in case you wanted to run all sites
# sapply(unique(outpaths), dir.create, showWarnings = FALSE)
# 
# # update fnames.out to include desired output paths.
# fnames_out <- paste0(outpaths, "/", fnames_out)

