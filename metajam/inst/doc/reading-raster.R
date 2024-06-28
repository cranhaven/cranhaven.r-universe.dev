## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----libraries, warning=FALSE, message=FALSE, eval=FALSE----------------------
#  # devtools::install_github("NCEAS/metajam")
#  library(metajam)
#  library(raster)
#  library(magrittr)

## ----constants, eval=FALSE----------------------------------------------------
#  # Directory to save the data set
#  path_folder <- "Human_impacts"
#  
#  # URL to download the dataset from DataONE
#  data_url <- "https://cn.dataone.org/cn/v2/resolve/urn:uuid:6f101827-2fc3-43da-8c8d-7b1f927c4c73"
#  

## ----download, eval=FALSE-----------------------------------------------------
#  # Create the local directory to download the datasets
#  dir.create(path_folder, showWarnings = FALSE)
#  
#  # Download the dataset and associated metdata
#  data_folder <- metajam::download_d1_data(data_url, path_folder)
#  # data_folder
#  # "Human_impacts/doi_10.5063_F15M63Z8__shipping__tif"

## ----read_raster, message=FALSE, eval=FALSE-----------------------------------
#  # Read the raster file and its associated metadata in as a named list
#  # using the raster:raster function
#  shipping_routes <-  read_d1_files(data_folder, "raster")
#  
#  # Plot the raster data
#  plot(shipping_routes$data)

## ----plot, out.width="90%", echo=FALSE, fig.align="center", fig.cap="Shipping routes frequency"----
knitr::include_graphics("../inst/images/shipping-raster.png")

## ----read_metadata, eval=FALSE------------------------------------------------
#  shipping_routes$summary_metadata

## ----metadata-table, out.width="100%", echo=FALSE, fig.align="center"---------
knitr::include_graphics("../inst/images/shipping-metadata.png")

