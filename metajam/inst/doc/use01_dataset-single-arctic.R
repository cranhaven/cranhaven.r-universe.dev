## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----libraries, warning=FALSE-------------------------------------------------
# devtools::install_github("NCEAS/metajam")
library(metajam)  


## ----constants----------------------------------------------------------------
# Directory to save the data set
path_folder <- "Data_polaris"

# URL to download the dataset from DataONE
data_url <- "https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3Aec704da8-f174-49db-b993-bae479cdc5d9"


## ----download, eval=FALSE-----------------------------------------------------
#  # Create the local directory to download the datasets
#  dir.create(path_folder, showWarnings = FALSE)
#  
#  # Download the dataset and associated metdata
#  data_folder <- metajam::download_d1_data(data_url, path_folder)
#  # data_folder
#  # "Data_polaris/doi_10.18739_A2KK3F__Polaris_2017_Permafrost"

## ----out.width="90%", echo=FALSE, fig.align="center", fig.cap="Local file structure of a dataset downloaded by metajam"----
knitr::include_graphics("../man/figures/metajam_v1_folder.png")

## ----read_data, eval=FALSE----------------------------------------------------
#  # Read all the datasets and their associated metadata in as a named list
#  polaris17_permafrost <- metajam::read_d1_files(data_folder)
#  

## ----out.width="90%", echo=FALSE, fig.align="center", fig.cap="Structure of the named list object containing tabular metadata and data as loaded by metajam"----
knitr::include_graphics("../man/figures/metajam_v1_named_list.png")

