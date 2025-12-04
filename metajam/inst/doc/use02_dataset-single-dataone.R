## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----libraries, warning=FALSE-------------------------------------------------
# devtools::install_github("NCEAS/metajam")
library(metajam)  


## ----constants----------------------------------------------------------------
# Directory to save the data set
path_folder <- "Data_coweeta"

# URL to download the dataset from DataONE
data_url <- "https://cn.dataone.org/cn/v2/resolve/https%3A%2F%2Fpasta.lternet.edu%2Fpackage%2Fdata%2Feml%2Fedi%2F858%2F1%2F15ad768241d2eeed9f0ba159c2ab8fd5"


## ----download, eval=FALSE-----------------------------------------------------
#  
#  # Create the local directory to download the datasets
#  dir.create(path_folder, showWarnings = FALSE)
#  
#  # Download the dataset and associated metdata
#  data_folder <- metajam::download_d1_data(data_url, path_folder)
#  
#  
#  

## ----out.width="90%", echo=FALSE, fig.align="center", fig.cap="Local file structure of a dataset downloaded by metajam"----
knitr::include_graphics("../man/figures/metajam_v1_folder.png")

## ----read_data, eval=FALSE----------------------------------------------------
#  # Read all the datasets and their associated metadata in as a named list
#  coweeta_diatom <- metajam::read_d1_files(data_folder)
#  

## ----libraries-2, warning=FALSE-----------------------------------------------
# devtools::install_github("NCEAS/metajam")
library(metajam)  


## ----constants-2--------------------------------------------------------------
# Directory to save the data set
path_folder <- "Data_alaska"

# URL to download the dataset from DataONE
data_url <- "https://cn.dataone.org/cn/v2/resolve/4139539e-94e7-49cc-9c7a-5f879e438b16"


## ----download-2, eval=FALSE---------------------------------------------------
#  
#  # Create the local directory to download the datasets
#  dir.create(path_folder, showWarnings = FALSE)
#  
#  # Download the dataset and associated metdata
#  data_folder <- metajam::download_d1_data(data_url, path_folder)
#  
#  
#  

## ----out.width="90%", echo=FALSE, fig.align="center", fig.cap="Local file structure of a dataset downloaded by metajam"----
knitr::include_graphics("../man/figures/metajam_v1_folder.png")

## ----read_data-2, eval=FALSE--------------------------------------------------
#  # Read all the datasets and their associated metadata in as a named list
#  coweeta_diatom <- metajam::read_d1_files(data_folder)
#  

## ----out.width="90%", echo=FALSE, fig.align="center", fig.cap="Structure of the named list object containing tabular metadata and data as loaded by metajam"----
knitr::include_graphics("../man/figures/metajam_v1_named_list.png")

