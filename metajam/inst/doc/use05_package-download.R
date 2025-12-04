## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----libraries, warning=FALSE-------------------------------------------------
# devtools::install_github("NCEAS/metajam")
library(metajam)  
library(readr)
library(purrr)

## ----constants----------------------------------------------------------------
# Directory to save the data set
path_folder <- "./Soil_bulk"

# URL to read the search results stored as a csv on Google Drive
csv_search_results_url <- "https://drive.google.com/uc?export=download&id=1WTLP2BcXCXmUyv4kmntyhuPfrBNdPIqV"


## ----download, eval=FALSE-----------------------------------------------------
#  # Create the local directory to store data sets
#  dir.create(path_folder, showWarnings = FALSE)
#  
#  # Read the data listing from Google Drive: https://drive.google.com/open?id=1WTLP2BcXCXmUyv4kmntyhuPfrBNdPIqV
#  data_listing <- read_csv(csv_search_results_url)
#  
#  
#  ### Download the data and metadata ----
#  
#  # Create the list of unique dois
#  dois <- unique(data_listing$identifier)
#  
#  # batch download the datasets
#  data_folders <- map(dois, ~download_d1_data_pkg(.x, path_folder))

