## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,comment = "#>")

## ----libraries, message=FALSE-------------------------------------------------
#devtools::install_github("NCEAS/metajam")
library(metajam)  

# For wrangling the data
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

## ----constants----------------------------------------------------------------
# Download the data from DataONE on your local machine
data_folder <- "Data_SEC"

# Ammonium to Ammoniacal-nitrogen conversion. We will use this conversion later.
coeff_conv_NH4_to_NH4N <- 0.7764676534


## ----download, eval=FALSE-----------------------------------------------------
#  # Create the local directory to store datasets
#  dir.create(data_folder, showWarnings = FALSE)
#  
#  # Get the datasets unique identifiers
#  test_datasets_listing <- readr::read_csv(system.file("extdata", "LTER-SEC_DatasetsListing_SearchedData.csv", package = "metajam"))
#  
#  # Keep only the LUQ related datasets
#  luq_test_datasets <- test_datasets_listing %>%
#    dplyr::filter(grepl("LUQ", .$`LTER site abbreviation`)) %>%
#    dplyr::select(`LTER site abbreviation`,
#           `Data Repository (PASTA) URL to Archive/Metadata`,
#           `Data Repository (PASTA) URL to File`,
#           `Data Repository (PASTA) Filename`) %>%
#    na.omit() %>%
#    dplyr::arrange(`Data Repository (PASTA) Filename`) # sort the data sets alphabetically
#  
#  ## Batch download the datasets
#  
#  # the tidiest way
#  local_datasets <- purrr::map(.x = luq_test_datasets$`Data Repository (PASTA) URL to File`,
#                               .f = ~ download_d1_data(.x, data_folder))
#  
#  # the apply way
#  # local_datasets <- lapply(luq_test_datasets$`Data Repository (PASTA) URL to File`, download_d1_data, data_folder)
#  
#  # the map way
#  # local_datasets <- map(luq_test_datasets$`Data Repository (PASTA) URL to File`, function(x) {download_d1_data(x, data_folder)})
#  
#  

## ----read_data, eval=FALSE----------------------------------------------------
#  # You could list the datasets dowloaded in the `Data_SEC` folder
#  # local_datasets <- dir(data_folder, full.names = TRUE)
#  
#  # or you can directly use the outputed paths from download_d1_data
#  # Read all the datasets and their associated metadata in as a named list
#  luq_datasets <- purrr::map(local_datasets, read_d1_files) %>%
#    purrr::set_names(purrr::map(., ~.x$summary_metadata$value[.x$summary_metadata$name == "File_Name"]))
#  

## ----attributes, eval=FALSE---------------------------------------------------
#  # list all the attributes
#  attributes_luq <- luq_datasets %>% purrr::map("data") %>% purrr::map(colnames)
#  
#  # Check if they are identical by comparing all against the first site
#  for(ds in names(attributes_luq)) {
#    print(identical(attributes_luq[[1]], attributes_luq[[ds]]))
#  }
#  
#  #> => We are good, same data structure across the sampling sites

## ----units, eval=FALSE--------------------------------------------------------
#  # List all the units used
#  luq_units <- luq_datasets %>% purrr::map("attribute_metadata") %>% purrr::map(~.[["unit"]])
#  
#  # Check if they are identical by comparing all against the first site
#  for(us in names(luq_units)) {
#    print(identical(luq_units[[1]], luq_units[[us]]))
#  }
#  
#  #>!!! => The 2 last datasets have different units!!!!!!!!!!
#  
#  # Let's check the differences
#  luq_units_merged <- luq_datasets %>%
#    purrr::map("attribute_metadata") %>%
#    purrr::map(. %>% select(attributeName, unit)) %>%
#    purrr::reduce(full_join, by = "attributeName")
#  
#  ## Rename
#  # Create the new names
#  luq_new_colnames <- names(luq_units) %>%
#    stringr::str_split("[.]") %>%
#    purrr::map(~.[1]) %>%
#    paste("unit", ., sep = "_")
#  
#  # Apply the new names
#  colnames(luq_units_merged) <- c("attributeName", luq_new_colnames)
#  

## ----fixing_units, eval=FALSE-------------------------------------------------
#  # fix attribute naming discrepancies -- to be improved
#  # Copy the units for Gage height
#  luq_units_merged <- luq_units_merged %>%
#    dplyr::mutate(unit_RioIcacos = ifelse(test = attributeName == "Gage_Ht",
#                                          yes = "foot", no = unit_RioIcacos),
#                  unit_RioMameyesPuenteRoto = ifelse(test = attributeName == "Gage_Ht",
#                                                     yes = "foot", no = unit_RioMameyesPuenteRoto))
#  
#  
#  # Copy the units for NH4
#  luq_units_merged <- luq_units_merged %>%
#    dplyr::mutate(unit_RioIcacos = ifelse(test = attributeName == "NH4-N",
#                                          yes = "microgramsPerLiter", no = unit_RioIcacos),
#                  unit_RioMameyesPuenteRoto = ifelse(test = attributeName == "NH4-N",
#                                                     yes = "microgramsPerLiter",
#                                                     no = unit_RioMameyesPuenteRoto))
#  
#  # drop the 2 last rows
#  luq_units_merged <- head(luq_units_merged, -2)
#  
#  ### Implement the unit conversion for RioIcacos and RioMameyesPuenteRoto ----
#  
#  # Simplify naming
#  RioIcacos_data <- luq_datasets$RioIcacos$data
#  RioIcacos_attrmeta <- luq_datasets$RioIcacos$attribute_metadata
#  
#  
#  ## RioIcacos
#  # Fix NAs. In this dataset "-9999" is the missing value code. So we need to replace those with NAs
#  RioIcacos_data <- na_if(RioIcacos_data, "-9999")
#  
#  # Do the unit conversion
#  RioIcacos_data <- RioIcacos_data %>%
#    dplyr::mutate( `Gage_Ht` = `Gage_Ht`* 0.3048)
#  
#  # Update the units column accordingly
#  RioIcacos_attrmeta <- RioIcacos_attrmeta %>%
#    dplyr::mutate(unit = gsub(pattern = "foot", replacement = "meter", x = unit))
#  
#  # Do the unit conversion for RioIcacos and RioMameyesPuenteRoto - NH4 to NH4-N
#  
#  # Ammonium to Ammoniacal-nitrogen conversion
#  coeff_conv_NH4_to_NH4N <- 0.7764676534
#  
#  # Unit conversion for RioIcacos and RioMameyesPuenteRoto - NH4 to NH4-N
#  RioIcacos_data <- RioIcacos_data %>% mutate( `NH4-N` = `NH4-N`* coeff_conv_NH4_to_NH4N)
#  
#  # Update the main object
#  luq_datasets$RioIcacos$data <- RioIcacos_data
#  
#  ## RioMameyesPuenteRoto
#  
#  # Simplify naming
#  RioMameyesPuenteRoto_data <- luq_datasets$RioMameyesPuenteRoto$data
#  RioMameyesPuenteRoto_attrmeta <- luq_datasets$RioMameyesPuenteRoto$attribute_metadata
#  
#  #Replace all cells with the missing value code ("-9999") with "NA"
#  RioMameyesPuenteRoto_data <- na_if(RioMameyesPuenteRoto_data, "-9999")
#  
#  #Tidy version of unit conversion
#  RioMameyesPuenteRoto_data <- RioMameyesPuenteRoto_data %>%
#    dplyr::mutate(`Gage_Ht` = `Gage_Ht`* 0.3048)
#  
#  # Update the units column accordingly
#  RioMameyesPuenteRoto_attrmeta <- RioMameyesPuenteRoto_attrmeta %>%
#    dplyr::mutate(unit = gsub(pattern = "foot", replacement = "meter", x = unit))
#  
#  # Do the unit conversion for RioMameyesPuenteRoto - NH4 to NH4-N
#  
#  #In this dataset the NH4-N column is actually empty, so this is not necessary. But here is how you would do it if you had to.
#  
#  RioMameyesPuenteRoto_data <- RioMameyesPuenteRoto_data %>%
#    dplyr::mutate( `NH4-N` = `NH4-N`* coeff_conv_NH4_to_NH4N)
#  
#  # Update the main object
#  luq_datasets$RioMameyesPuenteRoto$data <- RioMameyesPuenteRoto_data

## ----combine, eval=FALSE------------------------------------------------------
#  # bind the sampling sites data into one master dataset for LUQ
#  all_sites_luq <- luq_datasets %>%
#    purrr::map("data") %>%
#    dplyr::bind_rows(.id = "prov")
#  
#  # Replace -9999 with NAs
#  all_sites_luq <- na_if(all_sites_luq, "-9999")
#  
#  # Write as csv
#  write_csv(all_sites_luq, "stream_chem_all_LUQ.csv")

