## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  
  collapse = TRUE,
  comment = "#>",
  message = FALSE, 
  warning = FALSE,
  fig.height = 7, 
  fig.width = 7,
  dpi = 75
)

## ----eval = FALSE-------------------------------------------------------------
# # install from CRAN
# install.packages("geofi")
# 
# # Install development version from GitHub
# remotes::install_github("ropengov/geofi")

## ----include = FALSE, eval = TRUE---------------------------------------------
# Let's first create a function that checks if the suggested 
# packages are available
check_namespaces <- function(pkgs){
  return(all(unlist(sapply(pkgs, requireNamespace,quietly = TRUE))))
}
apiacc <- geofi::check_api_access()
pkginst <- check_namespaces(c("dplyr","tidyr","janitor","ggplot2"))
apiacc_pkginst <- all(apiacc,pkginst)

## ----municipality_map, eval = apiacc_pkginst----------------------------------
library(geofi)
muni <- get_municipalities(year = 2023)

library(dplyr)
sotkadata_swedish_speaking_pop <- geofi::sotkadata_swedish_speaking_pop

## ----bind_data, eval = apiacc_pkginst-----------------------------------------
map_data <- right_join(muni, 
                       sotkadata_swedish_speaking_pop, 
                       by = c("municipality_code" = "municipality_code"))

## ----plot1, fig.width = 10, fig.height = 7, eval = apiacc_pkginst-------------

library(ggplot2)
map_data %>% 
  ggplot(aes(fill = primary.value)) + 
  geom_sf() + 
  labs(title = unique(sotkadata_swedish_speaking_pop$indicator.title.fi)) +
  theme(legend.position = "top")

## ----zipcode_with_statistics_finland, eval = apiacc_pkginst-------------------
statfi_zipcode_population <- geofi::statfi_zipcode_population

## ----get_zipcodes, eval = apiacc_pkginst--------------------------------------
# Lets join with spatial data and plot the area of each zipcode
zipcodes19 <- get_zipcodes(year = 2019) 
zipcodes_map <- left_join(zipcodes19, 
                          statfi_zipcode_population)
ggplot(zipcodes_map) + 
  geom_sf(aes(fill = X2022), 
          color  = alpha("white", 1/3)) +
  labs(title = "Total number of inhabitants, 2022", 
       fill = NULL)

