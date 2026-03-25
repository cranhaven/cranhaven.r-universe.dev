#' @name organic
#' @title Meta-analysis dataset on comparison of organic to conventional crop systems.
#' @description
#' The data set consists of the data collected from litterature for a meta-analysis 
#' to compare crop yield in organic system to conventionnal system conducted by Seufert et al (2012).
#' It contains 65 studies. Each study contains values of yield to organic and conventionnal system compared in field experiment.
#' The studies cover different geographical places, years and crops.
#' @docType data
#' @usage organic
#' @format a \code{RangedData} instance, 1 row per measurement. Study, Crop_species, Country, Y_coord, X_coord, Continent, Developed, Development, Latitude, Crop_type, Perennial, Legume, Org_N_input, Conv_N_input, Org_fertilizer_type, Green_manure_org, Animal_manure_org, Irrigation, Moisture, Soil_carbon, Soil_pH, Yield_conv, SD_conv, N_conv, Yield_org, SD_org, N_org, Yield_unit, lnR, Var_lnR
#' @source Seufert et al (2012), real data extracted from published papers
#' Seufert, V., N. Ramankutty, and J.A. Foley. 2012. Comparing the yields of organic and conventional agriculture. Nature 485: 229-232. 
#' @examples
#' summary(organic)
NULL
