#' Process and summarize aerial survey DAS data
#'
#' This package contains functions designed for processing and analyzing 
#' aerial survey DAS data (AirDAS) collected using one of the following 
#' Southwest Fisheries Science Center (SWFSC) programs: 
#' PHOCOENA, SURVEY, CARETTA, or TURTLE (such as TURTLEP or TURTLE 4D).
#' Functionality includes checking AirDAS data for data entry errors, 
#' reading AirDAS data into a data frame, processing this data 
#' (extracting state and condition information for each AirDAS event), 
#' and summarizing sighting and effort information.
#'
#' @name swfscAirDAS-package
#' @aliases swfscAirDAS
#' @title Southwest Fisheries Science Center Aerial Survey DAS
#' @author Sam Woodman \email{sam.woodman@@noaa.gov}
#' @seealso \url{https://swfsc.github.io/swfscAirDAS/}
#'
#' @importFrom dplyr arrange between bind_cols bind_rows case_when distinct 
#'   everything filter full_join group_by if_else
#'   left_join mutate n right_join select slice starts_with summarise ungroup
#' @importFrom lubridate year month day tz
#' @importFrom magrittr %>%
#' @importFrom methods setOldClass
#' @importFrom parallel clusterExport detectCores parLapplyLB stopCluster
#' @importFrom purrr pmap_lgl
#' @importFrom readr cols col_character read_fwf fwf_positions
#' @importFrom rlang !! .data na_lgl
#' @importFrom stats na.omit runif
#' @importFrom stringr str_count str_detect str_match_all str_split
#' @importFrom swfscDAS das_effort_sight distance_greatcircle 
#'   .chop_condition_eff .chop_equallength_eff .dist_from_prev  
#'   .process_chr .process_num .segdata_aggr .segdata_proc das_effort_sight
#' @importFrom swfscMisc bearing destination distance setupClusters
#' @importFrom tidyr unnest
#' @importFrom utils head tail read.csv write.csv 
#' 
#' @keywords package
"_PACKAGE"

setOldClass("airdas_dfr")
setOldClass("airdas_df")
