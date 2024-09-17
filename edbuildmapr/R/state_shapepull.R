#' A function to import state shapefiles
#'
#' This function allows you to import a simplified version of state shapefiles which match the the US Census
#' Bureau, Education Demographic and Geographic Estimates Program (EDGE),
#' Composite School District Boundaries File.
#' @keywords shapefile EdBuild
#' @import dplyr magrittr stringr sf
#' @usage state_shapepull()
#' @return A spatial object where each row is a state.
#' @export
#' @format Simple feature collection with 4 fields: \describe{
#'   \item{State.Post}{State postal code, character string} \item{FIPSn}{State ID, character}
#'    \item{Name}{State name, character string} \item{geometry}{sfc_MULTIPOLYGON}}
#' @source
#' \url{https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/State+Shapes/US_States_Viz_2020_high_res.zip}
#'
#' @examples
#' \donttest{state_shp <- state_shapepull()}


state_shapepull = function() {

      temp <- tempfile()  ### create a temporary file to download zip file to
      temp2 <- tempfile() ### create a temporary file to put unzipped files in
      download.file("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/State+Shapes/US_States_Viz_2020_high_res.zip", temp) # downloading the data into the tempfile

      unzip(zipfile = temp, exdir = temp2) # unzipping the temp file and putting unzipped data in temp2

      filename <- list.files(temp2, full.names = TRUE) # getting the filename of the downloaded data

      shp_file <- filename %>%
        subset(grepl("*.shp$", filename)) ## selecting only the .shp file to read in

      dataset <- sf::st_read(shp_file, stringsAsFactors = FALSE) %>% ## reading in the downloaded data
        dplyr::mutate(FIPSn = as.character(FIPSn),
                      FIPSn = stringr::str_pad(FIPSn, width = 2, pad = "0")) %>%   ## changing to character so can join to master if that is opted in
        sf::st_transform(4269)  # defining crs of shapefile so that r knows what it is
      message("NOTE::This shapefile has been simplified to make analysis quicker. For final vizualizations, please use the unsimplified shapefiles available through NCES.")

    return(dataset)
}
