#' A function to import school district shapefiles
#'
#' This function allows you to import a simplified version of the US Census
#' Bureau, Education Demographic and Geographic Estimates Program (EDGE),
#' Composite School District Boundaries File.
#' @param data_year Four digit year of shapefile data you would like to pull.
#'   Available for any school year from 2013 to 2019.
#' @param with_data TRUE to attach EdBuild's school district master dataset to
#'   shapefile. Defaults to FALSE.
#' @keywords shapefile EdBuild
#' @import dplyr magrittr stringr sf
#' @importFrom utils read.csv
#' @usage sd_shapepull(data_year = "2019", with_data=FALSE)
#' @return A spatial object where each row is a school district.
#' @export
#' @format Simple feature collection with 6 fields and geometry Multi Polygon: \describe{
#'   \item{GEOID}{Unique school district ID, character} \item{NAME}{School
#'   district name, character string} \item{sdType}{School district level,
#'   character string} \item{FIPS}{State ID, character} \item{State}{State name,
#'   character} \item{Postal}{State postal code, character}
#'   \item{geometry}{sfc_MULTIPOLYGON} }
#' @source
#' \url{https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/SD+shapes/2019/shapefile_1819.zip}
#'
#' @examples
#' \donttest{sd_shp_19 <- sd_shapepull("2019")}


sd_shapepull = function(data_year = "2019", with_data=FALSE) {
  if (as.numeric(data_year)<2013) {
    message("Error: sd_shapepull cannot be used for data before the year 2013")
  }

  else if (as.numeric(data_year)>2019) {
    message("Error: The most recent year of school district shapefiles is for 2019; data_year > 2019 is not valid")
  }

  else {

    if (as.numeric(data_year)==2013 | as.numeric(data_year)==2014) {

      temp <- tempfile()  ### create a temporary file to download zip file to
      temp2 <- tempfile() ### create a temporary file to put unzipped files in
      download.file("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/SD+shapes/2014/shapefile_1314.zip", temp) # downloading the data into the tempfile

      unzip(zipfile = temp, exdir = temp2) # unzipping the temp file and putting unzipped data in temp2

      filename <- list.files(temp2, full.names = TRUE) # getting the filename of the downloaded data

      shp_file <- filename %>%
        subset(grepl("*.shp$", filename)) ## selecting only the .shp file to read in

      dataset <- sf::st_read(shp_file, stringsAsFactors = FALSE) %>% ## reading in the downloaded data
        dplyr::mutate(GEOID = as.character(GEOID),
               GEOID = stringr::str_pad(GEOID, width = 7, pad = "0")) %>%   ## changing to character so can join to master if that is opted in
        sf::st_set_crs(4269)  # defining crs of shapefile so that r knows what it is
      message("NOTE::This shapefile has been simplified to make analysis quicker. For final vizualizations, please use the unsimplified shapefiles available through NCES.")
    }

    else if (as.numeric(data_year)==2015 | as.numeric(data_year)==2016) {

      temp <- tempfile()  ### create a temporary file to download zip file to
      temp2 <- tempfile() ### create a temporary file to put unzipped files in
      download.file("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/SD+shapes/2016/shapefile_1516.zip", temp) # downloading the data into the tempfile

      unzip(zipfile = temp, exdir = temp2) # unzipping the temp file and putting unzipped data in temp2

      filename <- list.files(temp2, full.names = TRUE) # getting the filename of the downloaded data

      shp_file <- filename %>%
        subset(grepl("*.shp$", filename)) ## selecting on ly the .shp file to read in

      dataset <- sf::st_read(shp_file, stringsAsFactors = FALSE) %>% ## reading in the downloaded data
        dplyr::mutate(GEOID = as.character(GEOID),
               GEOID = stringr::str_pad(GEOID, width = 7, pad = "0")) %>%  ## changing to character so can join to master if that is opted in
        sf::st_set_crs(4269)  # defining crs of shapefile so that r knows what it is
      message("NOTE::This shapefile has been simplified to make analysis quicker. For final vizualizations, please use the unsimplified shapefiles available through NCES.")

    }

    else if (as.numeric(data_year)==2017 | as.numeric(data_year)==2018) {

      temp <- tempfile()  ### create a temporary file to download zip file to
      temp2 <- tempfile() ### create a temporary file to put unzipped files in
      download.file("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/SD+shapes/2018/shapefile_1718.zip", temp) # downloading the data into the tempfile

      unzip(zipfile = temp, exdir = temp2) # unzipping the temp file and putting unzipped data in temp2

      filename <- list.files(temp2, full.names = TRUE) # getting the filename of the downloaded data

      shp_file <- filename %>%
        subset(grepl("*.shp$", filename)) ## selecting only the .shp file to read in

      dataset <- sf::st_read(shp_file, stringsAsFactors = FALSE) %>% ## reading in the downloaded data
        dplyr::mutate(GEOID = as.character(GEOID),
               GEOID = stringr::str_pad(GEOID, width = 7, pad = "0")) %>%   ## changing to character so can join to master if that is opted in
        sf::st_set_crs(4269)  # defining crs of shapefile so that r knows what it is
      message("NOTE::This shapefile has been simplified to make analysis quicker. For final vizualizations, please use the unsimplified shapefiles available through NCES.")

    }

    else if (as.numeric(data_year)==2019) {

      temp <- tempfile()  ### create a temporary file to download zip file to
      temp2 <- tempfile() ### create a temporary file to put unzipped files in
      download.file("https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/SD+shapes/2019/shapefile_1819.zip", temp) # downloading the data into the tempfile

      unzip(zipfile = temp, exdir = temp2) # unzipping the temp file and putting unzipped data in temp2

      filename <- list.files(temp2, full.names = TRUE) # getting the filename of the downloaded data

      shp_file <- filename %>%
        subset(grepl("*.shp$", filename)) ## selecting only the .shp file to read in

      dataset <- sf::st_read(shp_file, stringsAsFactors = FALSE) %>% ## reading in the downloaded data
        dplyr::mutate(GEOID = as.character(GEOID),
                      GEOID = stringr::str_pad(GEOID, width = 7, "left", pad = "0")) %>%   ## changing to character so can join to master if that is opted in
        sf::st_set_crs(4269) # defining crs of shapefile so that r knows what it is
      message("NOTE::This shapefile has been simplified to make analysis quicker. For final vizualizations, please use the unsimplified shapefiles available through NCES.")

    }

    if (with_data == TRUE){
      if(as.numeric(data_year)==2013) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2013/full_data_13_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2013") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS, -Region) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                 op_shcls = dOperational_schools,
                 enroll_ccd = dEnroll_district,
                 dHaw_PI = dHawaiian_PI,
                 dAmInd_AK = dAmIndian_Aknative,
                 d2races = d2plus_races,
                 pctNW = pctNonwhite,
                 st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
       }
      else if(as.numeric(data_year)==2014) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2014/full_data_14_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2014") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS, -Region) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
       }
      else if(as.numeric(data_year)==2015) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2015/full_data_15_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2015") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS, -Region) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
      }
      else if(as.numeric(data_year)==2016) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2016/full_data_16_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2016") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS, -Region)

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
      }
      else if(as.numeric(data_year)==2017) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2017/full_data_17_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2017") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
      }
      else if(as.numeric(data_year)==2018) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2018/full_data_18_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2018") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          dplyr::mutate(frl_rate = dFRL/enroll_ccd)
      }

      else if(as.numeric(data_year)==2019) {
        url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Master/2019/full_data_19_geo_exc.csv"
        master <- read.csv(file = url, stringsAsFactors = FALSE) %>%
          dplyr::mutate(NCESID = as.character(NCESID),
                        NCESID = stringr::str_pad(NCESID, width = 7, pad = "0"),
                        year = "2019") %>%
          dplyr::select(-NAME, -State, -STATE_FIPS, -sdType) ## removing variables that duplicate with shapes

        dataset <- dataset %>%
          dplyr::left_join(master, by = c("GEOID" = "NCESID")) %>%
          dplyr::rename(urb = dUrbanicity,
                        op_shcls = dOperational_schools,
                        enroll_ccd = dEnroll_district,
                        dHaw_PI = dHawaiian_PI,
                        dAmInd_AK = dAmIndian_Aknative,
                        d2races = d2plus_races,
                        pctNW = pctNonwhite,
                        st_per_sqmi = student_per_sq_mile) %>%
          dplyr::filter(NAME != "School District Not Defined") %>%
          mutate(frl_rate = NA)
      }
    }

    else {
      dataset <- dataset %>%
        dplyr::filter(NAME != "School District Not Defined")
        return(dataset)
  }

}

}
