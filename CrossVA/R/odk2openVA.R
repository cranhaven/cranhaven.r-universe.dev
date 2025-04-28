#' Map VA records to InterVA5 & InSilico (with option data.type = "WHO2016").
#'
#' \code{odk2openVA} transforms data collected with the 2016 WHO VA instrument
#' or the 2014 WHO VA instrument  into a format that can be used with the
#' InterVA5 and InSilicoVA alogrithms for coding cause of death. It is a
#' wrapper for functions that handle specific versions of the 2016 WHO VA
#' instrument --  namely, 1.4.1 and 1.5.1 -- as well as the 2014 WHO VA
#' instrument.  Note: versions 1.5.2 and 1.5.2 do not include changes that
#' require modification for the data preparation, so the code for version
#' 1.5.1 should also work for these later two versions.
#'
#' @param odk A dataframe, obtained from reading an ODK Briefcase
#' export of records collected with the WHO questionnaire.
#'
#' @param id_col A character string of the column name (in odk) with the
#' unique ID for each death.
#'
#' @details
#' This is a wrapper function that tries to determint the type of WHO VA
#' instrument used to collect the data.  If the input (i.e., the odk export)
#' includes a column containing the string "ID1A110" (or "id1a110"), then this
#' function assumes the data were collected using the 2014 WHO VA instrument.
#' If the input (i.e., the odk export) contains the string "age_neonate_hours"
#' and does not contain the string "ID1A110" (or "id1a110"), then the function
#' assumes the questionnaire version is 1.4.1 from the 2016 instrument.  If
#' neither condition is met, then the function assumes the data were collected
#' with version 1.5.1 of the 2016 WHO VA instrument.  Note: versions 1.5.2 and
#' 1.5.2 do not include changes that require modification for the data preparation,
#' so the code for version 1.5.1 should also work for these later two versions.
#'
#' By default, this function assumes the data column named 'meta.instanceID'
#' contains the unique ID for each record.  If this column is not found, then
#' the ID is set to 1:nrow(odk).  Alternatively, the user may specify the
#' column name containing the ID by passing a string to the id_col parameter.
#' 
#' @examples
#' ## Example with 2016 WHO VA instrument version 1.5.1
#' record_f_name151 <- system.file("sample", "who151_odk_export.csv", package = "CrossVA")
#' records151 <- read.csv(record_f_name151, stringsAsFactors = FALSE)
#' output151 <- odk2openVA(records151)
#'
#' ## Example with 2016 WHO VA instrument version 1.4.1
#' record_f_name141 <- system.file("sample", "who141_odk_export.csv", package = "CrossVA")
#' records141 <- read.csv(record_f_name141, stringsAsFactors = FALSE)
#' output141 <- odk2openVA(records141)
#'
#' @export
#'
odk2openVA <- function (odk, id_col = "meta.instanceID") {

    id1A110 <- grep( "id1a110", tolower(names(odk)) ) # check for 2014 instrument
    version2014 <- length(id1A110) == 1

    hasAgeNeonateHours <- grep( "age_neonate_hours", tolower(names(odk)) ) # check 2016 version
    version2016_151 <- length(hasAgeNeonateHours) == 0

    version2016_141 <- !version2014 & !version2016_151

    if ( version2014 ) {
        cat( paste("Assuming 2014 WHO questionnaire", "\n", sep = "") )
        return( odk2openVA_2014(odk, id_col = id_col) )
    }

    if ( version2016_151 ) {
        cat( paste("Assuming 2016 WHO questionnaire version is 1.5.1", "\n", sep = "") )
        return( odk2openVA_v151(odk, id_col = id_col) )
    }

    if ( version2016_141 ) {
        cat( paste("Assuming 2016 WHO questionnaire version is 1.4.1", "\n", sep = "") )
        return( odk2openVA_v141(odk, id_col = id_col) )
    }

}
