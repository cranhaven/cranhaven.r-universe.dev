#' Print a dataset of the CES survey codes and their associated calls.
#'
#' @description
#' `get_cescodes()` prints out a data frame of the CES survey codes and the associated calls.
#' Provides a quick way of looking up a CES survey code and the associated call.
#'
#' @param indx A numeric value that determines the number of survey codes returned.
#' Default is set to 22, or the total set of survey codes.
#'
#' @details
#' Items under the *Survey Code Calls* and *Index Code Calls* can be copied and used with the `get_ces()` function.
#'
#' ## Survey Codes
#' * `ces2019_web` 2019 CES online survey dataset.
#' * `ces2019_phone` 2019 CES phone survey dataset.
#' * `ces2015_web` 2015 CES online survey dataset.
#' * `ces2015_phone` 2015 CES phone survey dataset.
#' * `ces2015_combo` 2015 CES combined (online/phone) dataset.
#' * `ces2011` 2011 CES survey dataset.
#' * `ces2008` 2008 CES survey dataset.
#' * `ces2004` 2004 CES survey dataset.
#' * `ces0411` 2004-2011 CES survey dataset.
#' * `ces0406` 2004-2006 CES survey dataset.
#' * `ces2000` 2000 CES survey dataset.
#' * `ces1997` 1997 CES survey dataset.
#' * `ces1993` 1993 CES survey dataset.
#' * `ces1988` 1988 CES survey dataset.
#' * `ces1984` 1984 CES survey dataset.
#' * `ces1974` 1974 CES survey dataset.
#' * `ces7480` 1974-1980 CES survey dataset.
#' * `ces72_jnjl` 1972 June-July CES survey dataset.
#' * `ces72_sep` 1972 September CES survey dataset.
#' * `ces72_nov` 1972 November CES survey dataset.
#' * `ces1968` 1968 CES survey dataset.
#' * `ces1965` 1965 CES survey dataset.
#'
#' ## Survey Code Calls
#' * `"ces2019_web"` calls 2019 CES online survey dataset.
#' * `"ces2019_phone"` calls 2019 CES phone survey dataset.
#' * `"ces2015_web"` calls 2015 CES online survey dataset.
#' * `"ces2015_phone"` calls 2015 CES phone survey dataset.
#' * `"ces2015_combo"` calls 2015 CES combined (online/phone) dataset.
#' * `"ces2011"` calls 2011 CES survey dataset.
#' * `"ces2008"` calls 2008 CES survey dataset.
#' * `"ces2004"` calls 2004 CES survey dataset.
#' * `"ces0411"` calls 2004-2011 CES survey dataset.
#' * `"ces0406"` calls 2004-2006 CES survey dataset.
#' * `"ces2000"` calls 2000 CES survey dataset.
#' * `"ces1997"` calls 1997 CES survey dataset.
#' * `"ces1993"` calls 1993 CES survey dataset.
#' * `"ces1988"` calls 1988 CES survey dataset.
#' * `"ces1984"` calls 1984 CES survey dataset.
#' * `"ces1974"` calls 1974 CES survey dataset.
#' * `"ces7480"` calls 1974-1980 CES survey dataset.
#' * `"ces72_jnjl"` calls 1972 June-July CES survey dataset.
#' * `"ces72_sep"` calls 1972 September CES survey dataset.
#' * `"ces72_nov"` calls 1972 November CES survey dataset.
#' * `"ces1968"` calls 1968 CES survey dataset.
#' * `"ces1965"` calls 1965 CES survey dataset.
#'
#' @return A printout of a designated number of Canadian Election Study survey codes and
#' associated character codes used for the `get_ces()` function.
#'
#' @examples
#' # print out CES code calls
#' get_cescodes()
#'
#' # call 1984 CES survey
#' get_ces("ces1984")
#'
#' @seealso `get_ces()` function help.


#'@export
# get_cescodes function
# creates three vectors of the ces survey codes and associated calls
# converts those vectors to data frames with associated index number for call
# merges the three data frames and renames the columns
# removes the data frame items and prints merged results
# can be used to lookup a survey code and the associated calls.
get_cescodes <- function(indx = 22){
  ces1 <- (c("ces2019_web", "ces2019_phone", "ces2015_web", "ces2015_phone", "ces2015_combo",
                "ces2011", "ces2008", "ces2004", "ces0411", "ces0406", "ces2000", "ces1997", "ces1993",
                "ces1988", "ces1984", "ces1974", "ces7480", "ces72_jnjl", "ces72_sep", "ces72_nov",
                "ces1968", "ces1965"))
  ces2 <- c('"ces2019_web"', '"ces2019_phone"', '"ces2015_web"', '"ces2015_phone"', '"ces2015_combo"',
                '"ces2011"', '"ces2008"', '"ces2004"', '"ces0411"', '"ces0406"', '"ces2000"', '"ces1997"', '"ces1993"',
                '"ces1988"', '"ces1984"', '"ces1974"', '"ces7480"', '"ces72_jnjl"', '"ces72_sep"', '"ces72_nov"',
                '"ces1968"', '"ces1965"')
  ces1 <- data.frame(ces1)
  ces1$index <- seq.int(nrow(ces1))
  ces2 <- data.frame(ces2)
  ces2$index <- seq.int(nrow(ces2))
  ces_calltable <- merge(ces1, ces2, by = "index")
  ces_calltable <- dplyr::rename(ces_calltable, ces_survey_code = ces1, get_ces_call_char = ces2)
  rm(ces1)
  rm(ces2)
  utils::head(ces_calltable, indx)
}
