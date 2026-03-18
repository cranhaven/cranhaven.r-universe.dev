#' Example length-at-age data
#'
#' Simple age-structured population data with age and length records for each
#' individual. \code{laa_data} represents a well-sampled age-length dataset,
#' whereas \code{laa_data_low_n} is one with few total samples,
#' \code{laa_data_low_age_n} is one with few samples in some ages,
#' and \code{laa_data_few_ages} is a dataset with few age groups sampled.
#' Species specific datasets are similar, but with the prefix \code{laa_}
#' replaced by \code{spp_}. These datasets contain species specific
#' length-at-age data
#'
#' @format ## `laa_data`
#' A data.frame with 244 rows and 2 columns:
#' \describe{
#'   \item{spp}{Species, only applicable for spp_data_* data.frames}
#'   \item{age}{Age of individual}
#'   \item{length}{Length of individual (arbitrary units)}
#' }
#' @name laa_data
#'
"laa_data"

#' @rdname laa_data
#' @format ## `laa_data_low_n`
#' A data.frame with 27 rows and 2 columns:
"laa_data_low_n"

#' @rdname laa_data
#' @format ## `laa_data_low_age_n`
#' A data.frame with 74 rows and 2 columns:
"laa_data_low_age_n"

#' @rdname laa_data
#' @format ## `laa_data_few_ages`
#' A data.frame with 49 rows and 2 columns:
"laa_data_few_ages"

#' @rdname laa_data
#' @format ## `spp_data`
#' A data.frame with 1022 rows and 3 columns:
"spp_data"

#' @rdname laa_data
#' @format ## `spp_data_low_n`
#' A data.frame with 87 rows and 3 columns:
"spp_data_low_n"

#' @rdname laa_data
#' @format ## `spp_data_low_age_n`
#' A data.frame with 160 rows and 3 columns:
"spp_data_low_age_n"

#' @rdname laa_data
#' @format ## `spp_data_few_ages`
#' A data.frame with 261 rows and 3 columns:
"spp_data_few_ages"

#' Separate species, county, waterbody example length-at-age and length data
#'
#' Simple age-structured population with age and/or length records, but
#' expanded across multiple counties and waterbodies for tests and examples
#' in \code{\link{make_halk}} used with levels.
#' 
#' @name wb_spp_data
#'
#' @format ## `wb_spp_laa_data`
#' A data.frame with 36,849 records and 5 columns
#' \describe{
#'   \item{spp}{Species}
#'   \item{county}{Arbitrary example county name}
#'   \item{waterbody}{Arbitrary example waterbody name nested within county}
#'   \item{age}{Age of individual, only in wb_spp_laa_data}
#'   \item{length}{Length of individual (arbitrary units)}
#' }
"wb_spp_laa_data"

#' @rdname wb_spp_data
"wb_spp_length_data"

#' Example length data
#'
#' Simple vector and data.frame containing length measurements. These are used
#' in examples for functions that assign ages.
#'
#' @name length_data
#'
#' @format ## length data
#' A data.frame with one column and 244 rows
#' \describe{
#'   \item{spp}{Species, only in spp_length_data}
#'   \item{length}{Length of individual (arbitrary units)}
#' }
"length_data"


#' @rdname length_data
#' @format ## `spp_length_data`
#' A data.frame with 1022 rows and 2 columns:
"spp_length_data"
