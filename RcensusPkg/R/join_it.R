#' @title join_it
#'
#' @description Outer join two dataframes that have a common column variable.
#'
#' Function uses fast \code{data.table} techniques to join two data.tables
#'   by their common key values.  Examples might include using the "GEOID" variable
#'   as a key to join data from \code{RcensusPkg::get_vintage_data()} with a
#'   simple feature with its geometries for counties, states, countries for example
#'   from \code{RcensusPkg::tiger_*_sf()}. The resulting dataframe could then display the
#'   geometries (with \code{RplotterPkg::create_sf_plot()}) with an aesthetic mapping
#'   (e.g. fill/color/size) with a joined data column. Joining could also take place
#'   between two simple features (created by \code{RcensusPkg::tiger_*_sf()}) or between
#'   two dataframes (created by \code{RcensusPkg::get_vintage_data()}).
#'
#'   The important thing to remember is that all the rows in 'df_2' will
#'   be present in the resultant \code{data.table}.
#'
#' @param df_1 The first dataframe to be joined.
#' @param df_2 The second dataframe to be joined with 'df_1'. All rows in
#'   this dataframe will be present in the resultant dataframe.
#' @param key_1 A string that names the column from 'df_1' that is common to 'df_2'.
#' @param key_2 A string that names the column from 'df_2' that is common to 'df_1'.
#' @param negate An optional logical which if \code{TRUE} will return a dataframe
#'   that has rows in 'df_1' but not in 'df_2'.
#' @param match An optional logical which if \code{TRUE} will return a dataframe
#'   that has rows where only both 'df_1' and 'df_2' have matches.
#' @param return_sf An optional logical which if \code{TRUE} will convert the resultant
#'   \code{data.table} to a simple feature if it has a geometries column.
#' @param na_rm An optional logical which if \code{TRUE} then remove rows
#'   with NA values. The default is \code{FALSE}.
#'
#' @return A \code{data.table} or simple feature object if 'return_sf' is \code{TRUE}.
#'
#' @examples
#' \dontrun{
#'   # Requires Census Bureau API key
#'   # Get the median household income by tract for Washington DC and join
#'   # this data with DC tract boundaries.
#'
#'   library(data.table)
#'   library(httr2)
#'   library(jsonlite)
#'   library(sf)
#'   library(usmap)
#'   library(withr)
#'   library(ggplot2)
#'   library(RcensusPkg)
#'
#'   # Get the 2020 median household income data by tract for DC
#'   dc_fips <- usmap::fips(state = "dc")
#'   dc_B19013_dt <- RcensusPkg::get_vintage_data(
#'     dataset = "acs/acs5",
#'     vintage = 2020,
#'     vars = "B19013_001E",
#'     region = "tract",
#'     regionin = paste0("state:", dc_fips)
#'   )

#'   # Get the simple feature DC tract geometries and join the data dataframe "dc_B19013_dt"
#'   output_dir <- withr::local_tempdir()
#'   if(!dir.exists(output_dir)){
#'     dir.create(output_dir)
#'   }
#'   dc_tracts_sf <- RcensusPkg::tiger_tracts_sf(
#'     state = dc_fips,
#'     output_dir = output_dir,
#'     general = TRUE,
#'     delete_files = FALSE
#'   )
#'   # Join the data with simple feature object
#'   dc_joined_sf <- RcensusPkg::join_it(
#'     df_1 = dc_B19013_dt,
#'     df_2 = dc_tracts_sf,
#'     key_1 = "GEOID",
#'     key_2 = "GEOID",
#'     return_sf = TRUE
#'   )
#' }
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setkeyv
#' @importFrom sf st_as_sf
#' @importFrom stats na.omit
#'
#' @export
join_it <- function(
  df_1 = NULL,
  df_2 = NULL,
  key_1 = NULL,
  key_2 = NULL,
  negate = FALSE,
  match = FALSE,
  return_sf = FALSE,
  na_rm = FALSE
){

  dt_1 <- data.table::as.data.table(df_1, na.rm = na_rm)
  dt_2 <- data.table::as.data.table(df_2, na.rm = na_rm)

  # set the keys
  data.table::setkeyv(dt_1, key_1)
  data.table::setkeyv(dt_2, key_2)

  result_dt <- NULL

  if(negate){
    result_dt <- dt_1[!dt_2]
  }else if(match){
    result_dt <- dt_1[dt_2, nomatch = 0]
  }else {
    result_dt <- dt_1[dt_2]
  }

  if(return_sf){
    result_dt <- sf::st_as_sf(result_dt)
  }
  if(na_rm){
    result_dt <- result_dt |>
      na.omit()
  }
  return(result_dt)
}
