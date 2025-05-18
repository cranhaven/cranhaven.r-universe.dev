#' Generating the STICS climat.txt file
#'
#' @param files_path Vector of 1 or 2 weather data files
#' @param out_dir output directory path
#' @return a generation success status (TRUE/FALSE)
#'
#' @details If 2 files are given, the file list is completed if the years
#' are not consecutive using the complete_climate_paths function
#' (for a multi-years data set)
#'
#' @examples
#' \dontrun{
#' gen_climate(c("path/to/weather.year1", "path/to/weather.year2"),
#'            "/path/to/out/dir" )
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
gen_climate <- function(files_path, out_dir) {

  # generate intermediate paths for a multi-years simulation
  # i.e. greater than 2
  files_path <- complete_climate_paths(files_path)

  # data concatenation
  climate_lines <- c()
  for (i in seq_along(files_path)) {
    climate_lines <- c(climate_lines, trimws(readLines(files_path[i])))
  }

  ret <- try(
    writeLines(text = climate_lines,
               con = file.path(out_dir, "climat.txt")
    )
  )

  if (methods::is(ret, "try-error")) {
    return(invisible(FALSE))
  }

  return(invisible(TRUE))

}


#' Generating the complete wheather files paths list
#'
#' @param files_path Vector of 1 or 2 weather data files
#'
#' @return a character vector of files paths
#'
#' @examples
#' \dontrun{
#' complete_climate_paths(
#'             c("path/to/weather.year1", "path/to/weather.year2"))
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
#'
complete_climate_paths <- function(files_path) {

  files_nb <- length(files_path)
  if (files_nb < 2) return(files_path)

  # Here we do not suppose that the file extension contains
  # the year of the data
  years <- sort(c(
    as.numeric(strsplit(basename(files_path[1]), split = "\\.")[[1]][2]),
    as.numeric(strsplit(basename(files_path[2]), split = "\\.")[[1]][2])
  ))

  years <- years[1]:years[2]

  if (length(years) < 3) {
    return(files_path)
  }

  years_chr <- as.character(years)
  file_name <- strsplit(basename(files_path[1]), split = "\\.")[[1]][1]
  files_path <- file.path(
    dirname(files_path[1]), paste0(file_name, ".", years_chr)
  )

  return(files_path)
}
