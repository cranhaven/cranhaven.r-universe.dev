#' Get julian date from the begining of the year
#'
#' @param x date or a series of dates such as, as.Date("yyyy-mm-dd")
#'
#' @return A julian date between 1 and 365, note that in leap years the day 366 is considered as 365
#' @author Mohanad Zaghloul [aut, cre], Simon Michael Papalexiou [aut, ths], Amin Elshorbagy [aut, ths]
#' @export
#'
#' @examples
#' 
#' get_julian(x = as.Date("1979-01-15"))
#' 
get_julian <- function(x) {
  year_x <- substr(x, 1, 4)
  myjulian <- as.numeric(julian(x, origin = as.Date(paste0(as.character(year_x), "-01-01")))) + 1
  if (myjulian == 366) {myjulian <- 365}
  return(myjulian)
}
