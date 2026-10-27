#' Get U SPORTS XC Teams
#'
#' @returns A tibble.
#' @examples
#' try(xc_universities())
#' @export

xc_universities <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/xc_universities/xc_universities.csv") %>% tibble::tibble()
}


