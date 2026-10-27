#' Get U SPORTS XC meet results
#'
#' @returns A tibble.
#' @examples
#' try(xc_meet_results())
#' @export

xc_meet_results <- function(){
  rankings <- read_csv("https://github.com/uwaggs/usports-data/releases/download/xc_meet_results/xc_meet_results.csv") %>% tibble::tibble()
  rankings
}


