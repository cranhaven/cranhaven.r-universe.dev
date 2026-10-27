#' Get U SPORTS track and field rosters
#'
#' @returns A tibble.
#' @examples
#' try(tnf_rosters())
#' @export

tnf_rosters <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/tnf_rosters/tnf_rosters.csv") %>% tibble::tibble()
}
