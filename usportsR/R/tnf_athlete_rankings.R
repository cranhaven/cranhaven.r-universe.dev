#' Get U SPORTS track and field athlete rankings
#'
#' @returns A tibble.
#' @examples
#' try(tnf_athlete_rankings())
#' @export

tnf_athlete_rankings <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/tnf_athlete_rankings/tnf_athlete_rankings.csv") %>% tibble::tibble()
}

