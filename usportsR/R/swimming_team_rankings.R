#' Get U SPORTS swimming team rankings
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |Rank          | int  |
#' |Team          | chr  |
#' |Count         | int  |
#' |Gender        | chr  |
#' |Date          | chr  |
#'
#' @examples
#' try(swimming_team_rankings())
#' @export
swimming_team_rankings <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/swimming_team_rankings/swimming_team_rankings.csv") %>% tibble::tibble()
}


