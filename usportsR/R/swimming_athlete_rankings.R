#' Get U SPORTS swimming athletes rankings
#'
#' @returns
#' |Column Name        | Type |
#' |:------------------|:-----|
#' |Season             | chr  |
#' |Gender             | chr  |
#' |Rank               | int  |
#' |Athlete.University | chr  |
#' |Age                | chr  |
#' |Team               | chr  |
#' |Conference         | chr  |
#' |Date               | chr  |
#' |Meet               | chr  |
#' |Time               | chr  |
#' |FINA               | int  |
#' |Event              | chr  |
#' |Date.Collected     | chr  |
#'
#' @examples
#' try(swimming_athlete_rankings())
#' @export
swimming_athlete_rankings <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/swimming_athlete_rankings/swimming_athlete_rankings.csv") %>% tibble::tibble()
}

