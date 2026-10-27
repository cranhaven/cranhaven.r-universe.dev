#' Get U SPORTS field hockey schedule
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |date          | chr  |
#' |away          | chr  |
#' |away_score    | int  |
#' |home          | chr  |
#' |home_score    | int  |
#' |status        | chr  |
#' |notes         | chr  |
#' |month         | chr  |
#' |box_scores    | chr  |
#' |conference    | int  |
#' |division      | int  |
#' |exhibition    | int  |
#' |post_season   | int  |
#' |season        | chr  |
#'
#' @examples
#' \donttest{try(field_hockey_schedule())}
#' @export

field_hockey_schedule <- function() {
  read_csv("https://github.com/uwaggs/usports-data/releases/download/field_hockey_schedule/womens_fh_schedule.csv") %>% tibble::tibble()
}

