#' Get USPORTS football schedule
#'
#' @returns
#' |Column Name  | Type      |
#' |:------------|:----------|
#' |league       | chr       |
#' |season       | chr       |
#' |game_id      | chr       |
#' |date         | Date      |
#' |exhibition   | lgl       |
#' |home_team    | chr       |
#' |away_team    | chr       |
#' |home_score   | int       |
#' |away_score   | int       |
#' |game_url     | chr       |
#'
#' @examples
#' \donttest{try(football_schedule())}
#' @export

football_schedule <- function() {
  read_csv("https://github.com/uwaggs/usports-data/releases/download/new_schedules/fball_schedule.csv") %>% tibble::tibble()
}
