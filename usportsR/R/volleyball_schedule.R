#' Get U SPORTS volleyball schedule
#'
#' @param gender Gender of athletes "m" or "w"
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
#' \donttest{try(volleyball_schedule("m"))}
#' @export

volleyball_schedule <- function(gender) {
  if (gender == "m"){
    read_csv("https://github.com/uwaggs/usports-data/releases/download/new_schedules/mvball_schedule.csv") %>% tibble::tibble()
  }else if(gender == "w"){
    read_csv("https://github.com/uwaggs/usports-data/releases/download/new_schedules/wvball_schedule.csv") %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}
