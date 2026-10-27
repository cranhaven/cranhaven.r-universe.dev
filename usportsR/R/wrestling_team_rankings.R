#' Get U SPORTS wrestling team rankings
#'
#' @param gender Gender of athletes "m" or "w"
#' @returns A tibble.
#' @examples
#' try(wrestling_team_rankings("m"))
#' try(wrestling_team_rankings("w"))
#' @export

wrestling_team_rankings <- function(gender){
  if (gender == "m"){
    read_csv("https://github.com/uwaggs/usports-wrestling/releases/download/wrestling_team_rankings/mens_team.csv") %>% tibble::tibble()
  }else if(gender == "w"){
    read_csv("https://github.com/uwaggs/usports-wrestling/releases/download/wrestling_team_rankings/womens_team.csv") %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}
