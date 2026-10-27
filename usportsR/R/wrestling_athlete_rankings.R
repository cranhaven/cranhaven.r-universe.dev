#' Get U SPORTS wrestling athletes rankings
#'
#' @param gender Gender of athletes "m" or "w"
#' @param weight weight class
#' @returns A tibble.
#' @examples
#' try(wrestling_athlete_rankings("m",90))
#' try(wrestling_athlete_rankings("w",54))
#' @export

wrestling_athlete_rankings <- function(gender,weight){
  if (gender == "m"){
    df <- read_csv("https://github.com/uwaggs/usports-wrestling/releases/download/wrestling_athlete_rankings/mens_athlete.csv")
    df <- df[df$Weight.Category == paste(as.character(weight),"kg",sep = ""),] %>% tibble::tibble()
    df
  }else if(gender == "w"){
    df <- read_csv("https://github.com/uwaggs/usports-wrestling/releases/download/wrestling_athlete_rankings/womens_athlete.csv")
    df <- df[df$Weight.Category == paste(as.character(weight),"kg",sep = ""),] %>% tibble::tibble()
    df
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}
