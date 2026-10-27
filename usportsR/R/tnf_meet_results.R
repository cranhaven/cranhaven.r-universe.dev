#' Get U SPORTS track and field meet results
#'
#' @returns A tibble.
#' @examples
#' try(tnf_meet_results())
#' @export

tnf_meet_results <- function(){
  read_csv("https://github.com/uwaggs/usports-data/releases/download/tnf_meet_results/tnf_meet_results.csv") %>% tibble::tibble()
}

