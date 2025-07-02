#' Try to guess the ARU type from a file path
#'
#' @param path Character. Path to wave file
#'
#' @return Tibble with columns 'manufacturer', 'model', and 'aru_type'
#' @export
#'
#' @examples
#'
#' get_pattern("pattern_aru_type")
#'
#' guess_ARU_type("/path/to/barlt/file.wav")
#'
#' guess_ARU_type("/path/to/sm/S4A2342.wav")
guess_ARU_type <- function(path){
   model_guess <- extract_replace(path, get_pattern("pattern_aru_type"))
  company_guess <- dplyr::case_when(
    is.na(model_guess)~NA_character_,
    model_guess=="BAR-LT"~"Frontier Labs",
    stringr::str_detect(model_guess,
                        "Song Meter")~"Wildlife Acoustics",
    TRUE~NA_character_)

  return(dplyr::tibble(manufacturer = company_guess,
                       model = model_guess,
                       aru_type = stringr::str_remove_all(model_guess, "-|\\s|\\d")))
}
