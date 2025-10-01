#----- LogMARcorrect ----
#######################--

#' Print size correction for non-standard viewing distance
#'
#' The logMAR scale allows simple conversion of print size between different viewing distances.
#' When the MNREAD test is not run at the standard distance (ie. 40 cm - 16 inches),
#' the angular print size (in logMAR) must be adjusted to compensate for the change in viewing distance.
#' This function allows to correct the print size accordingly to the viewing distance used for testing.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#'
#' @return The function returns the original dataframe with an added variable called "correct_ps" that contains corrected print size values (in logMAR).
#'
#' @examples # inspect the strucutre of the dataframe
#' @examples head(data_low_vision, 10)
#'
#' @examples # run the correction
#' @examples data_low_vision_new <- logMARcorrect(data_low_vision, ps, vd)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_new, 10)
#'
#' @import dplyr
#'
#' @export
logMARcorrect <- function(data, print_size, viewing_distance) {

  print_size <- enquo(print_size) # each column name needs to be redefined with enquo()
  viewing_distance <- enquo(viewing_distance) # each column name needs to be redefined with enquo()
  correct_ps <- NULL

  data %>% mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) # each column name needs to be encapsulated in (!!)
}

