#----- ReadingSpeed -----
#######################--

#' Reading speed calculation not corrected for the number of errors
#'
#' This function calculates reading speed (in words per minute) using reading time (in seconds) only.
#' This calculation provides a simplified value of reading speed, that does not take into account the number of misread words.
#'
#' @param data The name of your dataframe
#' @param reading_time The variable that contains the reading time for each sentence
#'
#' @return The function returns the original dataframe with an added variable called "reading_speed_nonCorrected" that contains reading speed (in words/min) for each sentence tested.
#'
#' @section Notes:
#' This function gives a less precise reading speed measurement than \code{\link{readingSpeed}}.
#' Unless you know what you are doing, consider using \code{\link{readingSpeed}} instead of this function.
#'
#' @seealso
#' \code{\link{readingSpeed}} for reading speed corrected for errors
#'
#' @examples # inspect the strucutre of the dataframe
#' @examples head(data_low_vision, 10)
#'
#' @examples # run the reading speed calculation
#' @examples data_low_vision_new <- readingSpeed_nonCorrected(data_low_vision, rt)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_new, 10)
#'
#' @import dplyr
#'
#' @export
readingSpeed_nonCorrected <- function(data, reading_time) {
  # This function calculates reading speed in words per minute (wpm) using reading time in seconds only.

  reading_time <- enquo(reading_time)
  reading_speed_nonCorrected <- NULL

  data %>% mutate (reading_speed_nonCorrected = 600 / (!!reading_time))
}



#' Reading speed calculation corrected for the number of errors
#'
#' This function calculates reading speed (in words per minute) for each sentence tested.
#' This calculation takes into account the number of misread words and gives a more precise reading speed measurement than \code{\link{readingSpeed_nonCorrected}}.
#'
#' @param data The name of your dataframe
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#'
#' @return The function returns the original dataframe with an added variable called "reading_speed" that contains reading speed (in words/min) for each sentence tested.
#'
#' @section Notes:
#' For general purposes, this method of reading speed calculation should be used preferentially over the less precise \code{\link{readingSpeed_nonCorrected}}.
#'
#' @seealso
#' \code{\link{readingSpeed_nonCorrected}} for reading speed non corrected for errors
#'
#' @examples # inspect the strucutre of the dataframe
#' @examples head(data_low_vision, 10)
#'
#' @examples # run the reading speed calculation
#' @examples data_low_vision_new <- readingSpeed(data_low_vision, rt, err)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_new, 10)
#'
#' @import dplyr
#'
#' @export
readingSpeed <- function(data, reading_time, errors) {
  # This function calculates reading speed in words per minute (wpm) using reading time in seconds and number of errors.
  # Its calculation provides an exact value of reading speed, that DOES take into account the number of misread words.
  # By excluding words that were missed or read incorrectly, this function gives a more precise reading speed measurement than 'readingSpeed_nonCorrected()'
  # 'readingSpeed()' should be prefered over 'readingSpeed_nonCorrected()'

  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  reading_speed <- NULL

  data %>% mutate (reading_speed = (10 - replace ((!!errors), (!!errors) > 10, 10)) / (!!reading_time) * 60)
}

