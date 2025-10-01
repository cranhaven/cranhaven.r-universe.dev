#----- ReadingAcuity ----
#######################--

#' Reading Acuity (RA) calculation
#'
#' Reading Acuity (RA) is defined as the smallest print size at which one can read without making significant errors.
#' This function measures Reading Acuity to the nearest 0.1 logMAR, while performing print size correction for non-standard testing viewing distance.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param ... Optional grouping arguments
#'
#' @return The function returns a new dataframe with a variable called "RA" that contains the Reading Acuity estimate (in logMAR).
#'
#' @seealso
#' \code{\link{mnreadParam}} for all MNREAD parameters estimation
#'
#' \code{\link{curveParam_RT}} for MRS and CPS estimation using values of reading time (instead of reading speed)
#'
#' \code{\link{curveParam_RS}} for MRS and CPS estimation using values of reading speed (instead of reading time)
#'
#' \code{\link{accIndex}} for Reading Accessibility Index calculation
#'
#'
#' @examples # inspect the structure of the dataframe
#' @examples head(data_low_vision, 10)
#'
#' #------
#'
#' @examples # restrict dataset to one MNREAD test only (subject s1, regular polarity)
#' @examples data_s1 <- data_low_vision %>% filter (subject == "s1" & polarity == "regular")
#'
#' @examples # run the reading acuity calculation
#' @examples data_low_vision_RA <- readingAcuity(data_s1, ps, vd, rt, err)
#'
#' @examples # inspect the newly created dataframe
#' @examples data_low_vision_RA
#'
#' #------
#'
#' @examples # run the reading acuity calculation on the whole dataset grouped by subject and polarity
#' @examples data_low_vision_RA <- readingAcuity(data_low_vision, ps, vd, rt, err,
#' @examples                                     subject, polarity)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_RA, 10)
#'
#' @import dplyr
#'
#' @export
readingAcuity <- function(data, print_size, viewing_distance, reading_time, errors, ... = NULL) {
  # This function estimates the Reading Acuity (RA) and returns it in a new dataframe.

  # These lines are needed in order to call the variable names unquoted
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)

  # These lines are needed to avoid the following note when running R-CMD check using devtools::check()
  # readingAcuity: no visible binding for global variable ‘errors10’
  # in other words, I need to define these variables in the environement of my function before I create them with dplyr
  errors10 <- NULL
  correct_ps <- NULL
  min_ps <- NULL
  sum_err <- NULL

  # modify the raw dataframe as needed before running the actual RA calculation
  temp_df <- as.data.frame(
    data %>%
      filter ((!!errors) != "NA" & (!!reading_time) > 0) %>%
      mutate (errors10 = replace ((!!errors), (!!errors) > 10, 10)) %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2))   )

  # calculate RA with no grouping argument
  if ( missing(...) )  {
    as.data.frame(
      temp_df %>%
        reframe (min_ps = min(correct_ps),
                   sum_err = sum((errors10), na.rm=T)) %>%
        mutate (RA = min_ps + sum_err*(0.01)) %>%
        select (-min_ps, -sum_err) )
  }

  # calculate RA with grouping argument(s)
  else {
    grouping_var <- quos(...)
    as.data.frame(
      temp_df %>%
        group_by (!!!grouping_var) %>%
        reframe (min_ps = min(correct_ps),
                   sum_err = sum((errors10), na.rm=T)) %>%
        mutate (RA = min_ps + sum_err*(0.01)) %>%
        select (-min_ps, -sum_err)  )
  }
}


