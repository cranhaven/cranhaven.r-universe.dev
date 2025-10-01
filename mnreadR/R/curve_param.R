#----- CurveParam RT -------
#######################--

#' Standard estimation of Maximum Reading Speed (MRS) and Critical Print Size (CPS) 
#' using individual data of reading time and number of errors.
#'
#' This function estimates simultaneously:
#'  \itemize{
#'   \item Maximum Reading Speed (MRS)
#'   \item Critical Print Size (CPS)
#'   }
#' while performing print size correction for non-standard testing viewing distance.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param ... Optional grouping arguments
#'
#' @return The function returns a new dataframe with two variables:
#'  \itemize{
#'   \item "CPS" -> contains the Critical Print Size estimate (in logMAR)
#'   \item "MRS" -> contains the Maximum Reading Speed estimate (in words/min)
#'   }
#'
#' @section Notes:
#' This function uses the original algorithm described in Legge (2007) to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS).
#' This algorithm searches for a reading speed plateau in the data. A plateau is defined as a range of print sizes
#' that supports reading speed at a significantly faster rate than the print sizes smaller or larger than the plateau range.
#' Concretely, the plateau is determined as print sizes which reading speed is at least 1.96 SD faster than the other print sizes.
#' The Maximum Reading Speed is estimated as the mean reading speed for print sizes included in the plateau.
#' The Critical Print Size is defined as the smallest print size on the plateau.
#'
#' For more details on the original algorithm, see Chapter 5 of this book:\\
#' Legge, G.E. (2007). Psychophysics of Reading in Normal and Low Vision. Mahwah, NJ & London: Lawrence Erlbaum Associates. ISBN 0-8058-4328-0
#' \url{https://books.google.fr/books/about/Psychophysics_of_Reading_in_Normal_and_L.html?id=BGTHS8zANiUC&redir_esc=y}
#'
#' To ensure proper estimation of the MRS and CPS, individual MNREAD curves should be plotted using \code{\link{mnreadCurve}} and inspected visually.
#'
#' @section Warning:
#' For the function to run properly, one needs to make sure that the variables are of the class:
#'  \itemize{
#'   \item \strong{print_size} -> numeric
#'   \item \strong{viewing_distance} -> integer
#'   \item \strong{reading_time} -> numeric
#'   \item \strong{errors} -> integer
#'   }
#'
#' In cases where only 3 or less sentences were read during a test,
#' the function won't be able to estimate the MRS and CPS
#' and will return NA values instead.
#' The ACC should be used to estimate the MNREAD score in such cases
#' where there are not enough data points to fit the MNREAD curve.
#' 
#' To ensure proper parameters estimation, the data should be entered along certain rules:
#'  \itemize{
#'   \item For the smallest print size that is presented but not read, right before the test is stopped: \strong{reading_time = NA, errors = 10}
#'   \item For all the small sentences that are not presented because the test was stopped before them: \strong{reading_time = NA, errors = NA}
#'   \item If a sentence is presented, and read, but the time was not recorded by the experimenter: \strong{reading_time = NA, errors = actual number of errors} (cf. s5-regular in low vision data sample)
#'   \item If a large sentence was skipped to save time but would have been read well: \strong{reading_time = NA, errors = NA} (cf. s1-regular in normal vision data sample)
#'   \item If a large sentence was skipped to save time because the subject cannot read large print: \strong{reading_time = NA, errors = 10} (cf. s7 in low vision data sample)
#'   }
#'
#' @seealso
#'  \code{\link{curveParam_RS}} for standard MRS and CPS estimation using values of reading speed (instead of reading time)
#'
#'  \code{\link{nlmeParam}} for MRS and CPS estimation using nonlinear mixed-effect (NLME) modeling
#'
#'  \code{\link{mnreadParam}} for all MNREAD parameters estimation (using standard calculation)
#'
#'  \code{\link{readingAcuity}} for Reading Acuity calculation
#'
#'  \code{\link{accIndex}} for Reading Accessibility Index calculation
#'
#'
#' @examples # inspect the structure of the dataframe
#' @examples head(data_low_vision, 10)
#'
#' #------
#'
#' @examples # restrict dataset to one MNREAD test only (subject s1, regular polarity)
#' @examples data_s1 <- data_low_vision %>%
#' @examples    filter (subject == "s1", polarity == "regular")
#'
#' @examples # run the parameters estimation
#' @examples data_low_vision_MRS_CPS <- curveParam_RT(data_s1, ps, vd, rt, err)
#'
#' @examples # inspect the newly created dataframe
#' @examples data_low_vision_MRS_CPS
#'
#' #------
#'
#' @examples # run the parameters estimation on the whole dataset grouped by subject and polarity
#' @examples data_low_vision_MRS_CPS <- curveParam_RT(data_low_vision, ps, vd, rt, err,
#' @examples                                            subject, polarity)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_MRS_CPS, 10)
#'
#' @importFrom stats sd
#' @import dplyr
#'
#' @export
curveParam_RT <- function(data, print_size, viewing_distance, reading_time, errors, ... = NULL) {
  # This function estimates the Maximum reading Speed (MRS) and Critical Print Size (CPS) and returns them in a new dataframe.
  # It calls the mansfield_algo() function that contains the actual estimation
  # It takes as many grouping arguments as needed :-)

  # warning('Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()',
  #         call. = FALSE, immediate. = FALSE)
  
  message('Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()')
  
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  rs <- NULL
  log_rs <- NULL
  correct_ps <- NULL
  nb_row <- NULL
  . <- NULL
  .drop <- TRUE

  # modify the raw dataframe as needed before running the actual MRS And CPS estimation
  temp_df <- as.data.frame(
    data %>%
      filter ((!!errors) != "NA" & (!!reading_time) > 0) %>%
      mutate (rs = (10 - replace ((!!errors), (!!errors) > 10, 10)) / (!!reading_time) * 60) %>%
      filter (rs != "NA", rs != "-Inf") %>%
      mutate (log_rs = log(rs)) %>%
      filter (log_rs != "NA", log_rs != "-Inf") %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf") )

  # estimates MRS and CPS with no grouping argument
  if ( missing(...) )  {
    as.data.frame(
      temp_df %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n()) %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  )
  }

  # estimates MRS and CPS with grouping argument(s)
  else {
    grouping_var <- quos(...)
    as.data.frame(
      temp_df %>%
        group_by (!!!grouping_var, .drop = TRUE) %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n())  %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  ) #%>%
      # filter (.drop != "NA") %>% select (-.drop)
  }
}

curveParam <- function(data, print_size, viewing_distance, reading_time, errors, ... = NULL) {
  .Deprecated("curveParam_RT")
}


#-------------

#----- CurveParam RS -------
#######################--

#' Standard estimation of Maximum Reading Speed (MRS) and Critical Print Size (CPS) using reading speed values.
#'
#' This function estimates simultaneously:
#'  \itemize{
#'   \item Maximum Reading Speed (MRS)
#'   \item Critical Print Size (CPS)
#'   }
#' while performing print size correction for non-standard testing viewing distance.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_speed The variable that contains the reading speed for each sentence
#' @param ... Optional grouping arguments
#'
#' @return The function returns a new dataframe with two variables:
#'  \itemize{
#'   \item "CPS" -> contains the Critical Print Size estimate (in logMAR)
#'   \item "MRS" -> contains the Maximum Reading Speed estimate (in words/min)
#'   }
#'
#' @section Notes:
#' This function uses the original algorithm described in Legge (2007) to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS).
#' This algorithm searches for a reading speed plateau in the data. A plateau is defined as a range of print sizes
#' that supports reading speed at a significantly faster rate than the print sizes smaller or larger than the plateau range.
#' Concretely, the plateau is determined as print sizes which reading speed is at least 1.96 SD faster than the other print sizes.
#' The Maximum Reading Speed is estimated as the mean reading speed for print sizes included in the plateau.
#' The Critical Print Size is defined as the smallest print size on the plateau.
#'
#' For more details on the original algorithm, see Chapter 5 of this book:\\
#' Legge, G.E. (2007). Psychophysics of Reading in Normal and Low Vision. Mahwah, NJ & London: Lawrence Erlbaum Associates. ISBN 0-8058-4328-0
#' \url{https://books.google.fr/books/about/Psychophysics_of_Reading_in_Normal_and_L.html?id=BGTHS8zANiUC&redir_esc=y}
#'
#' To ensure proper estimation of the MRS and CPS, individual MNREAD curves should be plotted using \code{\link{mnreadCurve}} and inspected visually.
#'
#' @section Warning:
#' To run the function properly, one needs to make sure that the variables are of the class:
#'  \itemize{
#'   \item \strong{print_size} -> numeric
#'   \item \strong{viewing_distance} -> integer
#'   \item \strong{reading_speed} -> numeric
#'   }
#'
#' In cases where only 3 or less sentences were read during a test,
#' the function won't be able to estimate the MRS and CPS
#' and will return NA values instead.
#' The ACC should be used to estimate the MNREAD score in such cases
#' where there are not enough data points to fit the MNREAD curve.
#'
#' @seealso
#'  \code{\link{curveParam_RT}} for standard MRS and CPS estimation using values of reading time (instead of reading speed)
#'
#'  \code{\link{nlmeParam}} for MRS and CPS estimation using a nonlinear mixed-effect model (NLME)
#'
#'  \code{\link{mnreadParam}} for all MNREAD parameters estimation
#'
#'  \code{\link{readingAcuity}} for Reading Acuity calculation
#'
#'  \code{\link{accIndex}} for Reading Accessibility Index calculation
#'
#'
#' @examples # inspect the structure of the dataframe
#' @examples head(data_low_vision, 10)
#' 
#' @examples # create the reading speed variable 
#' @examples data_low_vision <- data_low_vision %>% 
#' @examples   mutate (rs = (10 - replace (err, err > 10, 10)) / rt * 60)
#'
#' #------
#'
#' @examples # restrict dataset to one MNREAD test only (subject s1, regular polarity)
#' @examples data_s1 <- data_low_vision %>%
#' @examples    filter (subject == "s1", polarity == "regular")
#'
#' @examples # run the parameters estimation
#' @examples data_low_vision_MRS_CPS <- curveParam_RS(data_s1, ps, vd, rs)
#'
#' @examples # inspect the newly created dataframe
#' @examples data_low_vision_MRS_CPS
#'
#' #------
#'
#' @examples # run the parameters estimation on the whole dataset grouped by subject and polarity
#' @examples data_low_vision_MRS_CPS <- curveParam_RS(data_low_vision, ps, vd, rs,
#' @examples                                            subject, polarity)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_MRS_CPS, 10)
#'
#' @importFrom stats sd
#' @import dplyr
#'
#' @export
curveParam_RS <- function(data, print_size, viewing_distance, reading_speed, ... = NULL) {
  # This function estimates the Maximum reading Speed (MRS) and Critical Print Size (CPS) and returns them in a new dataframe.
  # It calls the mansfield_algo() function that contains the actual estimation
  # It takes as many grouping arguments as needed :-)

  message('Remember to check the accuracy of MRS and CPS estimates by inspecting the MNREAD curve with mnreadCurve()')
  
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_speed <- enquo(reading_speed)
  rs <- NULL
  log_rs <- NULL
  correct_ps <- NULL
  nb_row <- NULL
  . <- NULL
  .drop <- TRUE

  # modify the raw dataframe as needed before running the actual MRS And CPS estimation
  temp_df <- as.data.frame(
    data %>%
      mutate (rs = (!!reading_speed) ) %>%
      filter (rs != "NA", rs != "-Inf") %>%
      mutate (log_rs = log(rs)) %>%
      filter (log_rs != "NA", log_rs != "-Inf") %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf") )

  # estimates MRS and CPS with no grouping argument
  if ( missing(...) )  {
    as.data.frame(
      temp_df %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n()) %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  )
  }

  # estimates MRS and CPS with grouping argument(s)
  else {
    grouping_var <- quos(...)
    as.data.frame(
      temp_df %>%
        group_by (!!!grouping_var, .drop = TRUE) %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n()) %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  ) #%>%
      # filter (.drop != "NA") %>% select (-.drop)
  }
}


