#----- ReadingAccessibilityIndex -------
#####################################---

#' Reading ACCessibility Index (ACC) calculation
#'
#' This function calculates the Reading Accessibility Index, while applying suited rules for missing data.
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param ... Optional grouping arguments
#'
#' @return The function returns a new dataframe with a variable called "ACC" that contains the Reading Accessibility Index estimate.
#'
#' @section Notes:
#' The Reading ACCessibility Index (ACC) is a new measure representing an individual's access to text over the range of print sizes found in everyday life.
#' Its calculation does not rely on curve fitting and gives a direct comparison with the performance of normally sighted individuals.
#' The ACC calculation uses the print size values non corrected for non-standard viewing distance.
#'
#' For more details on the Reading Accessibility Index, see http://doi.org/10.1001/jamaophthalmol.2015.6097
#' 
#'
#' @section Warning:
#' To ensure that missing data are handled properly and that ACC calculation is correct, data need to be entered along certain rules:
#'  \itemize{
#'   \item For the smallest print size that is presented but not read, right before the test is stopped: \strong{reading_time = NA, errors = 10}
#'   \item For all the small sentences that are not presented because the test was stopped before them: \strong{reading_time = NA, errors = NA}
#'   \item If a sentence is presented, and read, but the time was not recorded by the experimenter: \strong{reading_time = NA, errors = actual number of errors} (cf. s5-regular in low vision data sample)
#'   \item If a large sentence was skipped to save time but would have been read well: \strong{reading_time = NA, errors = NA} (cf. s1-regular in normal vision data sample)
#'   \item If a large sentence was skipped to save time because the subject cannot read large print: \strong{reading_time = NA, errors = 10} (cf. s7 in low vision data sample)
#'   }
#'
#' @seealso
#'  \code{\link{mnreadParam}} for all MNREAD parameters estimation
#'
#'  \code{\link{curveParam_RT}} for MRS and CPS estimation using values of reading time (instead of reading speed)
#'
#'  \code{\link{curveParam_RS}} for MRS and CPS estimation using values of reading speed (instead of reading time)
#'
#'  \code{\link{readingAcuity}} for Reading Acuity calculation
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
#' @examples # run the reading accessibility index calculation
#' @examples data_low_vision_ACC <- accIndex(data_s1, ps, rt, err)
#'
#' @examples # inspect the newly created dataframe
#' @examples data_low_vision_ACC
#'
#' #------
#'
#' @examples # run the reading accessibility index calculation
#' @examples # on the whole dataset grouped by subject and polarity
#' @examples data_low_vision_ACC <- accIndex(data_low_vision, ps, rt, err,
#' @examples                                subject, polarity)
#'
#' @examples # inspect the structure of the newly created dataframe
#' @examples head(data_low_vision_ACC, 10)
#'
#' @import dplyr
#'
#' @export
accIndex <- function(data, print_size, reading_time, errors, ... = NULL) {
  # This function calculates the Reading Accessibility Index and returns it in a new dataframe.
  # It calls the acc_algo() function that contains the actual code calculation
  # and uses the non-corrected print size to make the calculation
  # It takes as many grouping arguments as needed :-)
  # To make sure that this calculaiton is correct, the data needs to be entered along certains rules:
  #   For the smallest print size that is presented but not read, right before the test is stopped: rt = NA, err = 10
  #   For all the small sentences that are not presented because the test was stopped before them: rt = NA, err = NA
  #   If a sentence is presented, and read, but the time was not recorded by the experimenter: rt = NA, err = actual number of errors (cf. s5-regular in LV data sample)
  #   If a large sentence was skipped to save time but would have been read well: rt = NA, err = NA (cf. s1-regular in NV data sample)
  #   If a large sentence was skipped to save time because the subject cannot read large print: rt = NA, err = 10 (cf. s7 in LV data sample)

  print_size <- enquo(print_size)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  rs <- NULL
  r_time <- NULL
  error_nb <- NULL
  p_size <- NULL
  ps <- NULL
  . <- NULL

  # modify the raw dataframe as needed before running the actual ACC calculation
  temp_df <- as.data.frame(
    data %>%
      mutate (rs = (10 - replace ((!!errors), (!!errors) > 10, 10)) / (!!reading_time) * 60) %>%
      mutate (r_time = (!!reading_time)) %>%
      mutate (error_nb = (!!errors)) %>%
      mutate (p_size = (!!print_size)) %>%
      mutate (ps = p_size) %>%
      filter (p_size >= 0.4 & p_size <= 1.3 )  
      
  )

  # estimates ACC with no grouping argument
  if ( missing(...) )  {
    ACCdf <- as.data.frame(
      temp_df %>%
        do (acc_algo(.))  )
  }

  # estimates ACC with grouping argument(s)
  else {
    grouping_var <- quos(...)
    ACCdf <- as.data.frame(
      temp_df %>%
        group_by (!!!grouping_var) %>%
        do (acc_algo(.))  )
  }

  return(ACCdf)
  
}

acc_algo <- function(df) {

  ACC <- NULL

  # In case data are not entered for all sentences in the ACC calculation range
  # I need to fill the dataframe with print size down to 0.4 logMAR
  ps = seq (1.3, 0.4, by = sign(0.4-1.3) * 0.1) # generates a decreasing sequence from 1.3 to 0.4
  df <- right_join(df, as.data.frame(ps), by = "ps") # adds rows for missing print sizes and fills them with NAs
  # df <- df %>% mutate (id = unique(na.omit(df$id))) # fill in the id value for added rows
  
  # I estimate the ACC assuming that Reading Speed has no missing values
  #   If there are no missing values, the ACC will compute OK
  #   If there are some missing values, the ACC will be set to NA
  ACC <- as.data.frame (
    df %>%
      reframe (ACC = mean (df$rs) / 200) )

  # If ACC was set to NA, it means that Reading Speed contains some missing values
  # Here I set rules to deal with these missing values
  if (is.na(ACC) == TRUE) {
    
    # for the last row of the temp_df only (ps = 0.4)
    for (i in nrow(df)) {
      if (is.na(df$rs[i]) == TRUE) {
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == TRUE) {
          df$rs[i] = 0}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] %in% 10) {
          df$rs[i] = 0}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] < 10 && is.na(df$r_time[i-1]) == FALSE) {
          df$rs[i] = df$rs[i-1]}
        }
    }
  
    # for rows 2 to 9 of the temp_df only (ps = 0.5 to 1.2)
    for (i in (nrow(df)-1):2) {
      if (is.na(df$rs[i] == TRUE)) {
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == TRUE) {
          df$rs[i] = 0}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] %in% 10) {
          df$rs[i] = 0}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] < 10 && is.na(df$r_time[i-1]) == FALSE) {
          df$rs[i] = mean(c(df$rs[i-1], df$rs[i+1]))}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] < 10 && is.na(df$r_time[i-1]) == TRUE) {
          df$rs[i] = mean(c(df$rs[i-2], df$rs[i+1]))}
      }
    }

    # for the first row of the temp_df only (ps = 1.3)
    for (i in 1:1) {
      if (is.na(df$rs[i] == TRUE)) {
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == TRUE) {
          df$rs[i] = df$rs[i+1]}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] < 10) {
          df$rs[i] = df$rs[i+1]}
        if (is.na(df$r_time[i]) == TRUE && is.na(df$error_nb[i]) == FALSE && df$error_nb[i] %in% 10) {
          df$rs[i] = 0}
      }
    }

  }
 
  # I calculate the ACC again now that I have assign a value to each missing reading speed
  ACC <- as.data.frame(
    df %>%
      reframe (ACC = mean (df$rs) / 200) )
  
  return(ACC)
  
  }
  

ACCcalc <- function(data, print_size, reading_time, errors, ... = NULL) {
  .Deprecated("accIndex")
}

