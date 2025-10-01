#----- nlmePredict_RS ------
##########################--

#' Estimation of the reading speed achieved for a given print size.
#' 
#' This function uses results from the NLME model created with \code{\link{nlmeModel}} to estimate the reading speed achieved for a specific print size.
#'
#' @param nlme.model The object returned by \code{\link{nlmeModel}}
#' @param print.size A specific value of print size in logMAR
#'
#' @return 
#' The function returns a dataframe with a with two variables:
#'  \itemize{
#'   \item "print_size" -> the print size value passed to the function (in logMAR) 
#'   \item "estimated_reading_speed" -> the reading speed achieved at the specified print size as estimated by the NLME model (in words/min)
#'   }
#'   
#'
#' @section Notes:
#' The values of print size returned have been corrected for non-standard testing viewing distance.
#' 
#' For more details on the nlme fit, see:\\
#' Cheung SH, Kallie CS, Legge GE, Cheong AM. Nonlinear mixed-effects modeling of MNREAD data. 
#' Invest Ophthalmol Vis Sci. 2008;49:828–835. doi: 10.1167/iovs.07-0555.
#' 
#'
#'
#' @seealso
#' \code{\link{nlmeModel}} to fit MNREAD data using a nonlinear mixed-effect (NLME) modeling
#'
#' \code{\link{nlmeParam}} to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS) from the NLME model
#'  
#' \code{\link{nlmeCurve}} to plot the individual MNREAD curves estimated from the NLME model
#'
#'
#'
#' @examples 
#' # inspect the structure of the dataframe
#' head(data_low_vision, 10)
#'
#' #------
#' 
#' # restrict dataset to one MNREAD test per subject (regular polarity only)
#' data_regular <- data_low_vision %>%
#'     filter (polarity == "regular")
#'
#' # run the NLME model for data grouped by subject
#' \donttest{ nlme_model <- nlmeModel(data_regular, ps, vd, rt, err, subject) }
#'
#' #------
#' 
#' # extract reading speed achieved at 1.6 logMAR according to the NLME fit 
#' \donttest{ nlmePredict_RS(nlme_model, 1.6) }
#'
#'
#'
#' @importFrom stats sd coef predict
#' @importFrom tibble rownames_to_column
#' @import dplyr
#' @import tidyr
#'
#'  
#'
#' @export
nlmePredict_RS <- function(nlme.model, print.size) {
  
  # the starting point for this calculation is the SSasympOff function used by nlme():
  # log_rs = asym*(1 - exp(-exp(lrc)*(correct_ps - x_intercept)))
  
  print_size <- NULL
  reading_speed_log <- NULL
  reading_speed <- NULL
  rowname <- NULL
  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  subject <- NULL
  nested_var <- NULL
  # predict.subject <- NULL
  # predict.nested_var <- NULL
  
  # extract coefficients from the nlme model
  my.coef <- rownames_to_column(as.data.frame(coef(nlme.model[[2]])))
  
  # create a new df with the specific print size passed to the function 
  newdata <- my.coef %>%
    separate (rowname, into = c("subject", "nested_var"), sep = "/", fill = "right") %>% 
    mutate (correct_ps = print.size)
  
  clean_predictRS <- my.coef %>%
    separate (rowname, into = c("subject", "nested_var"), sep = "/", fill = "right") %>% 
    mutate (print_size = print.size) %>%
    mutate (reading_speed_log = asym*(1 - exp(-exp(lrc)*(print_size - x_intercept)))) %>%
    mutate (reading_speed = 10^reading_speed_log) %>%
    select (subject, nested_var, print_size, reading_speed_log, reading_speed) %>%
    select_if (~!all(is.na(.)) ) 
  
  
  # # non-nested design 
  # if ( "nested_var" %in% names(nlme.model[[1]]) == FALSE  ) { 
  #   
  #   # extract prediction for newdata
  #   predictRS <- predict(nlme.model[[2]], newdata, level = 0:1)
  #   
  #   # clean the prediction dataframe
  #   clean_predictRS <- predictRS %>%
  #     mutate (print_size = print.size) %>%
  #     mutate (reading_speed_log = predict.subject) %>% 
  #     mutate (reading_speed = 10^predict.subject) %>% 
  #     select (subject, print_size, reading_speed_log, reading_speed) 
  #   
  # }
  # 
  # # nested design 
  # if ( "nested_var" %in% names(nlme.model[[1]]) == TRUE ) {
  #   
  #   # extract prediction for newdata
  #   predictRS <- predict(nlme.model[[2]], newdata, level = 0:2)
  #   
  #   # clean the prediction dataframe
  #   clean_predictRS <- predictRS %>%
  #     mutate (print_size = print.size) %>% 
  #     mutate (reading_speed_log = predict.nested_var) %>% 
  #     mutate (reading_speed = 10^predict.nested_var) %>% 
  #     select (nested_var, print_size, reading_speed_log, reading_speed) %>% 
  #     separate (nested_var, into = c("subject", "nested_var"), sep = "/", fill = "right") 
  #   
  # }
  
  return(clean_predictRS)
  
}
