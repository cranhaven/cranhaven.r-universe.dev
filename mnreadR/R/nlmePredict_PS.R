#----- nlmePredict_PS ------
##########################--

#' Estimation of the print size value necessary to achieve a given reading speed.
#' 
#' This function uses results from the NLME model created with \code{\link{nlmeModel}} to estimate the print size value required to achieve a specific reading speed.
#'
#' @param nlme.model The object returned by \code{\link{nlmeModel}}
#' @param reading.speed A specific value of reading speed in words/minute
#'
#' @return 
#' The function returns a dataframe with a with two variables:
#'  \itemize{
#'   \item "reading_speed" -> the reading speed value passed to the function (in words/min)
#'   \item "required_print_size" -> the print size required to achieve the reading speed value passed to the function (in logMAR)
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
#' # extract the critical print size required 
#' # to achieve 40 words/min (ie. spot reading) according to the NLME fit 
#' \donttest{ nlmePredict_PS(nlme_model, 40) }
#' 
#' #------
#'
#' # extract the critical print size required 
#' # to achieve 80 words/min (ie. fluent reading) according to the NLME fit 
#' \donttest{ nlmePredict_PS(nlme_model, 80) }
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
nlmePredict_PS <- function(nlme.model, reading.speed) {

  print_size <- NULL
  reading_speed <- NULL
  estimated_print_size <- NULL
  rowname <- NULL
  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  subject <- NULL
  nested_var <- NULL
  
  # extract coefficients from the nlme model
  my.coef <- rownames_to_column(as.data.frame(coef(nlme.model[[2]])))
  
  # create a new df with the estimated print size for the reading speed value passed to the function 
  clean_predictRS <- my.coef %>%
    separate (rowname, into = c("subject", "nested_var"), sep = "/", fill = "right") %>% 
    mutate (reading_speed = reading.speed) %>%
    
    # the starting point for this calculation is the SSasympOff function used by nlme():
    # log_rs = asym*(1 - exp(-exp(lrc)*(correct_ps - x_intercept)))
    # I simply inverted it to get correct_ps
    mutate (estimated_print_size = (log (1-( log10(reading_speed)/asym ) ) * (1/-exp(lrc))) + x_intercept) %>%
    
    select (subject, nested_var, reading_speed, estimated_print_size) %>%
    select_if (~!all(is.na(.)) ) 
  
  return(clean_predictRS)
  
}
