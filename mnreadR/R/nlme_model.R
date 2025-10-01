#----- nlmeModel --------
#######################--

#' MNREAD data fitting using a nonlinear mixed-effect (NLME) modeling. 
#'
#' This function uses a nonlinear mixed effects model (NLME), as described in Cheung et al. 2008, 
#' where variations across individuals are modeled as random effects. 
#' This function estimates and returns the NLME model while performing print size correction for non-standard testing viewing distance (ie. different than 40 cm).
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param subjectID The variable that contains the subject identifiers
#' @param nested Optional argument to build a model with a nested structure. 'nested' specifies which variable should be nested within subject. Default is NULL.
#' @param group Optional argument to build a model with a grouped structure. 'group' specifies which variable should be used as grouping argument. Default is NULL
#' 
#' 
#' @return The function returns a list of two objects: 
#'   \itemize{
#'   \item an object of class dataframe which is a cleaned version of the dataset called by the function to fit the model
#'   \item an object of class nlme returned by the function \code{\link{nlme}} 
#'   }
#' 
#'
#' @section Notes:
#' For subjects with incomplete data, warning messages might appear in the console. However, the NLME model will run, 
#' using supporting data from the rest of the population.
#' 
#' This functions supports nested, grouped and nested + grouped structures. 
#' 
#' If needed, the nlme object returned can be further explored using generic functions from the nlme package.
#' 
#' This function implements several functions from the nlme package to build the NLME model:
#'   \itemize{
#'   \item it first calls groupedData() to format the dataset in order to match the desired structure
#'   \item it then uses nlsList() to generate starting values
#'   \item it finally calls nlme() to build the model 
#'   }
#'  
#' For more details on the nlme fit, see:\\
#' Cheung SH, Kallie CS, Legge GE, Cheong AM. Nonlinear mixed-effects modeling of MNREAD data. 
#' Invest Ophthalmol Vis Sci. 2008;49:828–835. doi: 10.1167/iovs.07-0555.
#'
#'
#' @section Warning:
#' For the function to run properly, please make sure that variables are of the following classes:
#'   \itemize{
#'   \item \strong{print_size} -> numeric
#'   \item \strong{viewing_distance} -> integer
#'   \item \strong{reading_time} -> numeric
#'   \item \strong{errors} -> integer
#'   }
#'   
#' The optional arguments "nested" and "group" should only be specified when they are needed. 
#' In case they are called and set to NULL, the function will not run and will return an error.
#'
#'
#'
#' @seealso
#'  \code{\link{nlmeParam}} to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS) from the NLME model
#'  
#'  \code{\link{nlmeCurve}} to plot the individual MNREAD curves estimated from the NLME model
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
#' \donttest{ model_simple <- nlmeModel(data_regular, ps, vd, rt, err, subject) }
#'
#' # to print the model summary
#' \donttest{ summary(model_simple[[2]]) }
#' 
#' # to print the first 3 rows of the cleaned dataset containing the raw data and used to run the model
#' \donttest{ head(model_simple[[1]], 3) }
#'
#' #------
#'
#' # run the NLME model on the whole dataset with polarity nested within subject
#' \donttest{ model_nested <- lmeModel(data_low_vision, ps, vd, rt, err, subject,
#'                                    nested = polarity) }
#'
#' #------
#'
#' # run theNLME model on the whole dataset with polarity nested within subject 
#' # and grouped based on treatment
#' \donttest{ model_nested_grouped <- nlmeModel(data_low_vision, ps, vd, rt, err, subject,
#'                                             nested = polarity, group = treatment) }
#'
#'
#' 
#' @importFrom nlme nlsList nlme groupedData nlmeControl
#' @import dplyr
#' 
#' 
#'
#' @export
nlmeModel <- function(data, print_size, viewing_distance, reading_time, errors, subjectID, nested = NULL, group = NULL) {
  # This function runs an nlme fit on raw MNREAD data
  
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  subjectID <- enquo(subjectID)
  nested <- enquo(nested)
  group <-enquo(group)
  subject <- NULL
  group_var <- NULL
  nested_var <- NULL
  rs <- NULL
  log_rs <- NULL
  correct_ps <- NULL
  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  . <- NULL
  
  message('This may take a few minutes... just relax and take a break :-)')
  
  # modify the raw dataframe as needed before running the model
  temp_df <- as.data.frame(
    data %>%
      mutate (subject = (!!subjectID) ) %>%
      mutate (nested_var = (!!nested) ) %>%
      mutate (group_var = (!!group) ) %>%
      filter ((!!errors) != "NA" & (!!reading_time) > 0) %>%
      mutate (rs = (10 - replace ((!!errors), (!!errors) > 10, 10)) / (!!reading_time) * 60) %>%
      filter (rs != "NA", rs != "-Inf") %>%
      mutate (log_rs = log10(rs)) %>%
      filter (log_rs != "NA", log_rs != "-Inf") %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf")
  )
  
  
  # create the groupedData to be passed to the NLME model
  ## NB: no matter what structure is used for the model (nested, grouped, ...)
  ## the model call will always be the same, it is the structure of the groupedData that will change
  
  if ( "nested_var" %in% names(temp_df) == FALSE  ) {
    if ( "group_var" %in% names(temp_df) == FALSE ) {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject,
                                data = temp_df %>% select(log_rs, correct_ps, subject),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
    else {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject,
                                outer = ~ group_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, group_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
  }
  
  if ( "nested_var" %in% names(temp_df) == TRUE ) {
    if ( "group_var" %in% names(temp_df) == FALSE ) {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject/nested_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, nested_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))     }
    else {
      grouped_df <- groupedData(log_rs ~ correct_ps | subject/nested_var,
                                outer = ~ group_var,
                                data = temp_df %>% select(log_rs, correct_ps, subject, nested_var, group_var),
                                labels = list( x = "Print size", y = "Log reading speed"),
                                units = list( x = "(logMAR)", y = "(logWPM)"))    }
  }
  
  # generate starting values for nlme by running nlsList
  my.list <- nlsList (model = log_rs ~ SSasympOff (correct_ps, asym, lrc, x_intercept) ,
                      data = grouped_df )
  my.starting.values <- colMeans( as.matrix( coef(my.list), na.rm=TRUE ), 
                                  na.rm=TRUE ) # Missing values should be omitted from the calculations
  
  # run my nlme model
  my.model <- nlme (model = log_rs ~ SSasympOff (correct_ps, asym, lrc, x_intercept),
                    data = grouped_df,
                    fixed = asym + lrc + x_intercept ~ 1, # set the fixed structure for nlme
                    # by default the random structure is set to (asym ~ 1, lrc ~ 1, x_intercept ~ 1) for grp_var
                    start = my.starting.values,
                    control = nlmeControl(maxIter = 500, pnlsTol = 0.3)) # set the control parameters for nlme
  
  list_to_return <- list(temp_df, my.model)
  
  return(list_to_return)
  
}
