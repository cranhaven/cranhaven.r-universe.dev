#----- mnreadCurve ------
#######################--

#' MNREAD curve plotting.
#'
#' This function plots individual MNREAD curves, while showing the estimated MNREAD parameters:
#'  \itemize{
#'   \item Maximum Reading Speed (MRS)
#'   \item Critical Print Size (CPS)
#'   \item Reading Acuity (RA)
#'   }
#'
#' @param data The name of your dataframe
#' @param print_size The variable that contains print size values for each sentence (print size uncorrected for viewing distance)
#' @param viewing_distance The variable that contains the viewing distance value used for testing
#' @param reading_time The variable that contains the reading time for each sentence
#' @param errors The variable that contains the number of errors for each sentence
#' @param ... Optional grouping arguments
#'
#' @return 
#' The function returns a plot of reading speed (in words/min) as a function of print size (in logMAR). 
#' Reading Acuity is marked as a triangle, Maximum Reading Speed and Critical Print Size are shown with dashed lines. 
#' When using two grouping arguments, a colored diamond is added for clarification.
#' Highlighted data points represent the range of print sizes included in the Reading Accessibility Index calculation.
#'
#' @section Notes:
#' This function can't take more that two grouping arguments. 
#' The first grouping argument is used to draw sub-plots (using facet_wrap from ggplot2). 
#' The second grouping argument is color-coded.
#' 
#' This function performs print size correction for non-standard testing viewing distance before plotting the curve.
#' 
#' This function uses the original algorithm described in Legge (2007) to estimate Maximum Reading Speed (MRS) and Critical Print Size (CPS).
#' For more details on the parameters estimation, see \code{\link{curveParam_RT}}.
#'
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
#' MRS and CPS cannot be estimated and won't be displayed on the plot.
#' In such cases, the Reading Accessibility Index (ACC) can be used to estimate the MNREAD score instead (cf. \code{\link{accIndex}}).
#'
#' To ensure proper plotting, the data should be entered along certain rules:
#'  \itemize{
#'   \item For the smallest print size that is presented but not read, right before the test is stopped: \strong{reading_time = NA, errors = 10}
#'   \item For all the small sentences that are not presented because the test was stopped before them: \strong{reading_time = NA, errors = NA}
#'   \item If a sentence is presented, and read, but the time was not recorded by the experimenter: \strong{reading_time = NA, errors = actual number of errors} (cf. s5-regular in low vision data sample)
#'   \item If a large sentence was skipped to save time but would have been read well: \strong{reading_time = NA, errors = NA} (cf. s1-regular in normal vision data sample)
#'   \item If a large sentence was skipped to save time because the subject cannot read large print: \strong{reading_time = NA, errors = 10} (cf. s7 in low vision data sample)
#'   }
#'
#' @seealso
#' \code{\link{curveParam_RT}} for standard estimation of MRS and CPS using values of reading time (instead of reading speed)
#'
#' \code{\link{readingAcuity}} for Reading Acuity calculation
#'
#'
#'
#' @examples 
#' # inspect the structure of the dataframe
#' head(data_low_vision, 10)
#'
#' #------
#'
#' # restrict dataset to one MNREAD test only (subject s1, regular polarity)
#' data_s1_reg <- data_low_vision %>%
#'    filter (subject == "s1", polarity == "regular")
#'
#' # plot the MNREAD curve 
#' \donttest{ mnreadCurve(data_s1_reg, ps, vd, rt, err)  }
#' 
#' #------
#'
#' # restrict dataset to one subject (s1) and plot the MNREAD curves using ONE GROUPING ARGUMENT 
#' # (ie. polarity) 
#' data_s1 <- data_low_vision %>%
#'    filter (subject == "s1")
#'
#' # plot the MNREAD curve using ONE GROUPING ARGUMENT (ie. polarity)
#'  \donttest{ mnreadCurve(data_s1, ps, vd, rt, err, polarity)  }
#'
#' #------
#'
#' # restrict dataset to two subject (s1 & s2) and plot the MNREAD curves using TWO GROUPING ARGUMENTS 
#' # (ie. subject and polarity) 
#' data_s2 <- data_low_vision %>%
#'    filter (subject == "s1" | subject == "s2")
#'
#'  \donttest{ mnreadCurve(data_s2, ps, vd, rt, err, subject, polarity)  }
#' 
#' #------
#'
#' # Once created, the MNREAD curve can be customized as needed using ggplot2, 
#' # for ex., by adding the number of errors for each sentence on top of the curve
#'
#' # plot the MNREAD curve 
#' my.plot <- mnreadCurve(data_s1, ps, vd, rt, err, polarity)
#'
#' # display my.plot
#' print(my.plot)
#'
#' # calculate reading speed and perform print size correction
#' data_s1_new <- as.data.frame(
#' data_s1 %>%
#'     filter (err != "NA" & rt > 0) %>%
#'     mutate (errors10 = replace (err, err > 10, 10) ) %>%
#'     mutate (rs = 60 * (10 - errors10) / rt ) %>%
#'     mutate (correct_ps = ps + round(log10(40/(vd)), 2)) ) 
#'
#' # add the number of errors for each sentence 
#' my.new.plot <- my.plot + geom_text(aes(x = correct_ps, y = rs + 5, label = errors10),
#'                                    alpha = 0.5,
#'                                    data = data_s1_new %>% filter (errors10 != 0) )
#'
#' # display my.new.plot                                                                        
#' print(my.new.plot)
#' 
#' #------
#' 
#' # MNREAD curves can also be saved in a pdf file, with each page showing a different subject
#'  
#' # count the number of subjects to define the number of pages
#' num_pages = length(unique(data_s2$subject))
#' 
#' # create a pdf file 
#' \donttest{ 
#' pdf ("MNREAD_curves.pdf", width = 10.5, height = 8, paper = "special", useDingbats = TRUE)
#' 
#' # wrap the plots over several pages
#' for (i in seq(num_pages)){
#'     p <- mnreadCurve(data_s2 %>% filter (subject == sort(unique(data_s2$subject))[i]),
#'                      ps, vd, rt, err, subject, polarity) 
#'     print(p)
#' }
#' 
#' dev.off()  
#' }
#' 
#' 
#' @importFrom stats sd
#' @import dplyr
#' @import ggplot2
#' 
#'
#' @export
mnreadCurve <- function(data, print_size, viewing_distance, reading_time, errors, ... = NULL) {
  # This function plots the MNREAD curve. It also estimates the Reading Acuity (RA), 
  # Maximum Reading Speed (MRS) and Critical Print Size (CPS) and diplays them on the plot.
  
  print_size <- enquo(print_size)
  viewing_distance <- enquo(viewing_distance)
  reading_time <- enquo(reading_time)
  errors <- enquo(errors)
  errors10 <- NULL
  rs <- NULL
  log_rs <- NULL
  correct_ps <- NULL
  nb_row <- NULL
  min_ps <- NULL
  sum_err <- NULL
  MRS <- NULL
  CPS <- NULL
  . <- NULL
  .drop <- TRUE
 
  # modify the raw dataframe as needed before running the MRS And CPS estimation
  temp_df1 <- as.data.frame(
    data %>%
      filter ((!!errors) != "NA" & (!!reading_time) > 0) %>%
      mutate (errors10 = replace ((!!errors), (!!errors) > 10, 10)) %>%
      mutate (rs = 60 * (10 - errors10) / (!!reading_time)) %>%
      filter (rs != "NA", rs != "-Inf") %>%
      mutate (log_rs = log(rs)) %>%
      filter (log_rs != "NA", log_rs != "-Inf") %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf") )

  # modify the raw dataframe as needed to plot the data
  temp_df_plot <- as.data.frame(
    data %>%
      filter ((!!errors) != "NA" | (!!reading_time) != "NA") %>%
      mutate (errors10 = replace ((!!errors), (!!errors) > 10, 10)) %>%
      mutate (rs = 60 * (10 - errors10) / (!!reading_time)) %>%
      mutate (rs = replace(rs, is.na(rs), 0)) %>%
      mutate (correct_ps = (!!print_size) + round(log10(40/(!!viewing_distance)), 2)) %>%
      filter (correct_ps != "NA", correct_ps != "-Inf") )
  
  # plot the curve
  p <- ggplot(data = temp_df_plot,
              aes(x = correct_ps, y = rs))
  p <- p + scale_x_continuous(name = "Corrected Print Size (logMAR)")
  p <- p + scale_y_continuous(name = "Reading Speed (words/min)")
  # # show the number of errors
  # p <- p + geom_text(aes(x = correct_ps, y = rs + 10, label = errors10),
  #                    nudge_y = 0.5, alpha = 0.5,
  #                    data = temp_df_plot %>% filter (errors10 != 0) )
  
  # with no grouping argument
  if ( missing(...) )  {
    
    # calculate reading acuity
    RAdf <- as.data.frame(
      temp_df1 %>%
        reframe (min_ps = min(correct_ps),
                   sum_err = sum((errors10), na.rm=T)) %>%
        mutate (RA = min_ps + sum_err*(0.01)) %>%
        select (-min_ps, -sum_err)  )
    
    # estimates MRS and CPS
    MRS_CPSdf <- as.data.frame(
      temp_df1 %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n()) %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  )
    
    p <- p + geom_point()
    p <- p + geom_line() 
    # add RA
    p <- p + geom_point(aes_(x = quote(RA), y = 0),
                        shape = 25, size = 2, fill = "black", 
                        data = RAdf)
    # add ACC range
    p <- p + geom_point(aes_(),
                        size = 3,  alpha=.3,
                        data = temp_df_plot %>% filter ((!!print_size) %in% seq (1.3, 0.4, by = sign(0.4-1.3) * 0.1)) )

  }
  
  # with grouping argument(s)
  else {

    grouping_var <- quos(...)
    
    # calculate reading acuity
    RAdf <- as.data.frame(
      temp_df1 %>%
        group_by (!!!grouping_var, .drop = TRUE) %>%
        reframe (min_ps = min(correct_ps),
                   sum_err = sum((errors10), na.rm=T)) %>%
        mutate (RA = min_ps + sum_err*(0.01)) %>%
        select (-min_ps, -sum_err)  ) 
    
    # estimates MRS and CPS
    MRS_CPSdf <- as.data.frame(
      temp_df1 %>%
        group_by (!!!grouping_var, .drop = TRUE) %>%
        arrange (correct_ps) %>% # sort temp_df by correct_ps in ascending order
        mutate (nb_row = n()) %>%
        do (mansfield_algo(., .$correct_ps, .$nb_row, .$log_rs))  ) 
    
    if ( length(grouping_var) == 1 )  {
      p <- p + facet_wrap((grouping_var)[[1]], scales = "free")
      p <- p + geom_point()
      p <- p + geom_line()  
      # add RA
      p <- p + geom_point(aes_(x = quote(RA), y = 0),
                          shape = 25, fill = "black", size = 2,
                          data = RAdf)
      # add ACC range
      p <- p + geom_point(aes_(),
                          size = 3,  alpha=.3,
                          data = temp_df_plot %>% filter ((!!print_size) %in% seq (1.3, 0.4, by = sign(0.4-1.3) * 0.1)) )
      
    }
    
    if ( length(grouping_var) == 2 )  {
      p <- p + facet_wrap((grouping_var)[[1]], scales = "free")
      p <- p + geom_point(aes_(colour = (grouping_var)[[2]])) #(aes(colour = polarity))
      p <- p + geom_line(aes_(colour = (grouping_var)[[2]])) #(aes(colour = polarity))
      # add MRS/CPS crossing
      p <- p + geom_point(aes_(x = quote(CPS), y = quote(MRS), fill = (grouping_var)[[2]]),
                          shape = 23, size = 2,
                          data = MRS_CPSdf)
      # add RA
      p <- p + geom_point(aes_(x = quote(RA), y = 0, fill = (grouping_var)[[2]]),
                          shape = 25, size = 2,
                          data = RAdf)
      # add ACC range
      p <- p + geom_point(aes_(colour = (grouping_var)[[2]]),
                          size = 3,  alpha=.3,
                          data = temp_df_plot %>% filter ((!!print_size) %in% seq (1.3, 0.4, by = sign(0.4-1.3) * 0.1)) )
      
      }
    
    }
  
  # add MRS
  if (is.logical(MRS_CPSdf$MRS) == F) {
    p <- p + geom_hline(aes(yintercept = MRS),
                        linetype = "longdash", alpha=.4,
                        data = MRS_CPSdf)
    }
  # add CPS
  if (is.logical(MRS_CPSdf$CPS) == F) {
    p <- p + geom_vline(aes(xintercept = CPS),
                        linetype = "longdash", alpha=.4,
                        data = MRS_CPSdf)
    }
  
  return(p)
  
  
  }

