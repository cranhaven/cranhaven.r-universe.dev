#----- nlmeCurve ------
#######################--

#' Plot individual MNREAD fitted curves as estimated by a nonlinear mixed-effect (NLME) modeling. 
#' 
#' This function uses the NLME model created from \code{\link{nlmeModel}} to plot individual curves and Critical Print Size (CPS).
#'
#' @param nlme.model The object returned by \code{\link{nlmeModel}}
#' @param displayCPS Optional argument to display the CPS on the individuals curves. Default is TRUE. If set to FALSE, the CPS won't be plotted. 
#' @param CPScriterion Optional argument to specify a criterion for CPS estimation. The default criterion value is '90 of MRS'. This criterion can vary from 75 to 95 of MRS and should only be modified for specific purposes, as discussed in Cheung et al. 2008 
#'
#' @return 
#' The function returns a plot of reading speed (in log words/min) as a function of print size (in logMAR). 
#' If displayCPS is not specified, the Critical Print Size will be marked as an inverted triangle.
#'
#' @section Notes:
#' Print size shown on the plot(s) have been corrected for non-standard testing viewing distance.
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
#' \code{\link{mnreadCurve}} for standard MNREAD curve
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
#' # plot MNREAD curves and CPS with a default CPS criterion of '90 of MRS' 
#' \donttest{ nlmeCurve(nlme_model) }
#' 
#' # plot MNREAD curves without the CPS for a default CPS criterion of '90 of MRS' 
#' \donttest{ nlmeCurve(nlme_model, FALSE) }
#' 
#' # plot MNREAD curves and CPS with a specific CPS criterion of '80 of MRS'
#' \donttest{ nlmeCurve(nlme_model, TRUE, 0.8) }
#'
#' #------
#'
#' # Once created, the NLME curve can be further customized using ggplot2 
#'
#' # plot the NLME curve 
#' \donttest{ my_plot <- nlmeCurve(nlme_model) }
#'
#' # display my.plot
#' \donttest{ print(my_plot) }
#'
#' # modify my.plot
#' \donttest{ my_new_plot <- my_plot + 
#'  # overwrites the raw data points
#'     geom_point(data = nlme_model[[1]], aes(x=correct_ps, y = rs), size = 4) + 
#'  # changes the colors of the curve and raw data (effective only for nested designs)
#'     scale_color_brewer(palette="Set1") + 
#'  # changes the colors of the CPS diamond (effective only for nested designs)
#'     scale_fill_brewer(palette="Set1")  + 
#'  # modifies the aspect of the x-axis
#'     scale_x_continuous(breaks = seq (-0.5,2.5,0.4))  }
#' 
#' # display my.new.plot                                                                        
#' \donttest{ print(my_new_plot) }
#'
#'
#' #------
#'
#' # For very large datasets, it can be usefull to plot only selected facets to inspect individual fit
#' # To do so, one needs to restrict the dataframe called in each of the three layers of the plot
#' 
#' # list of subject names to keep
#' subjects_to_keep <- paste ("s", 1:4, sep = "")
#' 
#' # first filter the original data points (data called in the first layer)
#' \donttest{ my_plot$data <- my_plot$data %>%
#'     filter(subject %in% subjects_to_keep) %>%
#'     droplevels() }
#'     
#' # then filter the fitted data points (data called in the second layer)
#' \donttest{ my_plot$layers[[2]]$data <- my_plot$layers[[2]]$data %>% 
#'     filter(subject %in% subjects_to_keep) %>% 
#'     droplevels() }
#'   
#' # and finally, if 'displayCPS' was set to TRUE, filter the data used to display the CPS
#' \donttest{ my_plot$layers[[4]]$data <- my_plot$layers[[4]]$data %>% 
#'     filter(subject %in% subjects_to_keep) %>%
#'     droplevels() }
#'     
#' # plot the restricted my.plot
#' \donttest{ my_plot }
#'
#' #------
#' 
#' # It is also possible to export the curves in a pdf file running over several pages 
#' # and select the desired number of curves per page 
#' 
#' # set the desired number of subjects by page 
#' facet_nb = 4
#' 
#' # count the resulting number of pages 
#' num_pages = ceiling(length(unique(data_low_vision$subject))/facet_nb)
#'
#' # identify the list of subject names
#' subjects_to_plot <- unique(as.character(data_low_vision$subject))
#' 
#' # split the list into chunks the same size as the number of subjects per page
#' subjects_to_plot_splitted <- split(subjects_to_plot, ceiling(seq_along(subjects_to_plot)/facet_nb))
#' 
#' # create a pdf and wrap plots over several pages
#' \donttest{ pdf("nlme-MNREAD-curves.pdf",
#'               width = 10.5, height = 8,
#'               paper="USr", useDingbats=T)
#'               
#'               for (i in seq(num_pages))
#'               {
#'                  my.plot <- nlmeCurve(nlme_model, displayCPS = F) 
#'                  
#'                  # filter the original data points for the selected chunk of subjects
#'                  my.plot$data <- my.plot$data %>%
#'                  filter(subject %in% subjects_to_plot_splitted[[i]]) %>%
#'                  droplevels()
#'                  
#'                  # filter the fitted data points for the selected chunk of subjects
#'                  my.plot$layers[[2]]$data <- my.plot$layers[[2]]$data %>%
#'                  filter(subject %in% subjects_to_plot_splitted[[i]]) %>%
#'                  droplevels()
#'                  
#'                  print (my.plot + geom_line(colour = "red"))
#'               }
#'               
#'               dev.off()  }
#'               
#'               
#'
#' @importFrom stats sd coef predict
#' @importFrom tibble rownames_to_column
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#'  
#'
#' @export
nlmeCurve <- function(nlme.model, displayCPS = TRUE, CPScriterion = NULL) {

  asym <- NULL
  lrc <- NULL
  x_intercept <- NULL
  grp_var <- NULL
  rowname <- NULL
  MRS <- NULL
  CPS <- NULL
  fitted_rs <- NULL
  correct_ps <- NULL
  rs <- NULL
  subject <- NULL
  group_var <- NULL
  nested_var <- NULL
  predict.nested_var <- NULL
  predict.subject <- NULL
  . <- NULL
  
  # extract coefficients from the nlme model
  my.coef <- rownames_to_column(as.data.frame(coef(nlme.model[[2]])))
  
  # set the percentage of maximum reading speed we want to use
  # CPS is the smallest print size that yields p times the maximum reading speed
  if ( missing(CPScriterion) )  {
    CPS_crit = 0.90 }
  else {
    CPS_crit = CPScriterion }
  
  # get Maximum Reading Speed (MRS) and Critical Print Size (CPS)
  nlme.estimates <- my.coef %>% 
    mutate (MRS = 10^asym) %>%
    mutate (CPS = log((-log10(CPS_crit))/asym) / (-exp(lrc)) + x_intercept) %>%
    separate(rowname, into = c("subject", "nested_var"), sep = "/", fill = "right")
  
  # find the max CPS value to set the x-axis limit
  max_CPS <- max(nlme.estimates$CPS)
  
  # define the figure layout 
  p <- ggplot(data = nlme.model[[1]],
              aes(x = correct_ps, y = rs))
  p <- p + facet_wrap(~subject)
  p <- p + scale_x_continuous(name = "Corrected Print Size (logMAR)")
  # here I set: left y-axis = reading speed & right y-axis = log reading speed
  p <- p + scale_y_log10(name = "Reading Speed (words/min)", #position = "right", 
                         sec.axis = sec_axis(~ log10(.), 
                                             name = "Reading Speed (log words/min)"))
  p <- p + annotation_logticks(sides = "l", alpha = 0.4) 
  # to invert the left and right y-axes, I must set the folowing options:
  # scale_y_log10(position = "right") & annotation_logticks(sides = "r")
  
  # plot a non-nested design 
  if ( "nested_var" %in% names(nlme.model[[1]]) == FALSE  ) { 
    
    # extract the fitted values
    # add rows with missing data for incomplete datasets
    if (max_CPS > max(nlme.model[[1]]$correct_ps)) {
      full_data_to_plot <- nlme.model[[1]] %>% 
        complete (subject, correct_ps = seq(min(nlme.model[[1]]$correct_ps), max_CPS, 0.05)) %>% 
        select (subject, correct_ps)
    }
    else {
      full_data_to_plot <- nlme.model[[1]] %>% 
        complete (subject, correct_ps) %>% 
        select (subject, correct_ps)
    }
    # extract the fixed effects predictions at the subject level 
    fitted_df <- predict(nlme.model[[2]], full_data_to_plot, level = 0:1) # 0 = populations; 1 = subject
    # merge the fitted data with raw data saved as nlme.model[[1]]
    full_fitted_df <- bind_cols(full_data_to_plot, fitted_df) %>%
      filter (predict.subject >= 0)
    
    # plot the fitted curve
    p <- p + geom_line(aes(x = correct_ps, y = 10^predict.subject), 
                       size=1, alpha=0.7, show.legend = FALSE,
                       data = full_fitted_df)
    
    # plot the raw data
    if ( "group_var" %in% names(nlme.model[[1]]) == FALSE ) { 
      p <- p + geom_point() }
    else {  
      p <- p + geom_point(aes(shape = group_var)) }
    
    # add CPS
    if ( displayCPS == TRUE )  {
      p <- p + geom_point(aes(x = CPS, y = MRS),
                          shape = 25, size = 3, fill = "red",
                          data = nlme.estimates) }
    else {  }
    
    
  }
  
  # plot a nested design 
  if ( "nested_var" %in% names(nlme.model[[1]]) == TRUE ) {
    
    # extract the fitted values
    # add rows with missing data for incomplete datasets
    if (max_CPS > max(nlme.model[[1]]$correct_ps)) {
      full_data_to_plot <- nlme.model[[1]] %>% 
        complete (subject, nested_var, correct_ps = seq(min(nlme.model[[1]]$correct_ps), max_CPS, 0.05)) %>% 
        select (subject, nested_var, correct_ps)
    }
    else {
      full_data_to_plot <- nlme.model[[1]] %>% 
        complete (subject, nested_var, correct_ps) %>% 
        select (subject, nested_var, correct_ps)
    }
    # extract the fixed effects predictions at the nested variable level
    fitted_df <- predict(nlme.model[[2]], full_data_to_plot, level = 0:2) # 0 = populations; 1 = subject; 2 = nested_var
    # merge fitted data with raw data saved as nlme.model[[1]]
    full_fitted_df <- bind_cols(full_data_to_plot, fitted_df) %>%
      filter (predict.nested_var >= 0)
    
    # plot the fitted curve
    p <- p + geom_line(aes(x = correct_ps, y = 10^predict.nested_var, colour = nested_var), 
                       size=1, alpha=0.7, show.legend = FALSE,
                       data = full_fitted_df)
    
    # plot the raw data
    if ( "group_var" %in% names(nlme.model[[1]]) == FALSE ) {
      p <- p + geom_point(aes(colour = nested_var)) }
    else {
      p <- p + geom_point(aes(colour = nested_var, shape = group_var)) }
    
    # add CPS
    if ( displayCPS == TRUE )  {
      p <- p + geom_point(aes(x = CPS, y = MRS, fill = nested_var),
                          shape = 25, size = 3,
                          data = nlme.estimates) }
    else {  }
    
    
  }
  
  
  return(p)
  
}
