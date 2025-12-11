#' @title pretty_relativities
#'
#' @description Creates a pretty html plot of model relativities including base Levels.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm}
#' @param plot_approx_ci Set to TRUE to include confidence intervals in summary table. Warning, can be computationally expensive.
#' @param relativity_transform String of the function to be applied to the model estimate to calculate the relativity, for example: 'exp(estimate)'. Default is for relativity to be 'exp(estimate)-1'.
#' @param relativity_label String of label to give to relativity column if you want to change the title to your use case, some users may prefer to refer to this as odds ratio.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the fitted factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param width Width of plot
#' @param height Height of plot
#' @param iteractionplottype If plotting the relativity for an interaction variable you can "facet" or "colour" by one of the interaction variables. Defaults to null.
#' @param facetorcolourby If iteractionplottype is not Null, then this is the variable in the interaction you want to colour or facet by.
#' @param upper_percentile_to_cut For continuous variables this is what percentile to exclude from the upper end of the distribution. Defaults to 0.01, so the maximum percentile of the variable in the plot will be 0.99. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param lower_percentile_to_cut For continuous variables this is what percentile to exclude from the lower end of the distribution. Defaults to 0.01, so the mimimum percentile of the variable in the plot will be 0.01. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param spline_seperator string of the spline separator. For example AGE_0_25 would be "_".
#'
#' @return plotly plot of fitted relativities.
#'
#' @examples
#' library(dplyr)
#' library(prettyglm)
#' data('titanic')
#'
#' columns_to_factor <- c('Pclass',
#'                        'Sex',
#'                        'Cabin',
#'                        'Embarked',
#'                        'Cabintype',
#'                        'Survived')
#' meanage <- base::mean(titanic$Age, na.rm=TRUE)
#'
#' titanic  <- titanic  %>%
#'   dplyr::mutate_at(columns_to_factor, list(~factor(.))) %>%
#'   dplyr::mutate(Age =base::ifelse(is.na(Age)==TRUE,meanage,Age)) %>%
#'   dplyr::mutate(Age_0_25 = prettyglm::splineit(Age,0,25),
#'                 Age_25_50 = prettyglm::splineit(Age,25,50),
#'                 Age_50_120 = prettyglm::splineit(Age,50,120)) %>%
#'   dplyr::mutate(Fare_0_250 = prettyglm::splineit(Fare,0,250),
#'                 Fare_250_600 = prettyglm::splineit(Fare,250,600))
#'
#' survival_model3 <- stats::glm(Survived ~
#'                                 Pclass:Embarked +
#'                                 Age_0_25  +
#'                                 Age_25_50 +
#'                                 Age_50_120  +
#'                                 Sex:Fare_0_250 +
#'                                 Sex:Fare_250_600 +
#'                                 SibSp +
#'                                 Parch +
#'                                 Cabintype,
#'                               data = titanic,
#'                               family = binomial(link = 'logit'))
#'
#' # categorical factor
#' pretty_relativities(feature_to_plot = 'Cabintype',
#'                     model_object = survival_model3)
#'
#' # continuous factor
#' pretty_relativities(feature_to_plot = 'Parch',
#'                     model_object = survival_model3)
#'
#' # splined continuous factor
#' pretty_relativities(feature_to_plot = 'Age',
#'                     model_object = survival_model3,
#'                     spline_seperator = '_',
#'                     upper_percentile_to_cut = 0.01,
#'                     lower_percentile_to_cut = 0.01)
#'
#' # factor factor interaction
#' pretty_relativities(feature_to_plot = 'Pclass:Embarked',
#'                     model_object = survival_model3,
#'                     iteractionplottype = 'colour',
#'                     facetorcolourby = 'Pclass')
#'
#' # Continuous spline and categorical by colour
#' pretty_relativities(feature_to_plot = 'Sex:Fare',
#'                     model_object = survival_model3,
#'                     spline_seperator = '_')
#'
#' # Continuous spline and categorical by facet
#' pretty_relativities(feature_to_plot = 'Sex:Fare',
#'                     model_object = survival_model3,
#'                     spline_seperator = '_',
#'                     iteractionplottype = 'facet')
#' @export
#' @importFrom tibble "tibble"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyselect "contains"
#' @importFrom tidyr "pivot_longer"
#' @importFrom RColorBrewer "brewer.pal"
#' @import dplyr
#' @import plotly
#'

pretty_relativities <- function(feature_to_plot, model_object, plot_approx_ci = TRUE, relativity_transform = 'exp(estimate)-1', relativity_label = 'Relativity', ordering = NULL, plot_factor_as_numeric = FALSE, width = 800, height = 500, iteractionplottype = NULL, facetorcolourby = NULL, upper_percentile_to_cut = 0.01, lower_percentile_to_cut = 0, spline_seperator = NULL){
  # Fix for global variables
  # tidy_workflow <- NULL
  # Variable <- NULL
  # Relativity <- NULL
  # relativity <- NULL
  # Std.error <- NULL
  # Approx_Upper_95CI <- NULL
  # Approx_Lower_95CI <- NULL
  # name <- NULL
  # number_of_records <- NULL

  # Create relativity function from input
  base::eval(base::parse(text = base::paste('relativity <- function(estimate) { return(' , relativity_transform , ')}', sep='')))

  # Tidy model coefficients
  complete_factor_summary_df <- prettyglm::pretty_coefficients(model_object = model_object, relativity_transform = relativity_transform, return_data = T, spline_seperator = spline_seperator)

  # Extract training data from model object
  if (base::any(class(model_object) == 'workflow')){
    # workflow model objects here
    training_data <- tidy_workflow$fit$fit$fit$data
  } else if(base::any(class(model_object) == 'model_fit')){
    # pasnip model objects here
    training_data <- model_object$fit$data
  } else{
    #stats::glm objects here
    training_data <- model_object$data
  }

  # make sure the variable is in the p-file
  if (base::nrow(base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)))==0){
    # try switch order for interaction terms
    base::unlist(base::strsplit(feature_to_plot, ':'))[1]
    base::unlist(base::strsplit(feature_to_plot, ':'))[2]
    new_feature_to_plot <- base::paste0(base::unlist(base::strsplit(feature_to_plot, ':'))[2], ":", base::unlist(base::strsplit(feature_to_plot, ':'))[1])
    if (base::nrow(base::unique(dplyr::filter(complete_factor_summary_df, Variable == new_feature_to_plot)))>0){
      feature_to_plot <- new_feature_to_plot
    } else{
      # else throw error
      base::stop(base::paste(feature_to_plot , "not found in p-file. Try one of the following:", base::paste(unique(complete_factor_summary_df$Variable), collapse = " ")))
    }
  }

  if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factormain") {
    # Discrete variables -------------------------------------------------------------------------
    # Filter to the variable we are plotting
    plot_data <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot)

    # Get the number of records in each category
    factor_name <- base::unique(dplyr::pull(dplyr::select(plot_data, Variable)))
    count_df <- dplyr::select(training_data, tidyselect::all_of(factor_name)) %>%
      dplyr::group_by_at(tidyselect::all_of(factor_name)) %>%
      dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
      dplyr::ungroup()
    count_df <- count_df %>% dplyr::mutate(Variable = base::rep(factor_name, base::nrow(count_df))) %>%
      dplyr::rename(Level = factor_name)
    plot_data <- dplyr::left_join(plot_data, count_df, by = c('Level' = 'Level', 'Variable' = 'Variable'))

    # Change the variable to numeric for plotting if needed
    if (plot_factor_as_numeric == TRUE){
      plot_data <- plot_data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of('Level')), ~ as.numeric(as.character(dplyr::pull(dplyr::select(plot_data, tidyselect::all_of('Level'))))))
    }

    # Change ordering if specified
    if (base::is.null(ordering) ==  FALSE){
      if (base::length(ordering) >1){
        order_option <- ordering
        plot_data <- plot_data %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~factor(. ,Levels = order_option))
      } else{
        if (ordering == 'alphabetical'){
          order_option <-  'Level'
        } else if (ordering == 'relativity'){
          order_option <- 'Relativity'
        } else if (ordering == 'pvalue'){
          order_option <- 'P.Value'
        } else{
          base::warning('You have entered an incorrect ordering option. Please enter: alphabetical, relativity, pvalue or a vector of level names')
        }
        plot_data <- plot_data %>%
          dplyr::arrange(get(order_option)) %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~base::factor(., base::unique(.)))
      }
    } else{
      if (plot_factor_as_numeric == FALSE & (base::is.null(ordering) ==  FALSE) == FALSE){
        # if not other ordering, and no interaction, then order by the factor levels of the training dataset
        if (base::grepl(":", factor_name) == F){
          plot_data <- plot_data %>% dplyr::mutate_at(.vars = c("Level"),
                                                      .funs = ~factor(., levels = base::levels(dplyr::pull(dplyr::select(training_data, tidyselect::all_of(feature_to_plot))))))
        }
      }
    }

    if (plot_approx_ci == FALSE){
      p_return <- plot_data %>%
        plotly::plot_ly(height = height,
                        width = width) %>%
        plotly::add_trace(x = ~Level,
                          y = ~Relativity,
                          type="scatter",
                          mode="lines+markers",
                          name = relativity_label,
                          line = list(width = 4, color = 'black'),
                          marker = list(color = 'black', size = 8),
                          yaxis = "y") %>%
        plotly::add_bars(
          x = ~Level,
          y = ~number_of_records,
          yaxis = 'y2',
          marker = list(color = '#dddddd',
                        line = list(width=0,
                                    color='black')),
          showlegend = FALSE
        ) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot),
                       yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = FALSE),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    title = relativity_label, #relativity_label
                                    showgrid = TRUE),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.2),
                       autosize = TRUE,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)

    } else{
      # add confidence interval of 2* the standard error
      plot_data <- plot_data %>%
        dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                      Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))  #%>%
      # Create plot
      p_return <- plot_data %>%
        plotly::plot_ly(height = height,
                        width = width) %>%
        plotly::add_trace(x = ~Level,
                          y = ~Relativity,
                          type="scatter",
                          mode="lines+markers",
                          name = relativity_label,
                          line = list(width = 4, color = 'black'),
                          marker = list(color = 'black', size = 8),
                          yaxis = "y") %>%
        plotly::add_trace(x = ~Level,
                          y = ~Approx_Upper_95CI,
                          type="scatter",
                          mode="lines+markers",
                          name = 'Approx Upper 95 CI',
                          line = list(width = 4, color = 'grey',  dash = 'dash'),
                          marker = list(color = 'grey', size = 8),
                          yaxis = "y") %>%
        plotly::add_trace(x = ~Level,
                          y = ~Approx_Lower_95CI,
                          type="scatter",
                          mode="lines+markers",
                          name = 'Approx Lower 95 CI',
                          line = list(width = 4, color = 'grey',  dash = 'dash'),
                          marker = list(color = 'grey', size = 8),
                          yaxis = "y") %>%
        plotly::add_bars(
          x = ~Level,
          y = ~number_of_records,
          yaxis = 'y2',
          marker = list(color = '#dddddd',
                        line = list(width=0,
                                    color='black')),
          showlegend = FALSE
        ) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot),
                       yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = FALSE),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    title = relativity_label, #relativity_label
                                    showgrid = TRUE),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.2),
                       autosize = TRUE,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "ctsmain"){
    # Continuous Variables -----------------------------------------------------
    if (plot_approx_ci == T){
      complete_factor_summary_df <- complete_factor_summary_df %>%
        dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                      Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))  %>%
        tidyr::pivot_longer(cols = c(Relativity, Approx_Upper_95CI, Approx_Lower_95CI)) %>%
        dplyr::filter(., Variable == feature_to_plot) %>%
        dplyr::select(name,value)

      plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100),
                                  relativity_value = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Relativity'), 'value')),
                                  upper_95_ci = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Approx_Upper_95CI'), 'value')),
                                  lower_95_ci = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Approx_Lower_95CI'), 'value'))) %>%
        dplyr::mutate(feature_relativity = var_range*relativity_value) %>%
        dplyr::mutate(feature_upper_95_ci = var_range*upper_95_ci) %>%
        dplyr::mutate(feature_lower_95_ci = var_range*lower_95_ci) %>%
        dplyr::select(-c(relativity_value, upper_95_ci,lower_95_ci))

      # plot density and relativity
      fit <- training_data %>%
        dplyr::select(., all_of(feature_to_plot)) %>%
        dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
        dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- plotly::plot_ly(plot_data,
                                  height = height,
                                  width = width) %>%
        plotly::add_trace(x = ~var_range,
                          y = ~feature_relativity,
                          type="scatter",
                          mode="lines",
                          name = relativity_label,
                          line = list(width = 4, color = 'black'),
                          yaxis = "y2") %>%
        plotly::add_trace(x = ~var_range,
                          y = ~feature_upper_95_ci,
                          type="scatter",
                          mode="lines",
                          name = 'Approx Upper 95 CI',
                          line = list(width = 4, color = 'grey',  dash = 'dash'),
                          yaxis = "y2") %>%
        plotly::add_trace(x = ~var_range,
                          y = ~feature_lower_95_ci,
                          type="scatter",
                          mode="lines",
                          name = 'Approx Lower 95 CI',
                          line = list(width = 4, color = 'grey',  dash = 'dash'),
                          yaxis = "y2") %>%
        plotly::add_trace(x = fit$x,
                          y = fit$y,
                          type = "scatter",
                          mode = "lines",
                          fill = "tozeroy",
                          yaxis = "y",
                          name = "Density",
                          fillcolor = 'rgba(221,221,221,0.5)',
                          line  = list(color = 'rgba(221,221,221,0.7)')) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Density',
                                    zeroline = FALSE),
                       yaxis2 = list(side = 'left',
                                     title = relativity_label,
                                     showgrid = F,
                                     zeroline = FALSE,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.12,
                                     title = ''),
                       xaxis = list(title = feature_to_plot,
                                    zeroline = FALSE),
                       title = base::paste(relativity_label, 'for', feature_to_plot),
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    } else {
      # prep the data
      plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100),
                                  relativity_value = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot), 'Relativity'))) %>%
        dplyr::mutate(feature_relativity = var_range*relativity_value)

      # plot density and relativity
      fit <- training_data %>%
        dplyr::select(., all_of(feature_to_plot)) %>%
        dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
        dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- plotly::plot_ly(plot_data,
                                  height = height,
                                  width = width) %>%
        plotly::add_trace(x = ~var_range,
                          y = ~feature_relativity,
                          type="scatter",
                          mode="lines",
                          name = relativity_label,
                          line = list(color = 'black', width = 4),
                          yaxis = "y2") %>%
        plotly::add_trace(x = fit$x,
                          y = fit$y,
                          type = "scatter",
                          mode = "lines",
                          fill = "tozeroy",
                          yaxis = "y",
                          name = "Density",
                          fillcolor = 'rgba(221,221,221,0.5)',
                          line  = list(color = 'rgba(221,221,221,0.7)')) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Density',
                                    zeroline = FALSE),
                       yaxis2 = list(side = 'left',
                                     title = relativity_label,
                                     showgrid = F,
                                     zeroline = FALSE,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.32,
                                     title = ''),
                       xaxis = list(title = feature_to_plot,
                                    zeroline = FALSE),
                       title = base::paste(relativity_label, 'for', feature_to_plot),
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factorfactorinteraction"){
    # Factor Factor interactions --------------------------------------------------------------------
    # Filter to the interaction we are plotting
    plot_data <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot)

    # Get the number of records in each category
    factor_name <- base::unique(dplyr::pull(dplyr::select(plot_data, Variable)))

    # Count number in each group
    ivariable1 <- base::unlist(base::strsplit(factor_name, ':'))[1]
    ivariable2 <- base::unlist(base::strsplit(factor_name, ':'))[2]
    count_df <- dplyr::select(training_data, c(tidyselect::all_of(ivariable1),tidyselect::all_of(ivariable2))) %>%
        dplyr::group_by_at(c(ivariable1,ivariable2)) %>%
        dplyr::summarise(number_of_records = dplyr::n(), .groups = 'drop') %>%
        dplyr::ungroup()
    count_df <- count_df %>%
        dplyr::mutate(Variable = factor_name) %>%
        dplyr::mutate(Level = base::paste0(base::get(ivariable1),':',base::get(ivariable2))) %>%
        dplyr::select(.,c('Level', 'number_of_records', 'Variable'))

    plot_data <- dplyr::left_join(plot_data, count_df, by = c('Level' = 'Level', 'Variable' = 'Variable'))

    # Change ordering if specified
    if (base::is.null(ordering) ==  FALSE){
      if (base::length(ordering) >1){
        order_option <- ordering
        plot_data <- plot_data %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~factor(. ,Levels = order_option))
      } else{
        if (ordering == 'alphabetical'){
          order_option <-  'Level'
        } else if (ordering == 'relativity'){
          order_option <- 'Relativity'
        } else if (ordering == 'pvalue'){
          order_option <- 'P.Value'
        } else{
          base::warning('You have entered an incorrect ordering option. Please enter: alphabetical, relativity, pvalue or a vector of level names')
        }
        plot_data <- plot_data %>%
          dplyr::arrange(get(order_option)) %>%
          dplyr::mutate_at(.vars = c('Level'), .funs = ~base::factor(., base::unique(.)))
      }
    } else{
      if (plot_factor_as_numeric == FALSE & (base::is.null(ordering) ==  FALSE) == FALSE){
        # if not other ordering, and no interaction, then order by the factor levels of the training dataset
        if (base::grepl(":", factor_name) == F){
          plot_data <- plot_data %>% dplyr::mutate_at(.vars = c("Level"),
                                                      .funs = ~factor(., levels = base::levels(dplyr::pull(dplyr::select(training_data, tidyselect::all_of(feature_to_plot))))))
        } else{
          # For factors interactions default to ordering by level
          plot_data <- plot_data %>%
            dplyr::mutate(Level = as.factor(Level)) %>%
            dplyr::arrange(Level)
        }
      }
    }

    # add confidence interval of 2* the standard error
    plot_data <- plot_data %>%
      dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                    Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))

    # Create plot based on interaction facet / colouring options
    if (is.null(iteractionplottype) == T){
      if (plot_approx_ci == FALSE){
        p_return <- plot_data %>%
          plotly::plot_ly(height = height,
                          width = width) %>%
          plotly::add_trace(x = ~Level,
                            y = ~Relativity,
                            type="scatter",
                            mode="lines+markers",
                            name = relativity_label,
                            line = list(width = 4, color = 'black'),
                            marker = list(color = 'black', size = 8),
                            yaxis = "y") %>%
          plotly::add_bars(
            x = ~Level,
            y = ~number_of_records,
            yaxis = 'y2',
            marker = list(color = '#dddddd',
                          line = list(width=0,
                                      color='black')),
            showlegend = FALSE
          ) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot),
                         yaxis2 = list(side = 'right',
                                       title = 'Number of Records',
                                       showgrid = FALSE),
                         yaxis = list(overlaying='y2',
                                      side = 'left',
                                      title = relativity_label, #relativity_label
                                      showgrid = TRUE),
                         legend = list(orientation = "h",
                                       xanchor = "center",
                                       x=0.5,
                                       y=-0.2),
                         autosize = TRUE,
                         margin = list(b = 50, l = 50, r=80))
        return(p_return)
      } else{
        p_return <- plot_data %>%
          plotly::plot_ly(height = height,
                          width = width) %>%
          plotly::add_trace(x = ~Level,
                            y = ~Relativity,
                            type="scatter",
                            mode="lines+markers",
                            name = relativity_label,
                            line = list(width = 4, color = 'black'),
                            marker = list(color = 'black', size = 8),
                            yaxis = "y") %>%
          plotly::add_trace(x = ~Level,
                            y = ~Approx_Upper_95CI,
                            type="scatter",
                            mode="lines+markers",
                            name = 'Approx Upper 95 CI',
                            line = list(width = 4, color = 'grey',  dash = 'dash'),
                            marker = list(color = 'grey', size = 8),
                            yaxis = "y") %>%
          plotly::add_trace(x = ~Level,
                            y = ~Approx_Lower_95CI,
                            type="scatter",
                            mode="lines+markers",
                            name = 'Approx Lower 95 CI',
                            line = list(width = 4, color = 'grey',  dash = 'dash'),
                            marker = list(color = 'grey', size = 8),
                            yaxis = "y") %>%
          plotly::add_bars(
            x = ~Level,
            y = ~number_of_records,
            yaxis = 'y2',
            marker = list(color = '#dddddd',
                          line = list(width=0,
                                      color='black')),
            showlegend = FALSE
          ) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot),
                         yaxis2 = list(side = 'right',
                                       title = 'Number of Records',
                                       showgrid = FALSE),
                         yaxis = list(overlaying='y2',
                                      side = 'left',
                                      title = relativity_label, #relativity_label
                                      showgrid = TRUE),
                         legend = list(orientation = "h",
                                       xanchor = "center",
                                       x=0.5,
                                       y=-0.2),
                         autosize = TRUE,
                         margin = list(b = 50, l = 50, r=80))
        return(p_return)
      }
    } else if (iteractionplottype == 'facet'){
      # add columns of the variable names
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[1]] <- base::unlist(base::lapply(base::strsplit(as.character(plot_data$Level), ':'), `[[`, 1))
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[2]] <- base::unlist(base::lapply(base::strsplit(as.character(plot_data$Level), ':'), `[[`, 2))
      if (base::is.null(facetorcolourby)){
        base::stop("Please enter a valid input for facetorcolourby when iteractionplottype is not null")
      }
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(plot_data, dplyr::all_of(facetorcolourby)))))
      if (plot_approx_ci == FALSE){
        l <- 1
        plotlist <- list()
        while (l <= numberoffacets){
          facettoplot <- base::unique(dplyr::pull(dplyr::select(plot_data, dplyr::all_of(facetorcolourby))))[l]
          xaxisvariable <- stringr::str_remove(stringr::str_remove(factor_name, facetorcolourby),":")
          # Change the variable to numeric for plotting if needed
          if (plot_factor_as_numeric == TRUE){
            plot_data <- plot_data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(xaxisvariable)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(plot_data, tidyselect::all_of(xaxisvariable))))))
          }
          plotlist[[l]] <- plot_data %>%
            dplyr::filter(get(facetorcolourby) == facettoplot) %>%
            plotly::plot_ly(height = height,
                            width = width,
                            showlegend = base::ifelse(l==1,T,F)) %>%
            plotly::add_trace(x = ~get(xaxisvariable),
                              y = ~Relativity,
                              type="scatter",
                              mode="lines+markers",
                              name = relativity_label,
                              line = list(width = 4, color = 'black'),
                              marker = list(color = 'black', size = 8),
                              yaxis = "y2") %>%
            plotly::add_bars(
              x = ~get(xaxisvariable),
              y = ~number_of_records,
              yaxis = 'y',
              marker = list(color = '#dddddd',
                            line = list(width=0,
                                        color='black')),
              showlegend = FALSE
            ) %>%
            plotly::layout(title = base::paste(facettoplot),
                           yaxis = list(#overlaying='y2',
                             side = 'right',
                             title = 'Number of Records', #relativity_label
                             showgrid = TRUE
                           ),
                           yaxis2 = list(side = 'left',
                                         title = relativity_label,
                                         showgrid = FALSE,
                                         overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                           xaxis = list(title = xaxisvariable),
                           legend = list(orientation = "h",
                                         xanchor = "center",
                                         x=0.5,
                                         y=-0.2),
                           autosize = TRUE,
                           margin = list(b = 50, l = 50, r=80)) %>%
            plotly::add_annotations(
              x= 0.5,
              y= 1,
              xref = "paper",
              yref = "paper",
              text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
              showarrow = F
            )
          l <- l + 1

        }
        p_return <-
          plotly::subplot(plotlist,
                          nrows = numberoffacets,
                          titleY = T,
                          titleX = T,
                          margin = 0.07,
                          shareY = F,
                          shareX = T) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', factor_name, 'interaction', 'faceted by', facetorcolourby))
        return(p_return)
      } else {
        l <- 1
        plotlist <- list()
        while (l <= numberoffacets){
          facettoplot <- base::unique(dplyr::pull(dplyr::select(plot_data, dplyr::all_of(facetorcolourby))))[l]
          xaxisvariable <- stringr::str_remove(stringr::str_remove(factor_name, facetorcolourby),":")
          # Change the variable to numeric for plotting if needed
          if (plot_factor_as_numeric == TRUE){
            plot_data <- plot_data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(xaxisvariable)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(plot_data, tidyselect::all_of(xaxisvariable))))))
          }
          plotlist[[l]] <- plot_data %>%
            dplyr::filter(get(facetorcolourby) == facettoplot) %>%
            plotly::plot_ly(height = height,
                            width = width,
                            showlegend = base::ifelse(l==1,T,F)) %>%
            plotly::add_trace(x = ~get(xaxisvariable),
                              y = ~Relativity,
                              type="scatter",
                              mode="lines+markers",
                              name = relativity_label,
                              line = list(width = 4, color = 'black'),
                              marker = list(color = 'black', size = 8),
                              yaxis = "y2") %>%
            plotly::add_trace(x = ~get(xaxisvariable),
                              y = ~Approx_Upper_95CI,
                              type="scatter",
                              mode="lines+markers",
                              name = 'Approx Upper 95 CI',
                              line = list(width = 4, color = 'grey',  dash = 'dash'),
                              marker = list(color = 'grey', size = 8),
                              yaxis = "y2") %>%
            plotly::add_trace(x = ~get(xaxisvariable),
                              y = ~Approx_Lower_95CI,
                              type="scatter",
                              mode="lines+markers",
                              name = 'Approx Lower 95 CI',
                              line = list(width = 4, color = 'grey',  dash = 'dash'),
                              marker = list(color = 'grey', size = 8),
                              yaxis = "y2") %>%
            plotly::add_bars(
              x = ~get(xaxisvariable),
              y = ~number_of_records,
              yaxis = 'y',
              marker = list(color = '#dddddd',
                            line = list(width=0,
                                        color='black')),
              showlegend = FALSE
            ) %>%
            plotly::layout(title = base::paste(facettoplot),
                           yaxis = list(#overlaying='y2',
                             side = 'right',
                             title = 'Number of Records', #relativity_label
                             showgrid = TRUE
                           ),
                           yaxis2 = list(side = 'left',
                                         title = relativity_label,
                                         showgrid = FALSE,
                                         overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                           xaxis = list(title = xaxisvariable),
                           legend = list(orientation = "h",
                                         xanchor = "center",
                                         x=0.5,
                                         y=-0.2),
                           autosize = TRUE,
                           margin = list(b = 50, l = 50, r=80)) %>%
            plotly::add_annotations(
              x= 0.5,
              y= 1,
              xref = "paper",
              yref = "paper",
              text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
              showarrow = F
            )
          l <- l + 1

        }
        p_return <-
          plotly::subplot(plotlist,
                          nrows = numberoffacets,
                          titleY = T,
                          titleX = T,
                          margin = 0.07,
                          shareY = F,
                          shareX = T) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', factor_name, 'interaction', 'faceted by', facetorcolourby))
        return(p_return)
      }
    } else if (iteractionplottype == 'colour'){

      if (base::is.null(facetorcolourby)){
        base::stop("Please enter a valid input for facetorcolourby when iteractionplottype is not null")
      }

      # add columns of the variable names
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[1]] <- base::unlist(base::lapply(base::strsplit(as.character(plot_data$Level), ':'), `[[`, 1))
      plot_data[, base::unlist(base::strsplit(factor_name, ':'))[2]] <- base::unlist(base::lapply(base::strsplit(as.character(plot_data$Level), ':'), `[[`, 2))

      # aggregate the number of records
      xaxisvariable <- stringr::str_remove(stringr::str_remove(factor_name, facetorcolourby),":")
      # Change the variable to numeric for plotting if needed
      if (plot_factor_as_numeric == TRUE){
        plot_data <- plot_data %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(xaxisvariable)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(plot_data, tidyselect::all_of(xaxisvariable))))))
      }
      agg_number_to_plot <- plot_data %>%
        dplyr::group_by_at(xaxisvariable) %>%
        dplyr::summarise(number_of_records = sum(number_of_records), .groups = 'drop')

      plot_datafacet <- plot_data %>%
        dplyr::select(-('number_of_records')) %>%
        dplyr::left_join(agg_number_to_plot, by = xaxisvariable)

      p_return <- plot_datafacet %>%
        plotly::plot_ly(height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(xaxisvariable),
                          y = ~Relativity,
                          type="scatter",
                          mode="lines+markers",
                          color = ~get(facetorcolourby),
                          colors = RColorBrewer::brewer.pal((length(unique(dplyr::pull(dplyr::select(plot_datafacet, tidyselect::all_of(facetorcolourby)))))+1), "Set2")[1:length(unique(dplyr::pull(dplyr::select(plot_datafacet, tidyselect::all_of(facetorcolourby)))))],
                          name = ~get(facetorcolourby),
                          line = list(width = 4),
                          marker = list(size = 8)) %>%
        plotly::add_bars(
          x = ~get(xaxisvariable),
          y = ~number_of_records,
          yaxis = 'y2',
          marker = list(color = '#dddddd',
                        line = list(width=0,
                                    color='black')),
          showlegend = FALSE
        ) %>%
        plotly::layout(title = base::paste(relativity_label, 'for', factor_name, 'interaction', 'coloured by', facetorcolourby),
                       yaxis2 = list(side = 'right',
                                     title = 'Number of Records',
                                     showgrid = FALSE),
                       yaxis = list(overlaying='y2',
                                    side = 'left',
                                    title = relativity_label, #relativity_label
                                    showgrid = TRUE),
                       xaxis = list(title = xaxisvariable),
                       legend = list(orientation = "h",
                                     xanchor = "center",
                                     x=0.5,
                                     y=-0.2),
                       autosize = TRUE,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    } else{
      base::warning("iteractionplottype must be either 'colour', 'facet' or NULL. You have entered an invalid argument and no plot will be produced ")
    }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factorandctsinteraction"){
    if (is.null(iteractionplottype) == T){
      iteractionplottype <- 'colour'
      base::message("Factor and Continuous interaction detected, defualting to iteractionplottype = colour")
    }

    # separate the relativities
    ctsvariable <- base::unlist(base::strsplit(feature_to_plot, ':'))[2]
    factorvariable <- base::unlist(base::strsplit(feature_to_plot, ':'))[1]
    feature_rearragned <- base::paste0(factorvariable,":", ctsvariable)
    factor_levels <- base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned)$Level)

    if (iteractionplottype == 'facet'){
      complete_factor_summary_df <- complete_factor_summary_df %>%
        dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                      Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))
      if (plot_approx_ci == T){
        numberoffacets <- base::length(factor_levels)
        l <- 1
        plotlist <- list()
        while (l <= numberoffacets){
          facettoplot <- factor_levels[l]

          # get the fit for that facet
          fit <- dplyr::filter(training_data, get(factorvariable) == as.character(facettoplot))  %>%
            dplyr::select(., all_of(ctsvariable)) %>%
            dplyr::filter(base::get(ctsvariable) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
            dplyr::filter(base::get(ctsvariable) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
            dplyr::pull() %>%
            stats::density()

          # prep the plot data
          plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100),
                                      relativity_value = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Relativity")),
                                      upper_95_ci = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Approx_Upper_95CI")),
                                      lower_95_ci = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Approx_Lower_95CI")))  %>%
            dplyr::mutate(feature_relativity = var_range*relativity_value) %>%
            dplyr::mutate(feature_upper_95_ci = var_range*upper_95_ci) %>%
            dplyr::mutate(feature_lower_95_ci = var_range*lower_95_ci) %>%
            dplyr::select(-c(relativity_value, upper_95_ci,lower_95_ci))

          # Do the plot --------------------------------------------------------------------
          plotlist[[l]] <- plotly::plot_ly(plot_data,
                                           height = height,
                                           width = width,
                                           showlegend = base::ifelse(l==1,T,F)) %>%
            plotly::add_trace(x = ~var_range,
                              y = ~feature_relativity,
                              type="scatter",
                              mode="lines",
                              name = relativity_label,
                              line = list(color = 'black', width = 4),
                              yaxis = "y2") %>%
            plotly::add_trace(x = ~var_range,
                              y = ~feature_upper_95_ci,
                              type="scatter",
                              mode="lines",
                              name = 'Approx Upper 95 CI',
                              line = list(width = 4, color = 'grey',  dash = 'dash'),
                              yaxis = "y2") %>%
            plotly::add_trace(x = ~var_range,
                              y = ~feature_lower_95_ci,
                              type="scatter",
                              mode="lines",
                              name = 'Approx Lower 95 CI',
                              line = list(width = 4, color = 'grey',  dash = 'dash'),
                              yaxis = "y2") %>%
            plotly::add_trace(x = fit$x,
                              y = fit$y,
                              type = "scatter",
                              mode = "lines",
                              fill = "tozeroy",
                              yaxis = "y",
                              name = "Density",
                              fillcolor = 'rgba(221,221,221,0.5)',
                              line  = list(color = 'rgba(221,221,221,0.7)')) %>%
            plotly::layout(yaxis = list(side = 'right',
                                        title = 'Density',
                                        zeroline = FALSE),
                           yaxis2 = list(side = 'left',
                                         title = relativity_label,
                                         showgrid = F,
                                         zeroline = FALSE,
                                         overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                           legend = list(orientation = "h",
                                         y = -0.2,
                                         x = 0.15,
                                         title = ''),
                           xaxis = list(title = feature_to_plot,
                                        zeroline = FALSE),
                           title = base::paste(relativity_label, 'for', feature_to_plot),
                           autosize = T,
                           margin = list(b = 50, l = 50, r=80)) %>%
            plotly::add_annotations(
              x= 0.5,
              y= 1.05,
              xref = "paper",
              yref = "paper",
              text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
              showarrow = F
            )
          l <- l + 1
        }
        p_return <-
          plotly::subplot(plotlist,
                          nrows = numberoffacets,
                          titleY = T,
                          titleX = T,
                          margin = 0.07,
                          shareY = F,
                          shareX = T) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot, 'interaction', 'faceted by', factorvariable),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.15,
                                       title = ''))
        return(p_return)
      } else{
        numberoffacets <- base::length(factor_levels)
        l <- 1
        plotlist <- list()
        while (l <= numberoffacets){
          facettoplot <- factor_levels[l]

          # get the fit for that facet
          fit <- dplyr::filter(training_data, get(factorvariable) == as.character(facettoplot)) %>%
            dplyr::select(., tidyselect::all_of(ctsvariable)) %>%
            dplyr::filter(base::get(ctsvariable) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
            dplyr::filter(base::get(ctsvariable) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
            dplyr::pull() %>%
            stats::density()

          # prep the plot data
          plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100),
                                      relativity_value = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Relativity")),
                                      upper_95_ci = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Approx_Upper_95CI")),
                                      lower_95_ci = dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[l])), "Approx_Lower_95CI")))  %>%
            dplyr::mutate(feature_relativity = var_range*relativity_value) %>%
            dplyr::mutate(feature_upper_95_ci = var_range*upper_95_ci) %>%
            dplyr::mutate(feature_lower_95_ci = var_range*lower_95_ci) %>%
            dplyr::select(-c(relativity_value, upper_95_ci,lower_95_ci))

          # try the plotly plot!!!!!! --------------------------------------------------------------------
          plotlist[[l]] <- plotly::plot_ly(plot_data,
                                           height = height,
                                           width = width,
                                           showlegend = base::ifelse(l==1,T,F)) %>%
            plotly::add_trace(x = ~var_range,
                              y = ~feature_relativity,
                              type="scatter",
                              mode="lines",
                              name = relativity_label,
                              line = list(color = 'black', width = 4),
                              yaxis = "y2") %>%
            plotly::add_trace(x = fit$x,
                              y = fit$y,
                              type = "scatter",
                              mode = "lines",
                              fill = "tozeroy",
                              yaxis = "y",
                              name = "Density",
                              fillcolor = 'rgba(221,221,221,0.5)',
                              line  = list(color = 'rgba(221,221,221,0.7)')) %>%
            plotly::layout(yaxis = list(side = 'right',
                                        title = 'Density',
                                        zeroline = FALSE),
                           yaxis2 = list(side = 'left',
                                         title = relativity_label,
                                         showgrid = F,
                                         zeroline = FALSE,
                                         overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                           legend = list(orientation = "h",
                                         y = -0.2,
                                         x = 0.15,
                                         title = ''),
                           xaxis = list(title = feature_to_plot,
                                        zeroline = FALSE),
                           title = base::paste(relativity_label, 'for', feature_to_plot),
                           autosize = T,
                           margin = list(b = 50, l = 50, r=80)) %>%
            plotly::add_annotations(
              x= 0.5,
              y= 1.05,
              xref = "paper",
              yref = "paper",
              text = base::paste0('<b>',facettoplot, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
              showarrow = F
            )
          l <- l + 1
        }
        p_return <-
          plotly::subplot(plotlist,
                          nrows = numberoffacets,
                          titleY = T,
                          titleX = T,
                          margin = 0.07,
                          shareY = F,
                          shareX = T) %>%
          plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot, 'interaction', 'faceted by', factorvariable),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.15,
                                       title = ''))
        return(p_return)
      }
    } else if (iteractionplottype == 'colour'){
      # prep the data for plotting
      plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100))
      for (k in 1:length(factor_levels)){
        plot_data[,as.character(factor_levels[k])] <- dplyr::pull(dplyr::select(dplyr::filter(dplyr::filter(complete_factor_summary_df, Variable == feature_rearragned), Level == base::as.character(factor_levels[k])), "Relativity"))
        plot_data[,base::paste0(as.character(factor_levels[k])," ", relativity_label)] <- plot_data$var_range * plot_data[,as.character(factor_levels[k])]
      }
      plot_data <- plot_data %>%
        tidyr::pivot_longer(cols = tidyselect::contains(relativity_label),
                            names_to = "Level",
                            values_to = "feature_relativity")

      # plot density and relativity
      fit <- training_data %>%
        dplyr::select(., all_of(ctsvariable)) %>%
        dplyr::filter(base::get(ctsvariable) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
        dplyr::filter(base::get(ctsvariable) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(ctsvariable)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- plotly::plot_ly(plot_data,
                                  height = height,
                                  width = width) %>%
        plotly::add_trace(x = ~var_range,
                          y = ~feature_relativity,
                          type="scatter",
                          mode="lines",
                          color = ~Level,
                          colors = RColorBrewer::brewer.pal((length(unique(plot_data$Level))+1), "Set2")[1:length(unique(plot_data$Level))],
                          line = list(width = 4),
                          yaxis = "y2") %>%
        plotly::add_trace(x = fit$x,
                          y = fit$y,
                          type = "scatter",
                          mode = "lines",
                          fill = "tozeroy",
                          yaxis = "y",
                          name = "Density",
                          fillcolor = 'rgba(221,221,221,0.5)',
                          line  = list(color = 'rgba(221,221,221,0.7)')) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Density',
                                    zeroline = FALSE),
                       yaxis2 = list(side = 'left',
                                     title = relativity_label,
                                     showgrid = F,
                                     zeroline = FALSE,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.2,
                                     title = ''),
                       xaxis = list(title = feature_to_plot,
                                    zeroline = FALSE),
                       title = base::paste(relativity_label, 'for', feature_to_plot, 'coloured by', factorvariable),
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "ctsspline"){
    # spline no interaction
    spine_estimates <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot) %>%
      dplyr::mutate(SP_Min = stringr::word(Level,2,sep = spline_seperator)) %>%
      dplyr::mutate(SP_Max = stringr::word(Level,3,sep = spline_seperator)) %>%
      dplyr::mutate(SP_Max = stringr::word(Level,3,sep = spline_seperator))

    plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =1000))
    for (i in 1:nrow(spine_estimates)){
      New_col <- base::unlist(base::lapply(X = dplyr::pull(dplyr::select(plot_data, tidyselect::all_of('var_range'))), FUN = function(a) prettyglm::splineit(a, as.numeric(spine_estimates$SP_Min[i]), as.numeric(spine_estimates$SP_Max[i]))))
      New_col <- New_col*relativity((spine_estimates$Estimate[i]))
      plot_data <- plot_data %>%
        tibble::add_column(tibble(!!as.character(spine_estimates$Level[i]) := New_col))
    }

    plot_data <- plot_data %>% dplyr::mutate(feature_relativity = (base::rowSums(dplyr::select(., spine_estimates$Level))))#,
                                             #feature_relativity = relativity(base::rowSums(dplyr::select(., spine_estimates$Level))))


    # plot density and relativity
    fit <- training_data %>%
      dplyr::select(., all_of(feature_to_plot)) %>%
      dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
      dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
      dplyr::pull() %>%
      stats::density()

    p_return <- plotly::plot_ly(plot_data,
                                height = height,
                                width = width) %>%
      plotly::add_trace(x = ~var_range,
                        y = ~feature_relativity,
                        type="scatter",
                        mode="lines",
                        name = relativity_label,
                        line = list(color = 'black', width = 4),
                        yaxis = "y2") %>%
      plotly::add_trace(x = fit$x,
                        y = fit$y,
                        type = "scatter",
                        mode = "lines",
                        fill = "tozeroy",
                        yaxis = "y",
                        name = "Density",
                        fillcolor = 'rgba(221,221,221,0.5)',
                        line  = list(color = 'rgba(221,221,221,0.7)')) %>%
      plotly::layout(yaxis = list(side = 'right',
                                  title = 'Density',
                                  zeroline = FALSE),
                     yaxis2 = list(side = 'left',
                                   title = relativity_label,
                                   showgrid = F,
                                   zeroline = FALSE,
                                   overlaying = 'y'),
                     legend = list(orientation = "h",
                                   y = -0.2,
                                   x = 0.38,
                                   title = ''),
                     xaxis = list(title = feature_to_plot,
                                  zeroline = FALSE),
                     title = base::paste(relativity_label, 'for', feature_to_plot),
                     autosize = T,
                     margin = list(b = 50, l = 50, r=80))
    return(p_return)
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "factorandctsinteractionspline"){
     # get spline estimates
     spine_estimates <- complete_factor_summary_df %>%
      dplyr::filter(Variable == feature_to_plot) %>%
      dplyr::mutate(SP_Min = stringr::word(Level,2,sep = spline_seperator)) %>%
      dplyr::mutate(SP_Max = stringr::word(Level,3,sep = spline_seperator)) %>%
      dplyr::mutate(SP_Max = stringr::word(Level,3,sep = spline_seperator))

     if (is.null(iteractionplottype) == T){
       iteractionplottype <- 'colour'
       base::message("Factor and Continuous Spine interaction detected, defualting to iteractionplottype = colour")
     }

     if (iteractionplottype == 'facet'){
       l <- 1
       plotlist <- list()
       for (interactionk in base::unique(stringr::word(spine_estimates$Level,1,sep = ':'))){
         # Get the data for that interaction will need to change based on facet or colour
         for (g in 1:2) {
           if (base::class(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':')))) == 'numeric') {
             plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':'))), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':'))), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100))
             # get whatever is not g and then filter to that interaction
             ctsvariable <- stringr::word(feature_to_plot,g,sep = ':')
             otherg <- base::ifelse(g == 2,1,2)

             fit <- training_data %>%
               dplyr::filter(., base::get(stringr::word(feature_to_plot,otherg,sep = ':')) == interactionk) %>%
               dplyr::select(., all_of(stringr::word(feature_to_plot,g,sep = ':'))) %>%
               dplyr::filter(base::get(stringr::word(feature_to_plot,g,sep = ':')) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(stringr::word(feature_to_plot,g,sep = ':'))), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
               dplyr::filter(base::get(stringr::word(feature_to_plot,g,sep = ':')) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(stringr::word(feature_to_plot,g,sep = ':'))), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
               dplyr::pull() %>%
               stats::density()
           }
         }

         # Build the relativity curves
         spine_estimates_inside <- spine_estimates %>%
           dplyr::filter(stringr::word(Level,1,sep = ':') == interactionk)
         # check for empty plot data
         for (i in 1:nrow(spine_estimates_inside)){
           New_col <- base::unlist(base::lapply(X = dplyr::pull(dplyr::select(plot_data, tidyselect::all_of('var_range'))), FUN = function(a) prettyglm::splineit(a, as.numeric(spine_estimates_inside$SP_Min[i]), as.numeric(spine_estimates_inside$SP_Max[i]))))
           New_col <- New_col*relativity((spine_estimates_inside$Estimate[i]))
           plot_data <- plot_data %>%
             tibble::add_column(tibble(!!as.character(stringr::word(spine_estimates_inside$Level,2,sep = ':')[i]) := New_col))
         }

         plot_data <- plot_data %>% dplyr::mutate(feature_relativity = (base::rowSums(dplyr::select(., stringr::word(spine_estimates_inside$Level,2,sep = ':')))))#,
                                                  #feature_relativity = relativity(base::rowSums(dplyr::select(., stringr::word(spine_estimates_inside$Level,2,sep = ':')))))

         plotlist[[l]] <- plotly::plot_ly(plot_data,
                                          height = height,
                                          width = width,
                                          showlegend = base::ifelse(l==1,T,F)) %>%
           plotly::add_trace(x = ~var_range,
                             y = ~feature_relativity,
                             type="scatter",
                             mode="lines",
                             name = relativity_label,
                             line = list(color = 'black', width = 4),
                             yaxis = "y2") %>%
           plotly::add_trace(x = fit$x,
                             y = fit$y,
                             type = "scatter",
                             mode = "lines",
                             fill = "tozeroy",
                             yaxis = "y",
                             name = "Density",
                             fillcolor = 'rgba(221,221,221,0.5)',
                             line  = list(color = 'rgba(221,221,221,0.7)')) %>%
           plotly::layout(yaxis = list(side = 'right',
                                       title = 'Density',
                                       zeroline = FALSE),
                          yaxis2 = list(side = 'left',
                                        title = relativity_label,
                                        showgrid = F,
                                        zeroline = FALSE,
                                        overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                          legend = list(orientation = "h",
                                        y = -0.2,
                                        x = 0.15,
                                        title = ''),
                          xaxis = list(title = feature_to_plot,
                                       zeroline = FALSE),
                          title = base::paste(relativity_label, 'for', feature_to_plot),
                          autosize = T,
                          margin = list(b = 50, l = 50, r=80)) %>%
           plotly::add_annotations(
             x= 0.5,
             y= 1.05,
             xref = "paper",
             yref = "paper",
             text = base::paste0('<b>',interactionk, '<b>'),#"<b>paper reference = [0.5, 1]</b>",
             showarrow = F
           )
         l <- l + 1
       }
       numberoffacets <- l-1
       p_return <-
         plotly::subplot(plotlist,
                         nrows = numberoffacets,
                         titleY = T,
                         titleX = T,
                         margin = 0.07,
                         shareY = F,
                         shareX = T) %>%
         plotly::layout(title = base::paste(relativity_label, 'for', feature_to_plot, 'interaction', 'faceted by', stringr::word(feature_to_plot,otherg,sep = ':')),
                        legend = list(orientation = "h",
                                      y = -0.2,
                                      x = 0.35,
                                      title = ''))
       return(p_return)
     } else if (iteractionplottype == 'colour'){
       # Get the data for that interaction will need to change based on facet or colour
       for (g in 1:2) {
         if (base::class(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':')))) == 'numeric') {
           plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':'))), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::pull(dplyr::select(training_data, stringr::word(feature_to_plot,g,sep = ':'))), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100))
           # get whatever is not g and then filter to that interaction
           ctsvariable <- stringr::word(feature_to_plot,g,sep = ':')
           otherg <- base::ifelse(g == 2,1,2)

           fit <- training_data %>%
             dplyr::select(., all_of(stringr::word(feature_to_plot,g,sep = ':'))) %>%
             dplyr::filter(base::get(stringr::word(feature_to_plot,g,sep = ':')) <= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(stringr::word(feature_to_plot,g,sep = ':'))), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
             dplyr::filter(base::get(stringr::word(feature_to_plot,g,sep = ':')) >= as.numeric(stats::quantile(dplyr::select(training_data, tidyselect::all_of(stringr::word(feature_to_plot,g,sep = ':'))), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
             dplyr::pull() %>%
             stats::density()
         }
       }

       plot_data_record <- data.frame()
       for (interactionk in base::unique(stringr::word(spine_estimates$Level,1,sep = ':'))){
         # Build the relativity curves
         spine_estimates_inside <- spine_estimates %>%
           dplyr::filter(stringr::word(Level,1,sep = ':') == interactionk)
         # check for empty plot data
         plot_data_inside <- plot_data
         for (i in 1:nrow(spine_estimates_inside)){
           New_col <- base::unlist(base::lapply(X = dplyr::pull(dplyr::select(plot_data, tidyselect::all_of('var_range'))), FUN = function(a) prettyglm::splineit(a, as.numeric(spine_estimates_inside$SP_Min[i]), as.numeric(spine_estimates_inside$SP_Max[i]))))
           New_col <- New_col*relativity((spine_estimates_inside$Estimate[i]))
           plot_data_inside <- plot_data_inside %>%
             tibble::add_column(tibble(!!as.character(stringr::word(spine_estimates_inside$Level,2,sep = ':')[i]) := New_col))
         }
         plot_data_inside <- plot_data_inside %>% dplyr::mutate(feature_relativity = (base::rowSums(dplyr::select(., stringr::word(spine_estimates_inside$Level,2,sep = ':')))))#,
                                                                #feature_relativity = relativity(base::rowSums(dplyr::select(., stringr::word(spine_estimates_inside$Level,2,sep = ':')))))
         plot_data_inside$interaction <- interactionk
         plot_data_record <- rbind(plot_data_record,plot_data_inside)
       }
       p_return <- plotly::plot_ly(plot_data_record,
                                   height = height,
                                   width = width) %>%
         plotly::add_trace(x = ~var_range,
                           y = ~feature_relativity,
                           type="scatter",
                           mode="lines",
                           color = ~interaction,
                           colors = RColorBrewer::brewer.pal((length(unique(dplyr::pull(dplyr::select(plot_data_record, interaction))))+1), "Set2")[1:length(unique(dplyr::pull(dplyr::select(plot_data_record, interaction))))],
                           line = list(width = 4),
                           yaxis = "y2") %>%
         plotly::add_trace(x = fit$x,
                           y = fit$y,
                           type = "scatter",
                           mode = "lines",
                           fill = "tozeroy",
                           yaxis = "y",
                           name = "Density",
                           fillcolor = 'rgba(221,221,221,0.5)',
                           line  = list(color = 'rgba(221,221,221,0.7)')) %>%
         plotly::layout(yaxis = list(side = 'right',
                                     title = 'Density',
                                     zeroline = FALSE),
                        yaxis2 = list(side = 'left',
                                      title = relativity_label,
                                      showgrid = F,
                                      zeroline = FALSE,
                                      overlaying = 'y'),
                        legend = list(orientation = "h",
                                      y = -0.2,
                                      x = 0.35,
                                      title = ''),
                        xaxis = list(title = ctsvariable,
                                     zeroline = FALSE),
                        title = base::paste(relativity_label, 'for', feature_to_plot, 'coloured by', stringr::word(feature_to_plot,otherg,sep = ':')),
                        autosize = T,
                        margin = list(b = 50, l = 50, r=80))
       return(p_return)
     }
  } else if (base::unique(dplyr::filter(complete_factor_summary_df, Variable == feature_to_plot)$Effect) == "ctsctsinteraction"){
  #cts cts interaction ------------------------------------------------------------------
  complete_factor_summary_df <- complete_factor_summary_df %>%
    dplyr::mutate(Approx_Upper_95CI = (Relativity + 2*relativity(Std.error)),
                  Approx_Lower_95CI = (Relativity - 2*relativity(Std.error)))  %>%
    tidyr::pivot_longer(cols = c(Relativity, Approx_Upper_95CI, Approx_Lower_95CI)) %>%
    dplyr::filter(., Variable == feature_to_plot) %>%
    dplyr::select(name,value)

  variableone <- base::unlist(base::strsplit(feature_to_plot, ':'))[2]
  variabletwo <- base::unlist(base::strsplit(feature_to_plot, ':'))[1]

  product_data <- training_data %>%
    dplyr::select(tidyselect::all_of(variableone), tidyselect::all_of(variabletwo)) %>%
    dplyr::mutate(product = base::get(variableone)*base::get(variabletwo))

  fit <- dplyr::select(product_data, product) %>%
    dplyr::select(., c('product')) %>%
    dplyr::filter(product <= as.numeric(stats::quantile(dplyr::select(product_data, tidyselect::all_of('product')), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
    dplyr::filter(product >= as.numeric(stats::quantile(dplyr::select(product_data, tidyselect::all_of('product')), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
    dplyr::pull() %>%
    stats::density()

  plot_data <- tibble::tibble(var_range = base::seq(stats::quantile(dplyr::select(product_data, product), probs=c(lower_percentile_to_cut), na.rm = T), stats::quantile(dplyr::select(product_data, product), probs=c(1-upper_percentile_to_cut), na.rm = T),length.out =100),
                              relativity_value = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Relativity'), 'value')),
                              upper_95_ci = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Approx_Upper_95CI'), 'value')),
                              lower_95_ci = dplyr::pull(dplyr::select(dplyr::filter(complete_factor_summary_df, name == 'Approx_Lower_95CI'), 'value'))) %>%
    dplyr::mutate(feature_relativity = var_range*relativity_value) %>%
    dplyr::mutate(feature_upper_95_ci = var_range*upper_95_ci) %>%
    dplyr::mutate(feature_lower_95_ci = var_range*lower_95_ci) %>%
    dplyr::select(-c(relativity_value, upper_95_ci,lower_95_ci))

  if (plot_approx_ci == T){
    # plot density and relativity
    p_return <- plotly::plot_ly(plot_data,
                                height = height,
                                width = width) %>%
      plotly::add_trace(x = ~var_range,
                        y = ~feature_relativity,
                        type="scatter",
                        mode="lines",
                        name = relativity_label,
                        line = list(width = 4, color = 'black'),
                        yaxis = "y2") %>%
      plotly::add_trace(x = ~var_range,
                        y = ~feature_upper_95_ci,
                        type="scatter",
                        mode="lines",
                        name = 'Approx Upper 95 CI',
                        line = list(width = 4, color = 'grey',  dash = 'dash'),
                        yaxis = "y2") %>%
      plotly::add_trace(x = ~var_range,
                        y = ~feature_lower_95_ci,
                        type="scatter",
                        mode="lines",
                        name = 'Approx Lower 95 CI',
                        line = list(width = 4, color = 'grey',  dash = 'dash'),
                        yaxis = "y2") %>%
      plotly::add_trace(x = fit$x,
                        y = fit$y,
                        type = "scatter",
                        mode = "lines",
                        fill = "tozeroy",
                        yaxis = "y",
                        name = "Density",
                        fillcolor = 'rgba(221,221,221,0.5)',
                        line  = list(color = 'rgba(221,221,221,0.7)')) %>%
      plotly::layout(yaxis = list(side = 'right',
                                  title = 'Density',
                                  zeroline = FALSE),
                     yaxis2 = list(side = 'left',
                                   title = relativity_label,
                                   showgrid = F,
                                   zeroline = FALSE,
                                   overlaying = 'y'),
                     legend = list(orientation = "h",
                                   y = -0.2,
                                   x = 0.12,
                                   title = ''),
                     xaxis = list(title = feature_to_plot,
                                  zeroline = FALSE),
                     title = base::paste(relativity_label, 'for', feature_to_plot),
                     autosize = T,
                     margin = list(b = 50, l = 50, r=80))
    return(p_return)
  } else{
    # plot density and relativity
    p_return <- plotly::plot_ly(plot_data,
                                height = height,
                                width = width) %>%
      plotly::add_trace(x = ~var_range,
                        y = ~feature_relativity,
                        type="scatter",
                        mode="lines",
                        name = relativity_label,
                        line = list(width = 4, color = 'black'),
                        yaxis = "y2") %>%
      plotly::add_trace(x = fit$x,
                        y = fit$y,
                        type = "scatter",
                        mode = "lines",
                        fill = "tozeroy",
                        yaxis = "y",
                        name = "Density",
                        fillcolor = 'rgba(221,221,221,0.5)',
                        line  = list(color = 'rgba(221,221,221,0.7)')) %>%
      plotly::layout(yaxis = list(side = 'right',
                                  title = 'Density',
                                  zeroline = FALSE),
                     yaxis2 = list(side = 'left',
                                   title = relativity_label,
                                   showgrid = F,
                                   zeroline = FALSE,
                                   overlaying = 'y'),
                     legend = list(orientation = "h",
                                   y = -0.2,
                                   x = 0.12,
                                   title = ''),
                     xaxis = list(title = feature_to_plot,
                                  zeroline = FALSE),
                     title = base::paste(relativity_label, 'for', feature_to_plot),
                     autosize = T,
                     margin = list(b = 50, l = 50, r=80))
    return(p_return)
  }
  }
}

