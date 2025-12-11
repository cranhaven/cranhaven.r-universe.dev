#' @title actual_expected_bucketed
#'
#' @description Provides a rank plot of the actual and predicted.
#'
#' @param target_variable String of target variable name.
#' @param model_object GLM model object.
#' @param data_set Data to score the model on. This can be training or test data, as long as the data is in a form where the model object can make predictions. Currently developing ability to provide custom prediction functions, currently implementation defaults to `stats::predict`
#' @param number_of_buckets number of buckets for percentile
#' @param ylab Y-axis label.
#' @param width plotly plot width in pixels.
#' @param height plotly plot height in pixels.
#' @param first_colour First colour to plot, usually the colour of actual.
#' @param second_colour Second colour to plot, usually the colour of predicted.
#' @param facetby variable user wants to facet by.
#' @param prediction_type Prediction type to be pasted to predict.glm if predict_function is NULL. Defaults to "response".
#' @param predict_function prediction function to use. Still in development.
#' @param return_data Logical to return cleaned data set instead of plot.
#'

#' @return plot
#' Plotly plot by defualt.
#' ggplot if plotlyplot = F.
#' Tibble if return_data = T.
#'
#' @examples
#'
#' library(dplyr)
#' library(prettyglm)
#'
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
#' survival_model <- stats::glm(Survived ~
#'                                Sex:Age +
#'                                Fare +
#'                                Embarked +
#'                                SibSp +
#'                                Parch +
#'                                Cabintype,
#'                              data = titanic,
#'                              family = binomial(link = 'logit'))
#'
#' prettyglm::actual_expected_bucketed(target_variable = 'Survived',
#'                                     model_object = survival_model,
#'                                     data_set = titanic)
#'
#' @export
#' @importFrom tibble "tibble"
#' @importFrom plotly "layout"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @import dplyr
#'


actual_expected_bucketed <- function(target_variable, model_object, data_set = NULL, number_of_buckets = 25, ylab = 'Target', width = 800, height = 500, first_colour = 'black', second_colour = '#cc4678', facetby = NULL, prediction_type = 'response', predict_function = NULL, return_data=F){
  # make predictions on data set --------------------------------------------------------------
  # if provided data set is null then use the training data from model object
  if (is.null(data_set)==T){
    # Extract training data from model object
    if (base::any(class(model_object) == 'workflow')){
      # Workflow model objects here
      data_set <- tidy_workflow$fit$fit$fit$data
    } else if(base::any(class(model_object) == 'model_fit')){
      # pasnip model objects here
      data_set <- model_object$fit$data
    } else{
      #stats::glm objects here
      data_set <- model_object$data
    }
  }

  # make predictions
  if (is.null(predict_function) == T){
    predicted_dataset <- prettyglm::predict_outcome(target = target_variable,
                                                    model_object = model_object,
                                                    dataset = data_set,
                                                    prediction_type = prediction_type)
  } else{
    #base::simpleError('Functionality for custom predict function not avaliable yet')
    predicted_dataset <- predict_function(target = target_variable,
                                          model_object = model_object,
                                          dataset = data_set)
  }

  if (is.null(facetby) == T){
    # tidy data for rank plot --------------------------------------------------------------------
    # Create tidy data to plot
    Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
      dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) # check if this needs to be made more generic
    Plot_data$Rank <- Plot_data %>%  dplyr::select(Predicted_Values) %>% dplyr::pull() %>% dplyr::ntile(number_of_buckets)
    Plot_data <- Plot_data %>%
      dplyr::mutate(Rank = Rank/number_of_buckets) %>%
      dplyr::rename(Actual = Actual_Values) %>%
      dplyr::rename(Predicted = Predicted_Values) %>%
      tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
      dplyr::group_by(Rank, Data_Type) %>%
      dplyr::summarise(Average_value = mean(value), .groups = "drop") %>%
      ungroup()

    # Create plot --------------------------------------------------------------------------------
    p_return <- plotly::plot_ly(Plot_data,
                                height = height,
                                width = width,
                                colors = c(first_colour, second_colour)) %>%
      plotly::add_trace(x = ~Rank,
                        y = ~Average_value,
                        type="scatter",
                        mode="lines",
                        color = ~Data_Type,
                        line = list(width = 4),
                        yaxis = "y2") %>%
      plotly::layout(yaxis2 = list(side = 'left',
                                   title = ylab,
                                   showgrid = T,
                                   zeroline = FALSE,
                                   overlaying = 'y'),
                     legend = list(orientation = "h",
                                   y = -0.2,
                                   x = 0.37,
                                   title = ''),
                     xaxis = list(title = 'Percentile',
                                  zeroline = FALSE),
                     title = "Actual vs Predicted by Predicted Band",
                     autosize = T,
                     margin = list(b = 50, l = 50, r=80))
  } else{
    # faceted code ---------------------------------------------------------------------------
    numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(dplyr::bind_cols(list(data_set, predicted_dataset)), dplyr::all_of(facetby)))))
    l <- 1
    plotlist <- list()
    while (l <= numberoffacets){
      facettoplot <- base::unique(dplyr::pull(dplyr::select(dplyr::bind_cols(list(data_set, predicted_dataset)), dplyr::all_of(facetby))))[l]

      # Create tidy data to plot
      Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
        dplyr::filter(get(facetby) == facettoplot) %>%
        dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) # check if this needs to be made more generic
      Plot_data$Rank <- Plot_data %>%  dplyr::select(Predicted_Values) %>% dplyr::pull() %>% dplyr::ntile(number_of_buckets)
      Plot_data <- Plot_data %>%
        dplyr::mutate(Rank = Rank/number_of_buckets) %>%
        dplyr::rename(Actual = Actual_Values) %>%
        dplyr::rename(Predicted = Predicted_Values) %>%
        tidyr::pivot_longer(c(Actual, Predicted), names_to = 'Data_Type', values_to = 'value') %>%
        dplyr::group_by(Rank, Data_Type) %>%
        dplyr::summarise(Average_value = mean(value), .groups = "drop") %>%
        ungroup()

      # make plot
      plotlist[[l]] <- plotly::plot_ly(Plot_data,
                                       height = height,
                                       width = width,
                                       showlegend = base::ifelse(l==1,T,F),
                                       colors = c(first_colour, second_colour)) %>%
        plotly::add_trace(x = ~Rank,
                          y = ~Average_value,
                          type="scatter",
                          mode="lines",
                          color = ~Data_Type,
                          line = list(width = 4),
                          yaxis = "y") %>%
        plotly::layout(yaxis = list(side = 'left',
                                    title = ylab,
                                    showgrid = T,
                                    zeroline = FALSE),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.37,
                                     title = ''),
                       xaxis = list(title = 'Percentile',
                                    zeroline = FALSE),
                       #title = "Actual vs Predicted by Predicted Band",
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
    p_return <- plotly::subplot(plotlist,
                                nrows = numberoffacets,
                                titleY = T,
                                titleX = T,
                                margin = 0.07,
                                shareY = T,
                                shareX = T)


  }
  # Return plot or data ------------------------------------------------------------------------
  if (return_data == F){
    return(p_return)
  } else{
    return(Plot_data)
  }
}
