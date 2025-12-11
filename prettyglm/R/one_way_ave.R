#' @title one_way_ave
#'
#' @description Creates a pretty html plot of one way actual vs expected by specified predictor.
#'
#' @param feature_to_plot A string of the variable to plot.
#' @param model_object Model object to create coefficient table for. Must be of type: \link[stats]{glm}, \link[stats]{lm}
#' @param target_variable String of target variable name in dataset.
#' @param data_set Data set to calculate the actual vs expected for. If no input default is to try and extract training data from model object.
#' @param plot_type one of "Residual", "predictions" or "actuals" defaults to "predictions"
#' @param plot_factor_as_numeric Set to TRUE to return \link[base]{data.frame} instead of creating \link[knitr]{kable}.
#' @param ordering Option to change the ordering of categories on the x axis, only for discrete categories. Default to the ordering of the factor. Other options are: 'alphabetical', 'Number of records', 'Average Value'
#' @param width Width of plot
#' @param height Height of plot
#' @param number_of_buckets Number of buckets for continuous variable plots
#' @param first_colour First colour to plot, usually the colour of actual.
#' @param second_colour Second colour to plot, usually the colour of predicted.
#' @param facetby Variable to facet the actual vs expect plots by.
#' @param prediction_type Prediction type to be pasted to predict.glm if predict_function is NULL. Defaults to "response".
#' @param predict_function A custom prediction function can be provided here.It must return a \link[base]{data.frame} with an "Actual_Values" column, and a "Predicted_Values" column.
#' @param upper_percentile_to_cut For continuous variables this is what percentile to exclude from the upper end of the distribution. Defaults to 0.01, so the maximum percentile of the variable in the plot will be 0.99. Cutting off some of the distribution can help the views if outlier's are present in the data.
#' @param lower_percentile_to_cut For continuous variables this is what percentile to exclude from the lower end of the distribution. Defaults to 0.01, so the minimum percentile of the variable in the plot will be 0.01. Cutting off some of the distribution can help the views if outlier's are present in the data.
#'
#' @return plotly plot of one way actual vs expected.
#'
#' @examples
#' library(dplyr)
#' library(prettyglm)
#' data('titanic')
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
#' # Continuous Variable Example
#' one_way_ave(feature_to_plot = 'Age',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic,
#'             number_of_buckets = 20,
#'             upper_percentile_to_cut = 0.1,
#'             lower_percentile_to_cut = 0.1)
#'
#' # Discrete Variable Example
#' one_way_ave(feature_to_plot = 'Pclass',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic)
#'
#' # Custom Predict Function and facet
#' a_custom_predict_function <- function(target, model_object, dataset){
#'   dataset <- base::as.data.frame(dataset)
#'   Actual_Values <- dplyr::pull(dplyr::select(dataset, tidyselect::all_of(c(target))))
#'   if(class(Actual_Values) == 'factor'){
#'     Actual_Values <- base::as.numeric(as.character(Actual_Values))
#'   }
#'   Predicted_Values <- base::as.numeric(stats::predict(model_object, dataset, type='response'))
#'
#'   to_return <-  base::data.frame(Actual_Values = Actual_Values,
#'                                  Predicted_Values = Predicted_Values)
#'
#'   to_return <- to_return %>%
#'     dplyr::mutate(Predicted_Values = base::ifelse(Predicted_Values > 0.3,0.3,Predicted_Values))
#'   return(to_return)
#' }
#'
#' one_way_ave(feature_to_plot = 'Age',
#'             model_object = survival_model,
#'             target_variable = 'Survived',
#'             data_set = titanic,
#'             number_of_buckets = 20,
#'             upper_percentile_to_cut = 0.1,
#'             lower_percentile_to_cut = 0.1,
#'             predict_function = a_custom_predict_function,
#'             facetby = 'Pclass')
#'
#'
#' @export
#' @importFrom tibble "tibble"
#' @importFrom tidyselect "all_of"
#' @importFrom tidyr "pivot_longer"
#' @importFrom stats "density"
#' @importFrom stats "predict"
#' @import dplyr
#' @import plotly
#'

one_way_ave <- function(feature_to_plot, model_object, target_variable, data_set, plot_type = 'predictions', plot_factor_as_numeric = FALSE, ordering = NULL, width = 800, height = 500, number_of_buckets = 30, first_colour = 'black', second_colour = '#cc4678', facetby = NULL, prediction_type = 'response', predict_function = NULL, upper_percentile_to_cut = 0.01, lower_percentile_to_cut = 0){
  # Make sure plots can handle residuals as a plot_type input

  # Extract the actual and expected values -------------------------------------------
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

  # tidy data for plotting different if factor or continuous -----------------------------------
  Plot_data <- dplyr::bind_cols(list(data_set, predicted_dataset))
  if (class(dplyr::select(Plot_data, tidyselect::matches(feature_to_plot))[[1]]) %in% c('factor','integer','character')){
    # categorical logic
    if (base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0) Plot_data <- Plot_data[-base::which(base::is.na(Plot_data$Predicted_Values)),]

    if (is.null(facetby)==F){
      # faceted code ---------------------------------------------------------------------------
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby)))))
      l <- 1
      plotlist <- list()
      while (l <= numberoffacets){
        facettoplot <- base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby))))[l]

        # prep the data--------------------------------------------------------------
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          dplyr::mutate(Residual = Actual-Predicted) %>%
          tidyr::pivot_longer(c(Actual, Predicted, Residual), names_to = 'Data_Type', values_to = 'value') %>%
          dplyr::group_by_at(c(feature_to_plot,'Data_Type')) %>%
          dplyr::summarise(Average_value = mean(value),
                           Number_of_Records = n(),
                           .groups = 'drop') %>%
          dplyr::ungroup()

        if (plot_type == 'residuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Residual')
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for',feature_to_plot)
        } else if (plot_type == 'predictions'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type != 'Residual') %>%
            dplyr::mutate(Number_of_Records = base::ifelse(Data_Type == 'Predicted',0,Number_of_Records))
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
        } else if (plot_type == 'actuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Actual')
          ylabeltext <- target_variable
          Plottitle <- paste('Actuals for',feature_to_plot)
        } else{
          print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
        }

        if (plot_factor_as_numeric ==TRUE){
          Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(feature_to_plot)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(feature_to_plot))))))
        }

        Count_data <- Plot_data_to_plot %>%
          dplyr::select(c(feature_to_plot, 'Number_of_Records')) %>%
          base::unique()

        plotlist[[l]] <- Plot_data_to_plot %>%
          plotly::plot_ly(colors = c(first_colour, second_colour),
                          height = height,
                          width = width,
                          showlegend = base::ifelse(l==1,T,F)) %>%
          plotly::add_trace(x = ~get(feature_to_plot),
                            y = ~Average_value,
                            type="scatter",
                            mode="lines+markers",
                            color = ~Data_Type,
                            yaxis = "y2") %>%
          plotly::add_bars(data = Count_data,
                           x=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of(feature_to_plot))),
                           y=dplyr::pull(dplyr::select(Count_data, tidyselect::all_of('Number_of_Records'))),
                           name= 'Number of records',
                           yaxis = 'y',
                           marker = list(color = '#dddddd',
                                         line = list(width=0,
                                                     color='black'))) %>%
          plotly::layout(yaxis = list(side = 'right',
                                      title = 'Number of Recrods'),
                         yaxis2 = list(side = 'left',
                                       title = ylabeltext,
                                       showgrid = F,
                                       overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.32,
                                       title = ''),
                         xaxis = list(title = feature_to_plot),
                         title = Plottitle,
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
                                  shareY = F,
                                  shareX = T)
    } else{
      Plot_data <- Plot_data %>%
        dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
        dplyr::rename(Actual = Actual_Values) %>%
        dplyr::rename(Predicted = Predicted_Values) %>%
        dplyr::mutate(Residual = Actual-Predicted) %>%
        tidyr::pivot_longer(c(Actual, Predicted, Residual), names_to = 'Data_Type', values_to = 'value') %>%
        dplyr::group_by_at(c(feature_to_plot,'Data_Type')) %>%
        dplyr::summarise(Average_value = mean(value),
                         Number_of_Records = n(),
                         .groups = 'drop') %>%
        dplyr::ungroup()

      # Create plots --------------------------------------------------------------------------------
      if (plot_type == 'residuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for',feature_to_plot)
      } else if (plot_type == 'predictions'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual') %>%
          dplyr::mutate(Number_of_Records = base::ifelse(Data_Type == 'Predicted',0,Number_of_Records))
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
      } else if (plot_type == 'actuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual for',feature_to_plot)
      } else{
        print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
      }

      if (plot_factor_as_numeric ==TRUE){
        Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(feature_to_plot)), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(feature_to_plot))))))
      }

      # Change ordering if specified -----------------------------------------------------------------
      if (base::is.null(ordering) ==  F){
        order_option <- ordering
        if (length(order_option) >1){
          Plot_data_to_plot <- Plot_data_to_plot %>%
            dplyr::mutate_at(.vars = c(feature_to_plot), .funs = ~base::factor(. , levels = order_option))
        } else{
          if (ordering == 'alphabetical'){
            order_option <-  feature_to_plot
          } else if (ordering == 'Number of records'){
            order_option <- 'Number_of_Records'
          } else if (ordering == 'Average Value'){
            order_option <- 'Average_value'
          } else{
            base::print('You have entered an incorrect ordering option. Please enter: alphabetical, Number of records, Average Value or a vector of level names')
          }
          Plot_data_to_plot <- Plot_data_to_plot %>%
            dplyr::arrange(get(order_option)) %>%
            dplyr::mutate_at(.vars = c(feature_to_plot), .funs = ~base::factor(., base::unique(.)))
        }
      }

      # Create plot
      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(colors = c(first_colour, second_colour),
                        height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(feature_to_plot),
                          y = ~Average_value,
                          type="scatter",
                          mode="lines+markers",
                          color = ~Data_Type,
                          yaxis = "y2") %>%
        plotly::add_bars(x=~get(feature_to_plot),
                         y=~Number_of_Records,
                         name= 'Number of records',
                         yaxis = 'y',
                         marker = list(color = '#dddddd',
                                       line = list(width=0,
                                                   color='black'))) %>%
        plotly::layout(yaxis = list(side = 'right',
                                    title = 'Number of Recrods'),
                       yaxis2 = list(side = 'left',
                                     title = ylabeltext,
                                     showgrid = F,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.3,
                                     title = ''),
                       xaxis = list(title = feature_to_plot),
                       title = Plottitle,
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  } else{
    # continuous logic  -------------------------------------------------------------------------
    if (base::length(base::which(base::is.na(Plot_data$Predicted_Values))) > 0) Plot_data <- Plot_data[-base::which(base::is.na(Plot_data$Predicted_Values)),]
    # default number of buckets for continuous variables is 30
    if(base::is.null(number_of_buckets) == T){
      number_of_buckets <- 30
    }
    # prep the data and cut the data
    Plot_data <- Plot_data%>%
      dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
      dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T)))

    Plot_data[,base::paste0(feature_to_plot,'_cat')] = prettyglm::cut3(x = dplyr::pull(dplyr::select(Plot_data, tidyselect::all_of(feature_to_plot))), g = number_of_buckets)
    if (is.null(facetby)==F){
      # faceted code ---------------------------------------------------------------------------
      numberoffacets <- base::length(base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby)))))
      l <- 1
      plotlist <- list()
      while (l <= numberoffacets){
        facettoplot <- base::unique(dplyr::pull(dplyr::select(Plot_data, dplyr::all_of(facetby))))[l]

        # prep the data
        Plot_data_inside <- Plot_data %>%
          dplyr::filter(get(facetby) == facettoplot) %>%
          dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
          dplyr::rename(Actual = Actual_Values) %>%
          dplyr::rename(Predicted = Predicted_Values) %>%
          dplyr::mutate(Residual = Actual-Predicted) %>%
          tidyr::pivot_longer(c(Actual, Predicted, Residual), names_to = 'Data_Type', values_to = 'value') %>%
          dplyr::group_by_at(c(base::paste0(feature_to_plot,'_cat'),'Data_Type')) %>%
          dplyr::summarise(Average_value = mean(value),
                           Number_of_Records = n(),
                           .groups = 'drop') %>%
          dplyr::ungroup()

        if (plot_type == 'residuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Residual')
          ylabeltext <- 'Residual'
          Plottitle <- paste('Residuals for',feature_to_plot)
        } else if (plot_type == 'predictions'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type != 'Residual') %>%
            dplyr::mutate(Number_of_Records = base::ifelse(Data_Type == 'Predicted',0,Number_of_Records))
          ylabeltext <- target_variable
          Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
        } else if (plot_type == 'actuals'){
          Plot_data_to_plot <- dplyr::filter(Plot_data_inside, Data_Type == 'Actual')
          ylabeltext <- target_variable
          Plottitle <- paste('Actual for',feature_to_plot)
        } else{
          print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
        }

        # plot factor as numeric is by default true
        Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(base::paste0(feature_to_plot,'_cat'))), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(base::paste0(feature_to_plot,'_cat')))))))

        # try the plotly plot!!!!!! --------------------------------------------------------------------
        density_data <- dplyr::bind_cols(list(data_set, predicted_dataset)) %>%
          dplyr::filter(get(facetby) == facettoplot)

        fit <- density_data %>%
          dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
          dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
          dplyr::select(tidyselect::all_of(feature_to_plot)) %>%
          dplyr::pull() %>%
          stats::density(.,na.rm = T)

        plotlist[[l]] <- Plot_data_to_plot %>%
          plotly::plot_ly(colors = c(first_colour, second_colour),
                          height = height,
                          width = width,
                          showlegend = base::ifelse(l==1,T,F)) %>%
          plotly::add_trace(x = ~get(base::paste0(feature_to_plot,'_cat')),
                            y = ~Average_value,
                            type="scatter",
                            mode="lines+markers",
                            color = ~Data_Type,
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
                                      title = 'Density'),
                         yaxis2 = list(side = 'left',
                                       title = ylabeltext,
                                       showgrid = F,
                                       overlaying = base::paste0('y', as.character((l-1)*2 + 1))),
                         legend = list(orientation = "h",
                                       y = -0.2,
                                       x = 0.32,
                                       title = ''),
                         xaxis = list(title = feature_to_plot),
                         title = Plottitle,
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
                                  shareY = F,
                                  shareX = T
      )

    } else{
      # non faceted code -----------------------------------------------------------------------
      Plot_data <- Plot_data %>%
        dplyr::mutate(Actual_Values = (as.numeric(Actual_Values))) %>%
        dplyr::rename(Actual = Actual_Values) %>%
        dplyr::rename(Predicted = Predicted_Values) %>%
        dplyr::mutate(Residual = Actual-Predicted) %>%
        tidyr::pivot_longer(c(Actual, Predicted, Residual), names_to = 'Data_Type', values_to = 'value') %>%
        dplyr::group_by_at(c(base::paste0(feature_to_plot,'_cat'),'Data_Type')) %>%
        dplyr::summarise(Average_value = mean(value),
                         Number_of_Records = n(),
                         .groups = 'drop') %>%
        dplyr::ungroup()

      if (plot_type == 'residuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Residual')
        ylabeltext <- 'Residual'
        Plottitle <- paste('Residuals for',feature_to_plot)
      } else if (plot_type == 'predictions'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type != 'Residual') %>%
          dplyr::mutate(Number_of_Records = base::ifelse(Data_Type == 'Predicted',0,Number_of_Records))
        ylabeltext <- target_variable
        Plottitle <- paste('Actual Vs Predicted for',feature_to_plot)
      } else if (plot_type == 'actuals'){
        Plot_data_to_plot <- dplyr::filter(Plot_data, Data_Type == 'Actual')
        ylabeltext <- target_variable
        Plottitle <- paste('Actual for',feature_to_plot)
      } else{
        print("plot_type must be one of: 'residuals', 'predictions' or 'actuals'")
      }

      # plot factor as numeric is by default true
      Plot_data_to_plot <- Plot_data_to_plot %>% dplyr::mutate_at(dplyr::vars(tidyselect::all_of(base::paste0(feature_to_plot,'_cat'))), ~ as.numeric(as.character(dplyr::pull(dplyr::select(Plot_data_to_plot, tidyselect::all_of(base::paste0(feature_to_plot,'_cat')))))))

      # plot the data
      density_data <- dplyr::select(dplyr::bind_cols(list(data_set, predicted_dataset)), all_of(feature_to_plot))
      fit <- density_data %>%
        dplyr::filter(base::get(feature_to_plot) <= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(1-upper_percentile_to_cut), na.rm = T))) %>%
        dplyr::filter(base::get(feature_to_plot) >= as.numeric(stats::quantile(dplyr::select(density_data, tidyselect::all_of(feature_to_plot)), probs=c(lower_percentile_to_cut), na.rm = T))) %>%
        dplyr::pull() %>%
        stats::density()

      p_return <- Plot_data_to_plot %>%
        plotly::plot_ly(colors = c(first_colour, second_colour),
                        height = height,
                        width = width,
                        showlegend = T) %>%
        plotly::add_trace(x = ~get(base::paste0(feature_to_plot,'_cat')),
                          y = ~Average_value,
                          type="scatter",
                          mode="lines+markers",
                          color = ~Data_Type,
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
                                    title = 'Density'),
                       yaxis2 = list(side = 'left',
                                     title = ylabeltext,
                                     showgrid = F,
                                     overlaying = 'y'),
                       legend = list(orientation = "h",
                                     y = -0.2,
                                     x = 0.32,
                                     title = ''),
                       xaxis = list(title = feature_to_plot),
                       title = Plottitle,
                       autosize = T,
                       margin = list(b = 50, l = 50, r=80))
      return(p_return)
    }
  }
  return(p_return)
}

