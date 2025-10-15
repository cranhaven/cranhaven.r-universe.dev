#' @title Histogram Plot for Numerical Variables
#' @description
#' This function generates histogram plots for all numerical variables in the input data frame.
#' It offers a vivid and effective visual summary of the distribution of each numerical variable,
#' helping in a quick understanding of their central tendency, spread, and shape.
#'
#' @param data The input data frame containing numerical variables.
#' @param fill The fill color for the histogram bars (default: "skyblue").
#' @param color The border color for the histogram bars (default: "black").
#' @param alpha The alpha (transparency) value for the histogram bars (default: 0.7).
#' @param subplot A logical argument (default: FALSE) indicating whether to create subplots for each variable.
#' @param nrow Number of rows for subplots (used when subplot is TRUE, default: 2).
#' @param margin Margin for subplots (used when subplot is TRUE, default: 0.1).
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 

#' @return A list of histogram plot.
#'
#' @examples
#' hist_plot(data = mtcars, fill = "skyblue", color = "black", alpha = 0.7, subplot = FALSE)
#' @importFrom htmltools tagList 
#' @import ggplot2
#' @import stats
#' @import magrittr
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' @importFrom plotly layout
#' @export
hist_plot <- function(data, fill = "skyblue", color = "black", alpha = 0.7, subplot = FALSE, nrow = 2, margin = 0.1,html=FALSE) {
  numerical <- names(Filter(is.numeric, data))
  if (length(numerical) == 0) stop("There is no numerical variable in the dataset")
  
  gg_list <- lapply(numerical, function(var) {
    binwidth <- density(data[[var]], bw = "SJ")$bw
    
    plot <- ggplot2::ggplot(data, aes(x = !!sym(var))) + 
      geom_histogram(binwidth = binwidth, fill = fill, color = color, alpha = alpha) +
      labs(title = paste(var, "Distribution"), y = "Frequency") +
      theme_minimal()
    
    ggplotly(plot, tooltip = "all", dynamicTicks = TRUE) %>% style(hoverinfo = "text") 
  })
  
  if (html) gg_list <- do.call(htmltools::tagList, gg_list)
  
  if (subplot) {
    fig <- plotly::subplot(gg_list, nrows = nrow, titleX = TRUE, titleY = FALSE, margin = margin) %>%
      style(hoverinfo = "text") %>%
      layout(title = list(text = "Distribution Histgram"))
  } else {
    fig <- gg_list
  }
  
  return(fig)
}


#' @title Numerical Variables Density Plots
#' @description
#' This function generates density plots for all numerical variables in the input data frame.
#' It offers a vivid and effective visual summary of the distribution of each numerical variable,
#' helping in a quick understanding of their central tendency, spread, and shape.
#' 
#' @param data The input data frame containing numerical variables.
#' @param fill The fill color of the density plot (default: "skyblue").
#' @param color The line color of the density plot (default: "black").
#' @param alpha The transparency of the density plot (default: 0.7).
#' @param subplot A logical argument (default: FALSE) indicating whether to create subplots.
#' @param nrow Number of rows for subplots (if subplot is TRUE, default: 2).
#' @param margin Margin for subplots (if subplot is TRUE, default: 0.1).
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 

#' @return A list of density plots.
#'
#' @examples
#' data(mtcars)
#' dens_plot(mtcars)
#'
#' @import ggplot2
#' @importFrom htmltools tagList
#' @import stats
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' @importFrom plotly layout
#' @export
dens_plot <- function(data, fill = "skyblue", color = "black", alpha = 0.7, subplot = FALSE, nrow = 2, margin = 0.1,html=FALSE) {
  numerical <- names(Filter(is.numeric, data))
  if (length(numerical) == 0) stop("There is no numerical variable in the dataset")
  
  gg_list <- lapply(numerical, function(var){
    plot <- ggplot(data, aes_string(x = var)) + 
      geom_density(fill = fill, color = color, alpha = alpha) +
      labs(title = paste(var, "Density Plot"), y = "Density") +
      theme_minimal()
    
    ggplotly(plot, tooltip = "all", dynamicTicks = TRUE) %>%
      plotly::style(hoverinfo = "text") 
  })
  
  if (html) gg_list <- do.call(htmltools::tagList, gg_list)
  
  if (subplot) {
    fig <- plotly::subplot(gg_list, nrows = nrow, titleX = TRUE, titleY = FALSE, margin = margin) %>%
      plotly::style(hoverinfo = "text") %>%
      plotly::layout(title = list(text = "Density Plots"))
  } else {
    fig <- gg_list
  }
  return(fig)
}



#' @title QQ Plots for Numerical Variables
#' @description
#' This function generates QQ plots for all numerical variables in the input data frame.
#' QQ plots are valuable for assessing the distributional similarity between observed data
#' and a theoretical normal distribution. It acts as a guide, revealing deviations from the 
#' expected norm, outliers, and the contours of distribution tails.
#'
#' @param data The input data frame containing numerical variables.
#' @param color The color of the QQ plot line (default: "skyblue").
#' @param subplot A logical argument (default: FALSE) indicating whether to create subplots.
#' @param nrow Number of rows for subplots (if subplot is TRUE, default: 2).
#' @param margin Margin for subplots (if subplot is TRUE, default: 0.1).
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 

#' @return A list of QQ plots.
#'
#' @examples
#' data(mtcars)
#' qq_plot(mtcars)
#'
#' @import ggplot2
#' @import htmltools
#' @import stats
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' @importFrom plotly layout
#' @export
qq_plot <- function(data, color = "skyblue", subplot = FALSE, nrow = 2, margin = 0.1,html=FALSE) {
  numerical <- names(Filter(is.numeric, data))
  if (length(numerical) == 0) stop("There is no numerical variable in the dataset")
  
  gg_list <- lapply(numerical, function(var) {
    plot <- ggplot(data, aes(sample = !!sym(var))) + 
      geom_qq() +
      geom_qq_line(color = color) +
      labs(title = paste(var, "QQ Plot"), subtitle = "Normal QQ Plot") +
      theme_minimal() 
    
    ggplotly(plot, tooltip = "all", dynamicTicks = TRUE) %>%
      plotly::style(hoverinfo = "text") %>%
      plotly::layout(title = list(text = paste(var, "QQ Plot")))
  })
  
  if (html) gg_list <- do.call(htmltools::tagList, gg_list)
  
  if (subplot) {
    fig <- plotly::subplot(gg_list, nrows = nrow, titleX = TRUE, titleY = FALSE, margin = margin) %>%
      plotly::style(hoverinfo = "text") %>%
      plotly::layout(title = list(text = ""))
  } else {
    fig <- gg_list
  }
  
  return(fig)
}



#' @title Bar Plots for Categorical Variables
#' @description
#' This function generates bar plots for all categorical variables in the input data frame.
#' Bar plots offer a visual representation of the distribution of categorical variables,
#' making it easy to understand the frequency of each category. They are particularly
#' useful for exploring patterns, identifying dominant categories, and comparing the relative
#' frequencies of different levels within each variable. 
#'
#' @param data The input data frame containing categorical variables.
#' @param fill Fill color for the bars (default: "skyblue").
#' @param color Border color of the bars (default: "black").
#' @param width Width of the bars (default: 0.7).
#' @param subplot A logical argument (default: FALSE) indicating whether to create subplots.
#' @param nrow Number of rows for subplots (if subplot is TRUE, default: 2).
#' @param margin Margin for subplots (if subplot is TRUE, default: 0.1).
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 

#' @return A list of bar plots.
#'
#' @examples
#' data(iris)
#' bar_plot(iris)
#'
#' @import ggplot2
#' @import htmltools
#' @import stats
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' @importFrom plotly layout
#' @export
bar_plot <- function(data, fill = "skyblue", color = "black", width = 0.7, subplot = FALSE, nrow = 2, margin = 0.1,html=FALSE) {
  categories = frequencies = NULL
  categorical <- names(Filter(function(x) is.factor(x) || is.character(x), data))
  if (length(categorical) == 0) stop("There is no categorical variable in the dataset")
  
  gg_list <- lapply(categorical, function(cat) {
    table_data <- table(data[[cat]])
    bar_df <- data.frame(categories = names(table_data), frequencies = as.numeric(table_data))
    
    plot <- ggplot2::ggplot(bar_df, aes(x = frequencies, y = reorder(categories, frequencies))) +
      geom_bar(stat = "identity", color = color, width = width, fill = fill) +
      labs(title = paste(cat, "Bar Plot"), x = "Frequency", y = cat) +
      theme_minimal()
    
    ggplotly(plot) %>%
      plotly::style(hoverinfo = "text") %>%
      plotly::layout(title = list(text = paste(cat, "Bar Plot")))
  })
  
  if (html) gg_list <- do.call(htmltools::tagList, gg_list)
  
  if (subplot) {
    fig <- plotly::subplot(gg_list, nrows = nrow, titleX = TRUE, titleY = FALSE, margin = margin) %>%
      plotly::style(hoverinfo = "text") %>%
      plotly::layout(title = list(text = ""))
    return(fig)
  } else {
    return(gg_list)
  }
}


#' @title Pie Plots for Categorical Variables
#' @description
#' This function generates pie charts for categorical variables in the input data frame using plotly.
#' Pie plots offer a visual representation of the distribution of categorical variables,
#' making it easy to understand the frequency of each category. They are particularly
#' useful for exploring patterns, identifying dominant categories, and comparing the relative
#' frequencies of different levels within each variable. 
#' 
#' @param data The input data frame containing categorical variables.
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 
#' @return A list of pie charts.
#'
#' @examples
#' data(iris)
#' pie_plot(iris)
#' @import htmltools
#' @import stats
#' @importFrom plotly plot_ly
#' @importFrom plotly layout
#' @export
pie_plot <- function(data,html=FALSE) {
  categorical <- names(Filter(function(x) is.factor(x) || is.character(x), data))
  if (length(categorical) == 0) stop("There is no categorical variable in the dataset")
  
  gg_list <- lapply(categorical, function(cat) {
    table_data <- table(data[[cat]])
    pie_df <- data.frame(categories = names(table_data), frequencies = as.numeric(table_data))
    
    plot_ly(pie_df, labels = ~categories, values = ~frequencies, type = 'pie',
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            textinfo = 'label+percent') %>%
      layout(title = paste(cat, "Pie Chart"))
    
  })
  if (html) gg_list <- do.call(htmltools::tagList,gg_list)
  return(gg_list)
}







