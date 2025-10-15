#' @title Principal Component Analysis (PCA)
#'
#' @description
#'   This function performs Principal Component Analysis (PCA) on the input data, providing
#'   a detailed analysis of variance, eigenvalues, and eigenvectors. It offers options to generate
#'   a scree plot for visualizing variance explained by each principal component and a biplot to
#'   understand the relationship between variables and observations in reduced dimensions.
#'   
#'
#' @param data Numeric matrix or data frame containing the variables for PCA.
#' @param variance_threshold Proportion of total variance to retain (default: 0.90).
#' @param center Logical, indicating whether to center the data (default: TRUE).
#' @param scale Logical, indicating whether to scale the data (default: FALSE).
#' @param scree_plot Logical, whether to generate a scree plot (default: FALSE).
#' @param biplot Logical, whether to generate a biplot (default: FALSE).
#' @param choices Numeric vector of length 2, indicating the principal components to plot in the biplot.
#' @param groups Optional grouping variable for coloring points in the biplot.
#' @param length_scale Scaling factor for adjusting the length of vectors in the biplot (default: 1).
#' @param scree_legend Logical, indicating whether to show legend in scree plot (default: True).
#' @param scree_legend_pos A vector c(x, y) to adjust the position of the legend.
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 
#' @return 
#' A list containing:
#'  - summary_table: A matrix summarizing eigenvalues and cumulative variance explained.
#'  - scree_plot: A scree plot if scree_plot is TRUE.
#'  - biplot: A biplot if biplot is TRUE.
#'
#' @examples
#' data(mtcars)
#' pca_result <- pca(mtcars, scree_plot = TRUE, biplot = TRUE)
#' pca_result$summary_table
#' pca_result$scree_plot
#' pca_result$biplot
#'
#' @import ggplot2
#' @importFrom htmltools tagList
#' @importFrom DT datatable
#' @importFrom plotly ggplotly
#' @importFrom plotly add_text
#' @importFrom plotly add_trace
#' @export
pca <- function(data, 
                variance_threshold = 0.90, 
                center = TRUE, 
                scale = FALSE, 
                scree_plot = FALSE, 
                biplot = FALSE, 
                choices = 1:2, 
                groups = NULL,
                length_scale = 1,
                scree_legend = TRUE,
                scree_legend_pos = c(0.7,0.5),
                html = FALSE) {
  x = y = cum_y = cum_label = xvar = yvar = varname = angle = hjust = NULL
  pca_result <- prcomp(data, center = center, scale. = scale)
  eigenvalue <- (pca_result$sdev)^2
  eigenvector <- pca_result$rotation
  score <- pca_result$x
  
  proportion_variance_explained <- (eigenvalue) / sum(eigenvalue)
  num_components <- min(which(cumsum(proportion_variance_explained) >= variance_threshold))
  
  selected_eigenvalue <- eigenvalue[1:num_components]
  selected_eigenvector <- eigenvector[, 1:num_components]
  selected_score <- score[, 1:num_components]
  
  if (html) result <- htmltools::tagList() else result <- list()
  
  summary_table <- matrix(
    c(selected_eigenvalue, 
      selected_eigenvalue / sum(eigenvalue) * 100, 
      cumsum(selected_eigenvalue / sum(eigenvalue)) * 100),
    nrow = length(selected_eigenvalue),
    byrow = FALSE
  )
  rownames(summary_table) <- paste("PC", 1:nrow(summary_table))
  colnames(summary_table) <- c("Eigenvalue", "Variance Explained(%)", "Cumulative Variance Explained(%)")
  
  
  table1<-datatable(
    summary_table,
    extensions = "Buttons",
    caption = "Summary Table",
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'),
      paging = FALSE,  
      searching = FALSE,
      ordering = FALSE  
    ),
    style = 'default',
    class = 'table-striped table-bordered'
  )
  
  result$summary_table <- table1
  
  if (scree_plot) {
    data <- data.frame(
      x = seq_along(eigenvalue),
      y = eigenvalue / sum(eigenvalue) * 100,
      cum_y = cumsum(eigenvalue / sum(eigenvalue) * 100),
      label = round(eigenvalue / sum(eigenvalue) * 100, 2),
      cum_label = round(cumsum(eigenvalue / sum(eigenvalue) * 100), 2)
    )
    
    scree_plot <- plot_ly(data, x = ~x) %>%
      add_trace(
        type = "bar",
        y = ~y,
        name = "Individual Explained Variance",
        marker = list(color = "steelblue", line = list(color = "black", width = 1)),
        showlegend = scree_legend
      ) %>%
      add_trace(
        type = "scatter",
        mode = "markers",
        y = ~y,
        marker = list(size = 6, color = "black", symbol = "circle"),
        name = "",
        showlegend = scree_legend
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        y = ~cum_y,
        name = "Cumulative Explained Variance",
        line = list(color = "tomato"),
        showlegend = scree_legend
      ) %>%
      add_trace(
        type = "scatter",
        mode = "markers",
        y = ~cum_y,
        marker = list(size = 6, color = "black", symbol = "square"),
        name = "",
        showlegend = scree_legend
      ) %>%
      layout(
        title = "Scree Plot",
        xaxis = list(title = "PC"),
        yaxis = list(title = "Variance Explained (%)"),
        legend = list(
          x = scree_legend_pos[[1]],  
          y = scree_legend_pos[[2]],
          xanchor = "center",
          yanchor = "top"
        )
      )
    
    result$scree_plot <- scree_plot
  }
  
  
  if (biplot) {
    if (length(choices) != 2) {
      stop("Pick two Principle components")
    }
    
    scaling.factor <- sqrt(nrow(score) - 1)
    u <- sweep(score, 2, 1 / (sqrt(eigenvalue) * scaling.factor), FUN = '*')
    
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, sqrt(eigenvalue[choices]), FUN = '*'))
    
    v <- sweep(eigenvector, 2, sqrt(eigenvalue), FUN = '*')
    df.v <- as.data.frame(v[, choices])
    
    names(df.u) <- c('xvar', 'yvar')
    names(df.v) <- names(df.u)
    
    df.u <- df.u * scaling.factor * length_scale
    
    if (!is.null(groups)) {
      df.u$groups <- groups
    }
    
    df.v$varname <- rownames(v)
    df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
    df.v$hjust <- with(df.v, (1 - 1.2 * sign(xvar)) / 2)
    
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) +
      xlab(paste('PC', choices[1], ' (', sprintf('%0.1f%% explained var.)', 100 * eigenvalue[choices[1]]/sum(eigenvalue)), ')')) +
      ylab(paste('PC', choices[2], ' (', sprintf('%0.1f%% explained var.)', 100 * eigenvalue[choices[2]]/sum(eigenvalue)), ')')) +
      coord_equal()
    
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow(length = unit(1/2, 'picas')),
                   color = 'red')
    
    if (!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = 1)
    } else {
      g <- g + geom_point(alpha = 1)
    }
    
    g <- g +
      geom_text(data = df.v,
                aes(label = varname, x = xvar, y = yvar,
                    angle = angle, hjust = hjust),
                color = 'darkred', size = 3)+
      theme_minimal()+
      geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.2) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.2)
    
    
    result$biplot <-ggplotly(g)%>%
                      plotly::layout(title ="Biplot")
  }
  return(result)
}











