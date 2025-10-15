#' @title Correlation Matrix
#' @description
#'   Column 1: Row names representing Variable 1 in the correlation test.
#'   
#'   Column 2: Column names representing Variable 2 in the correlation test.
#'   
#'   Column 3: The correlation coefficients quantifying the strength and direction of the relationship.
#'   
#'   Column 4: The p-values associated with the correlations, indicating the statistical significance
#'             of the observed relationships. Lower p-values suggest stronger evidence against the null hypothesis.
#' 
#' The table provides valuable insights into the relationships between variables, helping to identify
#' statistically significant correlations.
#'   
#' @param data Input dataset.
#' @param type Pearson or Spearman correlation, default is Pearson.
#' @param corr_plot Generate a correlation matrix plot, default is false.
#' @param sig.level Significant level. Default is 0.01.
#' @param highlight Highlight p-value(s) that is less than sig.level, default is FALSE
#' @param html Whether the output should be in HTML format,used when knitting into HTML. Default is FALSE. 
#' 
#' @return A data frame which contains row names, column names, correlation coefficients, and p-values.
#' @return A plot of the correlation if corrplot is set to be true.
#'
#' @examples
#' data(mtcars)
#' corr_matrix(mtcars, type = 'pearson')
#' @importFrom DT datatable formatStyle styleInterval
#' @importFrom htmltools tagList
#' @importFrom Hmisc rcorr
#' @importFrom plotly plot_ly
#' @importFrom plotly layout
#' @importFrom plotly colorbar
#' 
#' @export
corr_matrix <- function(data, type = 'pearson', corr_plot = FALSE, sig.level = 0.01,highlight=FALSE,html=FALSE) {
  if (html) result <- htmltools::tagList() else result <- list()
  
  cormat <- Hmisc::rcorr(as.matrix(data), type = type)$r
  
  pmat <- Hmisc::rcorr(as.matrix(data), type = type)$P
  pmat[is.na(pmat)] <- 0
  
  ut <- lower.tri(cormat)
  data <- data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor = cormat[ut],
    p = pmat[ut]
  )

  table1 <- DT::datatable(
    data,
    extensions = "Buttons",
    caption = "Correlation Table",
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf'),
      paging = TRUE,
      searching = FALSE,
      ordering = TRUE,
      scrollX = TRUE
    ),
    style = 'default',
    class = 'table-striped table-bordered'
  )
  
  if (highlight) {
    table1 <- table1 %>%
      formatStyle(
        columns = c("p"),
        valueColumns = c("p"),
        backgroundColor = styleInterval(0.05, c('yellow','transparent'))
      )
  }
  
  cormat[upper.tri(cormat)] <- NA
  pmat[upper.tri(cormat)] <- NA
  cormat[lower.tri(cormat)] <- ifelse(pmat[lower.tri(cormat)] < sig.level,cormat[lower.tri(cormat)], NA)
  plot <- plot_ly(z = cormat, x = rownames(cormat), y = colnames(cormat),, type = "heatmap", colorscale = "RdBu", reversescale = TRUE) %>%
    layout(title = "Correlation Heatmap",
           xaxis = list(title = "Variables", showgrid = FALSE),
           yaxis = list(title = "Variables", showgrid = FALSE))%>%
    colorbar(zmin = -1, zmax = 1) 
  result <- htmltools::tagList()
  result$table <- table1
  result$plot <- plot
  return(if (corr_plot) result else table1)
}



