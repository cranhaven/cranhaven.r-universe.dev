#' Visualising Categorical Data
#'
#' A function which summarises categorical data using a waffle plot
#'
#' @param x a covariate to be summarised
#' @param nm a covariate name
#' @return a ggplot object
#' @import RColorBrewer
#' @export
numVis <- function(x,nm){

  y <- NULL
  cls <- brewer.pal(3,"BuGn")
  df <- data.frame("y"=x)
  p <- ggplot(aes(x=y),data=df) +
    geom_density(aes(x=y,y=after_stat(density)), fill=cls[1],colour=cls[1] ) +
    xlab("")+
    ylab("")+
    theme_minimal()+
    ggtitle(nm)+
    theme(plot.title = element_text(hjust = 0.05))

  p
}
