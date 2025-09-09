#' Visualising Categorical Data
#'
#' A function which summarises categorical data using a waffle plot
#'
#' @param x a covariate to be summarised
#' @param nm a covariate name
#' @return a ggplot object
#' @import RColorBrewer waffle
#' @export
facVis <- function(x,nm){

  Freq <- NULL

  db <- data.frame(table(x));db
  cls <- brewer.pal(max(3,nrow(db)),"BuGn")
  p <- ggplot(data=db,aes(fill=x,values=Freq))+
    geom_waffle(color="white",size=0.33,n_rows=10)+
    theme_void()+
    scale_fill_manual(values = cls) +
    ggtitle(nm)+
    theme(legend.position="right") +
    theme(legend.title=element_blank())+
    theme(plot.title = element_text(hjust = 0.07))
  p
}
