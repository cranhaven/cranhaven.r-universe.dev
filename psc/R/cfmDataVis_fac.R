#' Visualising Categorical Data
#'
#' A function which summarises categorical data using a bar plot.  A
#' sub-function of cfmDataVis
#'
#' @param x a covariate to be summarised
#' @param nm a covariate name
#' @return a ggplot object
#' @import RColorBrewer
cfmDataVis_fac <- function(x,nm){

  Freq <- NULL

  db <- data.frame(table(x));db
  cls <- brewer.pal(max(3,nrow(db)),"BuGn")
  p <- ggplot(data=db,aes(x=x,y=Freq))+
    geom_col(position = position_stack())+
    theme_void()+
    scale_fill_manual(values = cls) +
    ggtitle(nm)+
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 11, face = "bold"),
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.07,face = "bold", size = 12))


}
