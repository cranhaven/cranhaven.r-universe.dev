#' Visualising Categorical Data
#'
#' A function which compares visually a new categorical covariate against
#' equivalent data from a CFM
#'
#' @param p a ggplot objects
#' @param x a categorical covariate
#' @return a ggplot object
#' @import RColorBrewer waffle
#' @export
facVisComp <- function(p,x){

  Freq <- NULL

  old.d <- p$data;old.d
  tit <- p$labels$title
  old.d$source="CFM"
  dbnew <- data.frame(table(x));dbnew
  dbnew$source <- "DC"
  names(dbnew) <- names(old.d)
  df <- rbind(old.d,dbnew)

  cls <- brewer.pal(max(3,nrow(df)),"BuGn")


  p <- ggplot(data=df,aes(fill=x,values=Freq))+
    geom_waffle(color="white",size=0.33,n_rows=10)+
    theme_void()+
    scale_fill_manual(values = cls) +
    facet_wrap(~source,ncol=1)+
    ggtitle(tit)+
    theme(legend.position="right") +
    theme(legend.title=element_blank())+
    theme(plot.title = element_text(hjust = 0.07))
  p
}
