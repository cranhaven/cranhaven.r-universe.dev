#' Visualising Categorical Data
#'
#' A function which compares visually a new categorical covariate against
#' equivalent data from a CFM
#'
#' @param p a ggplot objects
#' @param x a categorical covariate
#' @return a ggplot object
#' @import RColorBrewer
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


  p <- ggplot(data=df,aes(x=x,y=Freq,fill=source))+
    geom_col(position = position_stack())+
    theme_void()+
    scale_fill_manual(values = cls) +
    facet_wrap(~source,ncol=1)+
    ggtitle(tit)+
    theme(
      legend.position="none",
      strip.text.x = element_text(size = 11, face = "bold"),
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.07,face = "bold", size = 12))
  p

  }
