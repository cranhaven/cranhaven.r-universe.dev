# SYMMETRY PLOTS FOR Y AND RESIDUALS
################################################################################
################################################################################
################################################################################
################################################################################
y_symmetry <- function (y, title) 
{
  xlab <- deparse(substitute(y))
  txt.title <- if (missing(title))   paste("Symmetry plot of variable", xlab)
  else title    
  lower_half <- upper_half <- NULL
    y1 <- sort(y)
     n <- length(y)
     m <- median(y)
   low <- rev(m - y1[1:floor((n + 1)/2)])
    up <- y1[ceiling((n + 1)/2):n] - m
   da1 <- data.frame(lower_half=low, upper_half=up)
ggplot2::ggplot(data=da1, ggplot2::aes(x=lower_half, y=upper_half))+
    ggplot2::geom_point()+
    ggplot2::geom_abline(slope=1, intercept=0, colour="red") +
    ggplot2::ggtitle(txt.title)
}
####################################################################################
####################################################################################
####################################################################################
resid_symmetry <- function (model, title) 
{
  xlab <- deparse(substitute(model))
txt.title <- if (missing(title))   paste("resid symmetry plot of model",xlab)
  else title  
lower_half <- upper_half <- NULL
  y <- resid(model)
  y1 <- sort(y)
  n <- length(y)
  m <- median(y)
  low <- rev(m - y1[1:floor((n + 1)/2)])
  up <- y1[ceiling((n + 1)/2):n] - m
  da1 <- data.frame(lower_half=low, upper_half=up)
ggplot2::ggplot(data=da1, ggplot2::aes(x=lower_half, y=upper_half))+
  ggplot2::geom_point()+
  ggplot2::geom_abline(slope=1, intercept=0, colour="red") +
  ggplot2::ggtitle(txt.title)
}
################################################################################
################################################################################
################################################################################
################################################################################