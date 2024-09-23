## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(Colossus)
library(data.table)


## ----fig.cap='Linear Interpolated Function'-----------------------------------
dft <- data.table("x"=c(1,2,3),"y"=c(2,5,10))
g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) + 
     ggplot2::geom_point(color="black") + 
     ggplot2::geom_line(color="black",alpha=1) + 
     ggplot2::labs(x="age (days)", y="Covariate Value")
x <- seq(1,3,by=0.1)
y <- 1+x^2
dft <- data.table("x"=x,"y"=y)
g <- g + ggplot2::geom_line(data=dft, ggplot2::aes(x=.data$x, y=.data$y),
                            color="black",linetype = "dashed")
g

## ----fig.cap='Monotonic Step Function Applied'--------------------------------
dft <- data.table("x"=c(-1,1,5,8,13),"y"=c(0,1,1,2,3))
g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) +
     ggplot2::geom_point(color="black")
dft <- data.table("x"=c(-1,-0.01,0,1,5.99,6,11.99,12,13),"y"=c(0,0,1,1,1,2,2,3,3))
g <- g + ggplot2::geom_line(data=dft, ggplot2::aes(x=.data$x, y=.data$y), color="black") +
         ggplot2::labs(x="age (days)", y="Covariate Value")
g

## ----fig.cap='Step Function Applied'------------------------------------------
dft <- data.table("x"=c(-1,1,5,8,13),"y"=c(1,2,2,3,2))
g <- ggplot2::ggplot(dft,ggplot2::aes(x=.data$x, y=.data$y)) +
     ggplot2::geom_point(color="black")
dft <- data.table("x"=c(-1,-0.01,0,1,5.99,6,11.99,12,13),"y"=c(1,1,2,2,2,3,3,2,2))
g <- g + ggplot2::geom_line(data=dft, ggplot2::aes(x=.data$x, y=.data$y), color="black") +
         ggplot2::labs(x="age (days)", y="Covariate Value")
g

