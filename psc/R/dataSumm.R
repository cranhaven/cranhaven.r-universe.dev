#' A function which provides basic summary information of a matrix x
#'
#' A function which provides basic summaries of data provided within a data frame
#' of covariets.  Summaries are in the form of frequencies of counts and associated
#' percentages for categorical data and median (IQR) for continuous data.
#' Intended for use with the pscCFM.R function to define the setting in which a
#' model has been generated
#'
#' @param x a matrix of covariates
#' @details Categorical/Character data are summarised by a table and Continuous
#'     data are summarised as median (IQR)
#' @export
dataSumm <- function(x){

  cl.x <- class(x)

  if(cl.x%in%c("character","factor")){
    x <- factor(x)
    lev <- levels(x)
    tb <- table(x)
    ret <- c("Class"=cl.x,tb)
  }

  if(cl.x%in%c("integer","numeric")){
    x <- as.numeric(x)
    quant <- round(quantile(x,c(0.5,0.25,0.75)),2)
    minx <- min(x,na.rm=T)
    maxx <- max(x,na.rm=T)
    miqr <- paste(quant[1]," (",quant[2],", ",quant[3],")",sep="")
    ret <- c("Class"=cl.x,"min"=minx,"max"=maxx,"med"=quant[1],"low"=quant[2],
             "upp"=quant[3])
    ret
  }

  ret

}
