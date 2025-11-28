################################################################################
################################################################################
################################################################################
################################################################################
# ACT abd PACF plot
################################################################################
################################################################################
################################################################################
################################################################################\
y_acf <- function(x, data, title)
{
     xlab <- deparse(substitute(y))
txt.title <- if (missing(title))   paste("ACF plot of",xlab)
             else title    
if (missing(x))  stop("the x is not declared")
if (!missing(data)) x <- get(xlab, envir = as.environment(data)) 
    bacf <- acf(x, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))  
  ciline <- qnorm((1 - 0.95)/2)/sqrt(length(x))
ggplot2::ggplot(data=bacfdf, ggplot2::aes(x=lag, y=acf)) + 
  ggplot2::geom_errorbar(ggplot2::aes(x=lag, ymax=acf, ymin=0), width=0)+
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color="gray")+ 
  ggplot2::geom_hline(ggplot2::aes(yintercept = ciline), col="blue", lty=2)+
  ggplot2::geom_hline(ggplot2::aes(yintercept = -ciline),col="blue", lty=2)+
  ggplot2::ggtitle(txt.title) 
}
##########################################################################
##########################################################################
##########################################################################
y_pacf <- function(x, data, title)
{
    xlab <- deparse(substitute(y))
txt.title <- if (missing(title))   paste("PACF plot of",xlab)
    else title      
if (missing(x))  stop("the x is not declared") 
if (!missing(data)) x <- get(xlab, envir = as.environment(data)) 
    bacf <- pacf(x, plot = FALSE)
  bacfdf <- with(bacf, data.frame(lag, acf))  
  ciline <- qnorm((1 - 0.95)/2)/sqrt(length(x))
ggplot2::ggplot(data=bacfdf, ggplot2::aes(x=lag, y=acf)) + 
    ggplot2::geom_errorbar(ggplot2::aes(x=lag, ymax=acf, ymin=0), width=0)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), color="gray")+ 
    ggplot2::geom_hline(ggplot2::aes(yintercept = ciline), col="blue", lty=2)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = -ciline), col="blue", lty=2)+
    ggplot2::ggtitle(txt.title) 
}
##########################################################################
##########################################################################
##########################################################################
##########################################################################