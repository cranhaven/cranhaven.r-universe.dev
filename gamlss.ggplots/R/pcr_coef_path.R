################################################################################
################################################################################
################################################################################
################################################################################
##### I have added col.fill 8-3-21
##### but it nedds more work on how to  deal with the legend
##### see page 115 of the book on ggplot to learn about legends
################################################################################
################################################################################
################################################################################
################################################################################
# the path of beta coefficients for different smoothing parameters 
# for a PCR object
pcr_coef_path <- function(x, legend=FALSE, plot=TRUE)
{
  if (!inherits(x,"PCR")) stop("the object should be a PCR class")
         M <- length(x$gamma)
        da <- t(x$coefficients[1:M, 1:M])
    lambda <- group <- NULL
       col <- c(col(da))
    if (legend)
    {
        pp <-  qplot(c(row(da)), c(da),  group = col, colour = col, geom = "line",
           ylab="beta", xlab="lambda")
    } else
    {
        dat <- data.frame(lambda=c(row(da)), beta=c(da), group=c(col(da)))
         pp <- ggplot2::ggplot(dat, 
                        ggplot2::aes(x=lambda, y=beta, group=group, col=group)) + 
           ggplot2::geom_line()+
           ggplot2::theme(legend.position = "none")
    }
if (plot) print(pp)
invisible(pp)
}
################################################################################
################################################################################
################################################################################
################################################################################
# the path of beta coefficients for different smoothing parameters 
# for a gamlss model in which PCR is fitted  
pcr_path <- function(x, parameter=c("mu", "sigma", "nu", "tau"),
                     legend=FALSE, plot=TRUE)
{
if (!is(x,"gamlss")) stop("the object should be a  gamlss class")
  parameter <- match.arg(parameter)
         xx <- getSmo(x, parameter=parameter)
if (!is(xx,"PCR")) stop("not PCR object detected") 
        pcr_coef_path(xx, legend=legend, plot=plot)+
          ggplot2::geom_vline(xintercept = xx$pc, colour = "gray")
}  
################################################################################
################################################################################
################################################################################
################################################################################
path.plot <- function(x,  parameter=c("mu", "sigma", "nu", "tau"),
                     xvar = c("norm", "dev", "lambda"))
{
  if (!is(x,"gamlss")) stop("the object should be a  gamlss class")
       parameter <- match.arg(parameter)
            xvar <- match.arg(xvar)
              xx <- getSmo(x, parameter=parameter)
  if (!is(xx[[1]],"glmnet")) stop("not glmnet object detected")            
             val <- xx[[2]]$optid
             dev <- xx[[1]]$dev.ratio
            norm <- colSums(abs(xx[[1]]$beta))
          lambda <- xx[[1]]$lambda 
             plot(xx[[1]], xvar=xvar)
 switch(xvar,
        "norm"=abline(v=norm[val], col="grey"),
        "dev"=abline(v=dev[val], col="grey"),
        "lambda"=abline(v=lambda[val], col="grey"))            
}                     
################################################################################
################################################################################
################################################################################
################################################################################

