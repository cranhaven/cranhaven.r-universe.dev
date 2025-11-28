# deviance increment function
#---------------------------------------------------------------------
# the DevianceIncr() function will calculate the global deviance increment
# i)  for a fitted gamlss model or
# ii) for test data if the option newdata is used
# large values for deviance increment indicate bad fit
# and for new data bad prediction
#######################################################################
#######################################################################
#######################################################################
####################################################################### 
devianceIncr_cdf <- function(obj, newdata = NULL)
{
#######################################################################  
  getDelta<-function(x){
     xx <- as.character(x)
    xx1 <- xx[grep(c("\\."), x=xx)]
    xx2 <- xx[grep(c("\\-"), x=xx)]
    stopifnot(class(xx)=="character")
     x1 <- gsub("(.*)(\\.)|([0]*$)","",xx1)
     x2 <- as.integer(gsub("(.*)(\\-)|([0]*$)","",xx2))
    # x2 <- gsub("(.*)(\\-)|([0]*$)","",xx)
    if (length(c(nchar(x1),x2))==0) 
    {
      ppp <- 0.1 
      return(ppp) 
    }else
    {
      mm <- max(c(nchar(x1),x2) )
      ppp <- paste0("0.",paste(c(rep(0, mm),1),  collapse = ""))
      ppp <- as.numeric(ppp)
      return(ppp)
    } 
  }
#######################################################################  
  if (!is.gamlss(obj)) stop("This works for gamlss objects only")
      family <-  if(is.null(obj$call$family)) as.gamlss.family(NO) 
                 else as.gamlss.family(obj$call$family)
       fname <- obj$family[1]  
      #  type <- family$type
       nopar <- family$nopar
        pfun <- paste0("p",fname)
         cdf <- eval(parse(text=pfun))
  parameters <- names(family$parameters)
      Delta  <- getDelta(obj$y)
    #      fn <- eval(parse(text=pdf))()$G.dev.inc
       npar <- eval(parse(text=fname))()$nopar
  if ("mu"%in%parameters) 
    suppressWarnings(mu <- predict(obj, what = "mu", newdata = newdata, type = "response"))
  if ("sigma"%in%parameters)
    suppressWarnings(sigma <- predict(obj, what = "sigma", newdata = newdata, type = "response"))
  if ("nu"%in%parameters) 
    suppressWarnings(nu <- predict(obj, what = "nu", newdata = newdata, type = "response", 
                       na.action = na.omit))
  if ("tau"%in%parameters)
    suppressWarnings(tau <- predict(obj, what = "tau", newdata = newdata, type = "response", 
                       na.action = na.omit))
  if (is.null(newdata))
  {
    DINC <- switch(nopar, cdf(obj$y+Delta, mu=mu)-cdf(obj$y-Delta, mu=mu),
        cdf(obj$y+Delta, mu=mu, sigma=sigma)-cdf(obj$y-Delta, mu=mu, sigma=sigma), 
        cdf(obj$y+Delta, mu=mu, sigma=sigma, nu=nu)-cdf(obj$y-Delta, mu=mu, sigma=sigma, nu=nu),
        cdf(obj$y+Delta, mu=mu, sigma=sigma, nu=nu, tau=tau)-cdf(obj$y-Delta, mu=mu, sigma=sigma, nu=nu, tau=tau))
  } else
  {
      YY <- eval(attributes(terms(formula(obj)))$variables[[2]], envir=newdata)
    DINC <- switch(nopar, 
                   cdf(YY+Delta, mu=mu)-cdf(YY-Delta, mu=mu),
                   cdf(YY+Delta, mu=mu, sigma=sigma)-cdf(YY-Delta, mu=mu, sigma=sigma), 
                   cdf(YY+Delta, mu=mu, sigma=sigma, nu=nu)-cdf(YY-Delta, mu=mu, sigma=sigma, nu=nu),
                   cdf(YY+Delta, mu=mu, sigma=sigma, nu=nu, tau=tau)-cdf(YY-Delta, mu=mu, sigma=sigma, nu=nu, tau=tau))
  } 
  -2*log(DINC)
}
##############################################################################
##############################################################################








