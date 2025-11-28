# this is a general profile (deviance or GAIC) function 
# it can be used for profiling degrees of freedom in a GAM model  
# or in fact any coefficient of a term in a GAMLSS models
# created by MS Thursday, June 19, 2003 at 13:29 
# modified by MS Monday, August 25, 2003 at 13:25 to include intervals for profile GD
# tested again and modified by MS at 16-08-12
# new features are that now there is no need to fit a lot of points since the 
# profile deviance (or GAIC) function is approximated by a spline approximation 
# also the reported maximum is more likely to be correct since ir is calculated
# from the approximate function rather that a grip of values which can be crude
prof_term <- function (model = NULL, 
                   criterion = c("GD","GAIC"), 
                     penalty = 2.5, 
                         from = NULL, 
                         to = NULL, 
                        step = NULL, 
                      length = 7,
                      xlabel = NULL,
                        plot = TRUE,
                        perc = 95,
                  start.prev = TRUE,
                    line.col = "darkgreen",
              dash.line.type = 3, 
              dash.line.size = .8,
                   text.size = 5,
                      title
                        ) 
{
          x <- NULL  
if(is.null(model)) stop("you have not defined the model")
if(is.null(from)) stop("you have not defined the minimum value")
if(is.null(to)) stop("you have not defined the maximum value")
criterion  <- match.arg(criterion)
  interval <- if (is.null(step)) seq(from=from, to=to, length.out=length) else  seq(from=from, to=to, by=step)
       I.C <- rep(0, length(interval)) 
      call <- model
for (i in 1:length(interval))
{
      this <- interval[i] # mikis Thursday, March 27, 2008 
      assign("that", this)
      on.exit(rm(this, inherits=TRUE))
      mod.1 <- eval(call) 
       call <- mod.1$call
  if (start.prev)
  {
   if (   "mu"%in%mod.1$parameters)  call$mu.start    <- fitted(mod.1,"mu")
   if ("sigma"%in%mod.1$parameters)  call$sigma.start <- fitted(mod.1,"sigma")
   if (   "nu"%in%mod.1$parameters)  call$nu.start    <- fitted(mod.1,"nu")
   if (  "tau"%in%mod.1$parameters)  call$tau.start   <- fitted(mod.1,"tau")
  }
 I.C[i]<-  if(criterion=="GD")  deviance(mod.1) else  GAIC(mod.1,k=penalty)
} # finish the loop
      xlab <- if(!is.null(xlabel)) xlabel else "parameter" 
      ylab <- if(criterion=="GD") "Global Deviances" 
              else   paste("GAIC pen=",penalty)
      main <- if(criterion=="GD") "Profile Global Deviance" else  "Profile GAIC"
  prof.out <- cbind(interval, I.C)
        mx <- which.min(I.C)
         m <- length(interval)
  if ((mx<=1)||(mx>=m)) 
    stop("increase the interval to contain the MLE of the parameter")
  prof.fun <- splinefun(interval, I.C) # the profile likelihood as a function
       PML <- uniroot(prof.fun, c(from, to), deriv=1)$root # get the ML
      Gmin <- prof.fun(PML) # get the deviance
       lim <- Gmin + qchisq((perc/100), 1)
        xl <- as.vector(interval)
        CI <- c(NA, NA)
if (plot) 
   {
#  start plotting
  txt.title <- if (missing(title))  main
                else title  
#  first the curve
  gg <-ggplot2::ggplot(data.frame(x = c(from, to)), aes(x)) +
    ggplot2::stat_function(fun=prof.fun, color=line.col)+
    ggplot2::xlab(xlab)+
    ggplot2::ylab(ylab)+
    ggplot2::ggtitle(txt.title)
  plims <- par("usr")
# then the minimum
# we need more in the plot if GD
if(criterion=="GD")
 { 
  if (lim < max(I.C)) 
    { gg <- gg + 
      ggplot2::geom_hline(aes(yintercept = lim), lty=dash.line.type, 
                          linewidth=dash.line.size)
      y0 <- plims[3]
    scal <- (1/10 * (plims[4] - y0))/par("pin")[2] #
     scx <- (2/10 * (plims[2] - plims[1]))/par("pin")[1] 
 # MS change to 2/10, Sunday, December 9, 2007 at 23:28  
       X <- xl[1] + scx  
       Y <- lim + scal
       P <- paste(perc,"%")
      gg <-  gg + 
        ggplot2::annotate(geom = "text", x = X, y = Y, label = P, 
                          size=text.size)
     }
  if (I.C[1] > lim)  #Defines the lower bound 
    {
  leftFun <- function(x)  {prof.fun(x)-lim} # 
lcrossing <- uniroot(leftFun, c(from, PML))$root # get the ML
   CI[1] <- lcrossing
      gg <- gg + ggplot2::geom_vline(aes(xintercept = lcrossing),  
                            lty=dash.line.type, linewidth=dash.line.size)
    }
  if (I.C[m] > lim)  #Defines the upper bound
    {
rcrossing <- uniroot(leftFun, c(PML, to))$root # get the ML
    CI[2] <- rcrossing
       gg <- gg + ggplot2::geom_vline(aes(xintercept = rcrossing),  
                    lty=dash.line.type, linewidth=dash.line.size)
        #            segments( rcrossing, y0,  rcrossing, lim, lty = 3)
    print(gg)    
    }
cat("******************************************************************", "\n")
                cat("The Maximum Likelihood estimator is " ,PML, "\n")
                cat("with a Global Deviance equal to ", Gmin, "\n")
                if ((I.C[1] > lim) && (I.C[m] > lim))    
                {cat("A ", perc,"% Confidence interval is: (" ,lcrossing, ",", rcrossing, ") \n")}
cat("******************************************************************", "\n")     
 } # end if GD 
    else
 {
  print(gg)
cat("******************************************************************", "\n")
cat("The Mimimum is " ,PML, "\n")
cat("with an an GAIC(",penalty,") =", Gmin,  "\n")
cat("******************************************************************", "\n")
 }
} 
     return(invisible(gg))
}
