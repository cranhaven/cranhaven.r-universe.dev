################################################################################
################################################################################
################################################################################
################################################################################
#load(system.file("extdata", "MomentSkewKurt1.RData", package="gamlss.dist"))
#load(system.file("extdata", "MomentSkewKurt2.Rda", package="gamlss.ggplots"))
#load(system.file("extdata", "MomentSkewKurt2.Rda", package="gamlss.ggplots"))
#load(system.file("extdata", "CentileSkewKurt.Rdata", package="gamlss.ggplots"))
#MomentSkewKurt2.rda
# Functions for moment bucket plots
################################################################################
################################################################################
################################################################################
################################################################################
#require(gamlss.ggplots)
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 1
################################################################################
################################################################################ 
moment_colour_half <- function(legend=TRUE) 
 {
################################################################################
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL
  load(system.file("extdata", "MomentSkewKurt1.RData", package="gamlss.ggplots"))
################################################################################
# local functions 
  boundaryf<-function(x)
  {
    tskew <- seq(0,0.99999,length=1000)
     skew <- tskew/(1-tskew)
     kurt <- 1 + (skew^2)
    tkurt <- kurt - 3
    tkurt <- tkurt/(1+abs(tkurt))
      fun <- splinefun(tkurt~tskew)
    fun
  }
  ff <- boundaryf()# we will use this function in the plot
################################################################################ 
fST3_2f <- function(x) 
  {
    if (length(x)>1) out <- ifelse(x<0.5|x>1, NA, fST3_2(x))
    else          out <- if (x<0.5||x>1) NA else fST3_2(x)
    out
  }
################################################################################
fST3_1f <- function(x) 
  {
    if (length(x)>1) out <- ifelse(x<0|x>0.499, NA, fST3_1(x))
    else out <- if (x<0||x>0.499) NA else fST3_1(x)
    out
  }
################################################################################  
#This is a repeat of the function in gamlss.dist because it restrict 
# the values of the function
skEGB2_1n<-function()
  {
    skEGB2<-function(nu,tau)
    {
       # m1y <- digamma(nu) - digamma(tau)
       mu2y <- trigamma(nu) + trigamma(tau)
       mu3y <- psigamma(nu,deriv=2) - psigamma(tau,deriv=2)
       mu4y <- psigamma(nu,deriv=3) + psigamma(tau,deriv=3) + 3*(mu2y^2)
       skew <- mu3y/(mu2y^1.5)
       kurt <- mu4y/(mu2y^2)
      tkurt <- kurt-3
      tskew <- skew/(1+abs(skew))
      tkurt <- tkurt/(1+abs(tkurt))
        out <- list(tskew=tskew,tkurt=tkurt)
        out
    }    
        tau <- seq(0.0001, 1000, length=1000
    )
         nu <- rep(1000,1000)
       sk2B <- skEGB2(nu,tau)
          x <- sk2B$tskew 
          y <- sk2B$tkurt 
        Fun <- approxfun(x, y)
    Fun
  }
fEGB2_1 <- skEGB2_1n()  
################################################################################
################################################################################
# main function starts here
# 
colors <- c("EGB2"= "magenta",  "JSU" = "darkgreen",
              "ST3"="blue", "SHASHo" = "orange", "SEP3" = "brown",
              "All"="black")
    gg <- ggplot2::ggplot(data = data.frame(x = c(0,1), y=c(-1,1)), 
          ggplot2::aes_string(x = "x", y="y"))+
          ggplot2::labs(x = "transformed moment skewness", 
                         y = "transformed moment excess kurtosis", 
                         color = "Distributions")+ 
          ggplot2::scale_color_manual(values = colors)+
          ggplot2::ylim(c(-1,1))+
      ggplot2::geom_segment(aes(x = 0, y =     1., xend = 1, yend = 1), 
                            lty=1, colour="black", lwd=1.5)+
      ggplot2::geom_segment(aes(x = 0, y = ff(0),  xend = 0, yend = 1), 
                            lty=1, colour="black", lwd=1.5)+
      ggplot2::stat_function(fun = fEGB2_2,   lty=1,  lwd=1, aes(color="EGB2"))+
      ggplot2::stat_function(fun = fEGB2_1,   lty=1,  lwd=1, aes(color="EGB2"))+
      ggplot2::stat_function(fun = fJSU,      lty=1,  lwd=1, aes(color="JSU"))+
      ggplot2::stat_function(fun = fSHASHo,   lty=1,  lwd=1, aes(color="SHASHo"))+
      ggplot2::stat_function(fun = fSEP3,     lty=1,  lwd=1, aes(color="SEP3"))+
      ggplot2::stat_function(fun = fST3_2f,   lty=1, lwd=1, aes(color="ST3"))+
      ggplot2::stat_function(fun = fST3_1f,   lty=1, lwd=1, aes(color="ST3"))+
      ggplot2::stat_function(fun = ff,        lty=1,  lwd=1.5, aes(color="All"))+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  if (legend==FALSE) gg <- gg + theme(legend.position = "none")
return(suppressWarnings(print(gg)))  
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 2
################################################################################
################################################################################
################################################################################
################################################################################
moment_gray_half <- function(legend=FALSE) 
{
######################################################################
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL
  load(system.file("extdata", "MomentSkewKurt1.RData", package="gamlss.ggplots"))
################################################################################  
# local functions 
boundaryf<-function(x)
  {
    tskew <- seq(0,0.99999,length=1000)
     skew <- tskew/(1-tskew)
     kurt <- 1 + (skew^2)
    tkurt <- kurt - 3
    tkurt <- tkurt/(1+abs(tkurt))
      fun <-splinefun(tkurt~tskew)
    fun
  }
ff <- boundaryf()# we will use this function in the plot
################################################################################  
fST3_2f <- function(x) 
  {
    if (length(x)>1) out <- ifelse(x<0.5|x>1, NA, fST3_2(x))
    else          out <- if (x<0.5||x>1) NA else fST3_2(x)
    out
  }
################################################################################
fST3_1f <- function(x) 
  {
    if (length(x)>1) out <- ifelse(x<0|x>0.499, NA, fST3_1(x))
    else out <- if (x<0||x>0.499) NA else fST3_1(x)
    out
  }
################################################################################  
#This is a repeat of the function in gamlss.dist because it restrict 
# the values of the function
skEGB2_1n<-function()
  {
    skEGB2 <- function(nu,tau)
    {
       #m1y <- digamma(nu) - digamma(tau)
      mu2y <- trigamma(nu) + trigamma(tau)
      mu3y <- psigamma(nu,deriv=2) - psigamma(tau,deriv=2)
      mu4y <- psigamma(nu,deriv=3) + psigamma(tau,deriv=3) + 3*(mu2y^2)
      skew <- mu3y/(mu2y^1.5)
      kurt <- mu4y/(mu2y^2)
     tkurt <- kurt-3
     tskew <- skew/(1+abs(skew))
     tkurt <- tkurt/(1+abs(tkurt))
       out <- list(tskew=tskew,tkurt=tkurt)
       out
    }    
       tau <- seq(0.0001, 1000, length=1000
    )
    nu <- rep(1000,1000)
    sk2B <- skEGB2(nu,tau)
       x <- sk2B$tskew 
       y <- sk2B$tkurt 
     Fun <-  approxfun(x, y)
     Fun
  }
fEGB2_1 <- skEGB2_1n()  
################################################################################
################################################################################
# main function starts here 
  type <- c("JSU" = "twodash","SHASHo" = "longdash", "SEP3" = "dotted",
            "ST3"="dotdash", "EGB2"= "dashed", "All"="solid")
  colors <- c("JSU" = gray(.7),"SHASHo" = gray(.3), "SEP3" = gray(.4),
              "ST3"=gray(.5), "EGB2"= gray(.6), "All"="black")
  ggg <- ggplot2::ggplot(data = data.frame(x = c(0,1)), 
                         ggplot2::aes_string(x = "x"))+
    ggplot2::xlab("transformed moment skewness")+
    ggplot2::ylab("transformed moment excess kurtosis")+
    ggplot2::scale_linetype_manual(values=type)+
    ggplot2::scale_color_manual(values = colors)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 1, xend = 1, yend = 1), 
                          lty=1, colour="black", lwd=1.5)+
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = ff(0), xend = 0, yend = 1), 
                          lty=1, colour="black", lwd=1.5)+
    ggplot2::stat_function(fun = fJSU,  
                           ggplot2::aes(linetype="JSU",    color="JSU"), lwd=1)+
    ggplot2::stat_function(fun = fSHASHo,   
                           ggplot2::aes(linetype="SHASHo", color="SHASHo"), lwd=1)+
    ggplot2::stat_function(fun = fSEP3,     
                           ggplot2::aes(linetype="SEP3",   color="SEP3"),  lwd=1)+
    ggplot2::stat_function(fun = fST3_2f,   
                           ggplot2::aes(linetype="ST3",    color="ST3"),  lwd=1)+
    ggplot2::stat_function(fun = fST3_1f,   
                           ggplot2::aes(linetype="ST3",    color="ST3"),  lwd=1)+
    ggplot2::stat_function(fun = fEGB2_2,   
                           ggplot2::aes(linetype="EGB2",   color="EGB2"),  lwd=1)+
    ggplot2::stat_function(fun = fEGB2_1,   
                           ggplot2::aes(linetype="EGB2",   color="EGB2"),  lwd=1)+
    ggplot2::stat_function(fun = ff,        
                           ggplot2::aes(linetype="All",    color="All"), lwd=1.5)+
    ggplot2::geom_point(ggplot2::aes(x=0, y=0), colour="black", pch=20, size = 4)
  if (legend==FALSE) ggg <- ggg + theme(legend.position = "none")
  return(suppressWarnings(print(ggg)))  
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 3
################################################################################
################################################################################
################################################################################
################################################################################
moment_colour_both <- function(legend=TRUE, line_width=1 ) 
{
################################################################################
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL  
  load(system.file("extdata", "MomentSkewKurt1.RData", package="gamlss.ggplots"))
################################################################################
colors <- c("EGB2"= "magenta",  "JSU" = "darkgreen",
             "ST3"="blue", "SHASHo" = "orange", "SEP3" = "brown",
            "All"="black")
# this is to ensure that only the right range is plotted 
################################################################################ 
#This is a repeat of the function in gamlss.dist because it restrict 
# the values of the function
skEGB2_1n<-function()
{
  skEGB2 <- function(nu,tau)
  {
    # m1y <- digamma(nu) - digamma(tau)
    mu2y <- trigamma(nu) + trigamma(tau)
    mu3y <- psigamma(nu,deriv=2) - psigamma(tau,deriv=2)
    mu4y <- psigamma(nu,deriv=3) + psigamma(tau,deriv=3) + 3*(mu2y^2)
    skew <- mu3y/(mu2y^1.5)
    kurt <- mu4y/(mu2y^2)
   tkurt <- kurt-3
   tskew <- skew/(1+abs(skew))
   tkurt <- tkurt/(1+abs(tkurt))
     out <- list(tskew=tskew,tkurt=tkurt)
     out
  }    
    tau <- seq(0.0001, 1000, length=1000
  )
     nu <- rep(1000,1000)
   sk2B <- skEGB2(nu,tau)
      x <- sk2B$tskew 
      y <- sk2B$tkurt 
    Fun <- approxfun(x, y)
  Fun
}
fEGB2_1 <- skEGB2_1n()  
################################################################################ 
bothJSU <- function(x) 
  {
    ffJSU <- function(x) fJSU(-x)
    if (length(x)>1) out <- ifelse(x<0, ffJSU(x), fJSU(x))
    else out <- if (x<0)  ffJSU(x) else fJSU(x)
    out
  }
################################################################################
bothff <- function(x) 
  {
   flipf <- function(x) ff(-x)
    if (length(x)>1) out <- ifelse(x<0, flipf(x), ff(x))
    else out <- if (x<0)  flipf(x) else ff(x)
    out
  }  
################################################################################  
bothSHASHo <- function(x) 
  {
    ffSHASHo <- function(x) fSHASHo(-x)
    if (length(x)>1) out <- ifelse(x<0, ffSHASHo(x), fSHASHo(x))
    else out <- if (x<0)  ffSHASHo(x) else fSHASHo(x)
    out
  }
################################################################################
bothSEP3 <- function(x) 
  {
    ffSEP3 <- function(x) fSEP3(-x)
    if (length(x)>1) out <- ifelse(x<0, ffSEP3(x), fSEP3(x))
    else out <- if (x<0)  ffSEP3(x) else fSEP3(x)
    out
  }
################################################################################
bothfST3_2 <- function(x) 
  {
    ffST3_2f <- function(x) fST3_2f(-x)
    if (length(x)>1) out <- ifelse(x<0, ffST3_2f(x), fST3_2f(x))
    else out <- if (x<0)  ffST3_2f(x) else fST3_2f(x)
    out
  }
################################################################################
bothfST3_1 <- function(x) 
  {
    ffST3_1f <- function(x) fST3_1f(-x)
    if (length(x)>1) out <- ifelse(x<0, ffST3_1f(x), fST3_1f(x))
    else out <- if (x<0)  ffST3_1f(x) else fST3_1f(x)
    out
  }
################################################################################
bothfEGB2_2 <- function(x) 
  {
    fffEGB2_2 <- function(x) fEGB2_2(-x)
    if (length(x)>1) out <- ifelse(x<0, fffEGB2_2(x), fEGB2_2(x))
    else out <- if (x<0)  fEGB2_2(x) else fEGB2_2(x)
    out
  }
################################################################################
bothfEGB2_1 <- function(x) 
  {
    fffEGB2_1 <- function(x) fEGB2_1(-x)
    if (length(x)>1) out <- ifelse(x<0, fffEGB2_1(x), fEGB2_1(x))
    else out <- if (x<0)  fEGB2_1(x) else fEGB2_1(x)
    out
}
################################################################################  
fST3_2f <- function(x) 
{
  if (length(x)>1) out <- ifelse(x<0.5|x>1, NA, fST3_2(x))
  else          out <- if (x<0.5||x>1) NA else fST3_2(x)
  out
}
################################################################################
fST3_1f <- function(x) 
{
  if (length(x)>1) out <- ifelse(x<0|x>0.499, NA, fST3_1(x))
  else out <- if (x<0||x>0.499) NA else fST3_1(x)
  out
}
################################################################################
boundaryf<-function(x)
{
  tskew <- seq(0,0.99999,length=1000)
  skew <- tskew/(1-tskew)
  kurt <- 1 + (skew^2)
  tkurt <- kurt - 3
  tkurt <- tkurt/(1+abs(tkurt))
  fun <-splinefun(tkurt~tskew)
  fun
}
ff <- boundaryf()# we will use this function in the plot  
################################################################################
  # the actual plot
  gg<- ggplot2::ggplot(data = data.frame(x = c(-1,1), y=c(-1,1)), 
                       ggplot2::aes_string(x = "x", y="y"))+
    labs(x = "transformed moment skewness", y = "transformed moment excess kurtosis", color = "Distributions.")+ 
  ggplot2::scale_color_manual(values = colors)+
  ggplot2::ylim(c(-1,1))+
  ggplot2::stat_function(fun = bothfEGB2_2,   lty=1,  lwd=line_width, 
                         ggplot2::aes(color="EGB2"))+
  ggplot2::stat_function(fun = bothfEGB2_1,   lty=1,  lwd=line_width, 
                         ggplot2::aes(color="EGB2"))+
  ggplot2::stat_function(fun = bothJSU,      lty=1,  lwd=line_width, 
                         ggplot2::aes(color="JSU"))+
  ggplot2::stat_function(fun = bothSHASHo,   lty=1,  lwd=line_width, 
                         ggplot2::aes(color="SHASHo"))+
  ggplot2::geom_segment(ggplot2::aes(x = -0.1, y = -0.4580381, xend = 0.1, yend = -0.4580381, color="SHASHo"), lty=1, lwd=line_width)+
  ggplot2::stat_function(fun = bothSEP3,      lty=1,  lwd=line_width, 
                         ggplot2::aes(color="SEP3"))+
  ggplot2::stat_function(fun = bothfST3_2,    lty=1,  lwd=line_width, 
                         ggplot2::aes(color="ST3"))+
  ggplot2::stat_function(fun = bothfST3_1,    lty=1, lwd=line_width, 
                         ggplot2::aes(color="ST3")) +
  ggplot2::stat_function(fun = bothff,       lty=1,  lwd=(line_width+.5), 
                         ggplot2::aes(color="All"))+
  ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)+
  ggplot2::geom_segment(aes(x = -1, y =  1, xend = 1, yend = 1), lty=1, colour="black", lwd=(line_width+.5))
  if (legend==FALSE) gg<- gg + ggplot2::theme(legend.position = "none")
  return(suppressWarnings(print(gg)))     
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 4
################################################################################
################################################################################ 
################################################################################ 
################################################################################ 
moment_gray_both <- function(line_width=1) 
{
################################################################################  
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL  
  load(system.file("extdata", "MomentSkewKurt1.RData", package="gamlss.ggplots")) 
################################################################################
bothJSU <- function(x) 
  {
    ffJSU <- function(x) fJSU(-x)
    if (length(x)>1) out <- ifelse(x<0, ffJSU(x), fJSU(x))
    else out <- if (x<0)  ffJSU(x) else fJSU(x)
    out
  }
################################################################################
bothff <- function(x) 
  {
    flipf <- function(x) ff(-x)
    if (length(x)>1) out <- ifelse(x<0, flipf(x), ff(x))
    else out <- if (x<0)  flipf(x) else ff(x)
    out
  }  
################################################################################  
bothSHASHo <- function(x) 
  {
    ffSHASHo <- function(x) fSHASHo(-x)
    if (length(x)>1) out <- ifelse(x<0, ffSHASHo(x), fSHASHo(x))
    else out <- if (x<0)  ffSHASHo(x) else fSHASHo(x)
    out
  }
################################################################################
bothSEP3 <- function(x) 
  {
    ffSEP3 <- function(x) fSEP3(-x)
    if (length(x)>1) out <- ifelse(x<0, ffSEP3(x), fSEP3(x))
    else out <- if (x<0)  ffSEP3(x) else fSEP3(x)
    out
  }
################################################################################
bothfST3_2 <- function(x) 
  {
    ffST3_2f <- function(x) fST3_2f(-x)
    if (length(x)>1) out <- ifelse(x<0, ffST3_2f(x), fST3_2f(x))
    else out <- if (x<0)  ffST3_2f(x) else fST3_2f(x)
    out
  }
################################################################################
bothfST3_1 <- function(x) 
  {
    ffST3_1f <- function(x) fST3_1f(-x)
    if (length(x)>1) out <- ifelse(x<0, ffST3_1f(x), fST3_1f(x))
    else out <- if (x<0)  ffST3_1f(x) else fST3_1f(x)
    out
  }
################################################################################
bothfEGB2_2 <- function(x) 
  {
    fffEGB2_2 <- function(x) fEGB2_2(-x)
    if (length(x)>1) out <- ifelse(x<0, fffEGB2_2(x), fEGB2_2(x))
    else out <- if (x<0)  fEGB2_2(x) else fEGB2_2(x)
    out
  }
################################################################################
bothfEGB2_1 <- function(x) 
  {
    fffEGB2_1 <- function(x) fEGB2_1(-x)
    if (length(x)>1) out <- ifelse(x<0, fffEGB2_1(x), fEGB2_1(x))
    else out <- if (x<0)  fEGB2_1(x) else fEGB2_1(x)
    out
  }
################################################################################
bothfEGB2_1 <- function(x) 
{
  fffEGB2_1 <- function(x) fEGB2_1(-x)
  if (length(x)>1) out <- ifelse(x<0, fffEGB2_1(x), fEGB2_1(x))
  else out <- if (x<0)  fEGB2_1(x) else fEGB2_1(x)
  out
}
################################################################################  
fST3_2f <- function(x) 
{
  if (length(x)>1) out <- ifelse(x<0.5|x>1, NA, fST3_2(x))
  else          out <- if (x<0.5||x>1) NA else fST3_2(x)
  out
}
################################################################################
fST3_1f <- function(x) 
{
  if (length(x)>1) out <- ifelse(x<0|x>0.499, NA, fST3_1(x))
  else out <- if (x<0||x>0.499) NA else fST3_1(x)
  out
}
################################################################################
boundaryf<-function(x)
{
  tskew <- seq(0,0.99999,length=1000)
  skew <- tskew/(1-tskew)
  kurt <- 1 + (skew^2)
  tkurt <- kurt - 3
  tkurt <- tkurt/(1+abs(tkurt))
  fun <-splinefun(tkurt~tskew)
  fun
}
ff <- boundaryf()# we will use this function in the plot  
################################################################################
################################################################################
# the actual plot
  gg<- ggplot2::ggplot(data = data.frame(x = c(-1,1), y=c(-1,1)), 
                       ggplot2::aes_string(x = "x", y="y"))+
  ggplot2::labs(x = "transformed moment skewness", 
                y = "transformed moment excess kurtosis", 
                color = "Distributions.")+ 
  ggplot2::scale_color_manual(values = colors)+
  ggplot2::ylim(c(-1,1))+
  ggplot2::geom_segment(aes(x = -1, y =1, xend = 1, yend = 1), lty=1, 
                        colour="black", lwd=(line_width+.5))+
  ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)+
  ggplot2::stat_function(fun = bothff, lty=1,lwd=(line_width+.5), 
                         color= gray(.2))+
  ggplot2::stat_function(fun = bothJSU,      lty=2, colour=gray(.2), 
                         lwd=line_width)+
  ggplot2::stat_function(fun = bothSHASHo,   lty=3, colour=gray(.3), 
                         lwd=line_width)+
  ggplot2::geom_segment(aes(x = -0.2, y = -0.4580381, xend = 0.2,
                  yend = -0.4580381), color=gray(.3), lty=3, lwd=line_width, )+
  ggplot2::stat_function(fun = bothSEP3,     lty=4, colour=gray(.4), lwd=line_width)+
  ggplot2::stat_function(fun = bothfST3_2,   lty=5, colour=gray(.5), lwd=line_width)+
  ggplot2:: stat_function(fun = bothfST3_1,   lty=5, colour=gray(.5), lwd=line_width)+
  ggplot2::stat_function(fun = bothfEGB2_2,  lty=6, colour=gray(.6), lwd=line_width)+
  ggplot2::stat_function(fun = bothfEGB2_1,  lty=6, colour=gray(.6), lwd=line_width)
  gg    
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 5
################################################################################
################################################################################
################################################################################
################################################################################
centile_colour_half <- function(type=c("tail", "central"), legend=TRUE, line_width=1) 
{
################################################################################ 
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL  
  load(system.file("extdata", "CentileSkewKurt.RData", package="gamlss.ggplots"))
################################################################################
################################################################################  
    colors <- c("SB"="red", "EGB2"= "magenta",  "JSU" = "darkgreen",
                "ST3"="blue", "SHASHo" = "orange", "SEP3" = "brown",
                "All"="black"
               )
     type <- match.arg(type)
c11 <- c12 <- c21 <- c22 <-  NULL     
if (type=='central')
  {
    dEGB2_1sk = data.frame(c11 = cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk = data.frame(c21 = cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    gg <-  ggplot2::ggplot(data = data.frame(x = c(0,1), y=c(-1,1)), 
                           ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "central centile skewness", 
                    y = "transformed centile excess kurtosis", 
                    color = "Distributions")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::geom_line(data=dEGB2_1sk, aes(x=c11, y = c12, color="EGB2"), 
                         lty=1, lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_2sk, aes(x=c21, y = c22, color="EGB2"), 
                         lty=1, lwd=line_width)+
      ggplot2::stat_function(fun = cJSU,      lty=1,  lwd=line_width, 
                             ggplot2::aes(color="JSU"))+
      ggplot2::stat_function(fun = cSHASH, xlim=c(0.005, 1), lty=1,  lwd=line_width, 
                             ggplot2::aes(color="SHASHo"))+
      ggplot2::stat_function(fun = cSEP3,  xlim=c(-0.005, 1),   lty=1,  
                             lwd=line_width, aes(color="SEP3"))+
      ggplot2::stat_function(fun = cST3_2, xlim=c(0.1445, 1), lty=1, lwd=line_width, 
                             ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = cST3_1, xlim=c(0, 0.1445), lty=1, lwd=line_width, 
                             ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = cSB,        lty=1,  lwd=line_width, 
                             ggplot2::aes(color="SB"))+
      ggplot2::geom_segment(aes(x = -0.003, y = 1, xend = 1.003, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 0, y = -0.7101, xend = 0, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -0.003, y = -0.7101, xend = 1, 
                            yend = -0.7101), lty=1, colour="black", 
                            lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1.003, y = -0.7101, xend = 1.003, yend = 1),
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }    
  if (type=='tail')
  {
    gg <-  ggplot2::ggplot(data = data.frame(x = c(0,1), y=c(-1,1)), 
                           ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "tail centile skewness", 
                    y = "transformed centile kurtosis", 
                    color = "Distributions")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::stat_function(fun = tSHASH, xlim=c(0, 1), lty=1,  lwd=1, 
                             ggplot2::aes(color="SHASHo"))+
      ggplot2::stat_function(fun = tSEP3,  xlim=c(0, 1),   lty=1,  lwd=1, 
                             ggplot2::aes(color="SEP3"))+
      ggplot2::stat_function(fun = tJSU,      lty=1,  lwd=1, 
                             ggplot2::aes(color="JSU"))+
      ggplot2::stat_function(fun = tSB,        lty=1,  lwd=1, 
                             ggplot2::aes(color="SB"))+
      ggplot2::stat_function(fun = tST3_2, xlim=c(0.484, 1),   lty=1, lwd=1, 
                             ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = tST3_1, xlim=c(0, 0.482),   lty=1, lwd=1, 
                             ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = tEGB2_1, xlim=c(0, 0.70), lty=1, lwd=1, 
                             ggplot2::aes(color="EGB2"))+
      ggplot2::stat_function(fun = tEGB2_2, xlim=c(0, 0.70),  lty=1, lwd=1, 
                             ggplot2::aes(color="EGB2"))+
      ggplot2::geom_segment(aes(x = -0.003, y = 1, xend = 1.003, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 0, y = -0.7101, xend = 0, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -0.003, y = -0.7101, xend = 1, yend = -0.7101), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1.003, y = -0.7101, xend = 1.003, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }  
  if (legend==FALSE) gg<- gg + theme(legend.position = "none")
  return(suppressWarnings(print(gg)))  
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 6
################################################################################
################################################################################
################################################################################
################################################################################ 
centile_colour_both <- function(type=c("tail", "central"), legend=TRUE, line_width=1) 
{
################################################################################ 
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL
  load(system.file("extdata", "CentileSkewKurt.RData", package="gamlss.ggplots"))
################################################################################
c_both_SEP3 <- function(x) 
{
  ccSEP3 <- function(x) cSEP3(-x) 
 if (length(x)>1)
 {
   out <- ifelse( x<0, ifelse(x>-1e-14, ccSEP3(-0.00000000000001), ccSEP3(x)), 
                  ifelse(x< 1e-14,  cSEP3(0.00000000000001),   cSEP3(x)))
 } else
 {
   out <- if (x<0)  ccSEP3(x) else cSEP3(x)
 }   
  out
}
#curve(c_both_SEP3, -1,1)
################################################################################
t_both_SEP3 <- function(x) 
{
  ttSEP3 <- function(x) tSEP3(-x) 
  if (length(x)>1)
  {
    out <- ifelse( x<0, ifelse(x>-1e-14, ttSEP3(-0.00000000000001), ttSEP3(x)), 
                   ifelse(x< 1e-14,  tSEP3(0.00000000000001),   tSEP3(x)))
  } else
  {
    out <- if (x<0)  ttSEP3(x) else tSEP3(x)
  }   
  out
}
# curve(t_both_SEP3, -1,1)
###############################################################################
c_both_JSU <- function(x) 
{
  ccJSU <- function(x) cJSU(-x) 
  if (length(x)>1)
  {
    out <- ifelse( x<0, ifelse(x>-1e-14, ccJSU(-0.00000000000001), ccJSU(x)), 
                   ifelse(x< 1e-14,  cJSU(0.00000000000001),   cJSU(x)))
  } else
  {
    out <- if (x<0)  ccJSU(x) else cJSU(x)
  }   
  out
}
#curve(c_both_JSU, -1,1)
################################################################################
t_both_JSU <- function(x) 
{
  ttJSU <- function(x) tJSU(-x) 
  if (length(x)>1)
  {
    out <- ifelse( x<0, ifelse(x>-1e-14, ttJSU(-0.00000000000001), ttJSU(x)), 
                   ifelse(x< 1e-14,  tJSU(0.00000000000001),   tJSU(x)))
  } else
  {
    out <- if (x<0)  ttJSU(x) else tJSU(x)
  }   
  out
}
#curve(t_both_JSU, -1,1)
################################################################################
c_both_SHASH <- function(x) 
{
  ccSHASH <- function(x) cSHASH(-x)
  if (length(x)>1) out <- ifelse(x<0, ccSHASH(x), cSHASH(x))
  else out <- if (x<0)  ccSHASH(x) else cSHASH(x)
  out
}
#curve(c_both_SHASH, -1,1)
################################################################################
t_both_SHASH <- function(x) 
{
  ttSHASH <- function(x) tSHASH(-x)
  if (length(x)>1) out <- ifelse(x<0, ttSHASH(x), tSHASH(x))
  else out <- if (x<0)  ttSHASH(x) else tSHASH(x)
  out
}
#curve(t_both_SHASH, -1,1)
################################################################################
c_both_SB <- function(x) 
{
  ccSB <- function(x) cSB(-x)
  if (length(x)>1) out <- ifelse(x<0, ccSB(x), cSB(x))
  else out <- if (x<0)  ccSB(x) else cSB(x)
  out
}
#curve(c_both_SB, -1,1)
################################################################################
t_both_SB <- function(x) 
{
  ttSB <- function(x) tSB(-x)
  if (length(x)>1) out <- ifelse(x<0, ttSB(x), tSB(x))
  else out <- if (x<0)  ttSB(x) else tSB(x)
  out
}
#curve(t_both_SB, -1,1)
#
################################################################################
c_both_ST3 <- function(x) 
{
  ccST3_2 <- function(x) cST3_2(-x)
  ccST3_1 <- function(x) cST3_1(-x)
  out <- ifelse(x >= -1 &  x < -0.1441,  ccST3_2(x), 0) +
         ifelse(x >=-0.1442 & x < 0,     ccST3_1(x), 0)+
         ifelse(x >= 0     & x<= 0.1441,    cST3_1(x), 0)+
         ifelse(x >= 0.1442 & x<= 1,      cST3_2(x), 0)  
  out
}
#curve(c_both_ST3,-1,1) 
################################################################################
t_both_ST3 <- function(x) 
{
  ttST3_2 <- function(x) tST3_2(-x)
  ttST3_1 <- function(x) tST3_1(-x)
  out <- ifelse(x >= -1 &  x < -0.484,  ttST3_2(x), 0) +
    ifelse(x >=-0.484 & x < 0,     ttST3_1(x), 0)+
    ifelse(x >= 0     & x<0.484,    tST3_1(x), 0)+
    ifelse(x >= 0.484 & x<= 1,       tST3_2(x), 0)  
  out
}
#curve(t_both_ST3,-1,1) 
################################################################################
t_both_EGB2_1 <- function(x) 
{
  ttEGB2_1 <- function(x) tEGB2_1(-x)
  out <- ifelse(x >= -1 &  x <= -0.7,  NA, 0) +
         ifelse(x > - 0.7 & x < 0,  ttEGB2_1(x), 0)+
         ifelse(x >= 0     & x<=0.7,   tEGB2_1(x), 0)+
         ifelse(x >= 0.7 & x<= 1,       NA, 0)  
  out
}
#curve(t_both_EGB2_1, -1,1, ylim=c(-1,1))
#
################################################################################
t_both_EGB2_2 <- function(x) 
{
  ttEGB2_2 <- function(x) tEGB2_2(-x)
  out <- ifelse(x >= -1 &  x <= -0.7,  NA, 0) +
    ifelse(x > - 0.7 & x < 0,  ttEGB2_2(x), 0)+
    ifelse(x >= 0     & x<=0.7,   tEGB2_2(x), 0)+
    ifelse(x >= 0.7 & x<= 1,       NA, 0)  
  out
}
#curve(t_both_EGB2_2, add=T)
#
################################################################################
################################################################################
# end of local functions  
################################################################################
################################################################################
colors <- c("SB"="red", "EGB2"= "magenta",  "JSU" = "darkgreen",
            "ST3"="blue", "SHASHo" = "orange", "SEP3" = "brown",
            "All"="black")
    type <- match.arg(type)
    c11 <- c12 <- c21 <- c22 <-  NULL     
  if (type=='central')
  {
    dEGB2_1sk = data.frame(c11 = cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk = data.frame(c21 = cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    dEGB2_1sk_ = data.frame(c11 = -cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk_ = data.frame(c21 = -cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    gg <-  ggplot2::ggplot(data = data.frame(x = c(-1,1), y=c(-1,1)), 
                           ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "central centile skewness", 
                    y = "transformed central centile excess kurtosis", 
                    color = "Distributions")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::geom_line(data=dEGB2_1sk, aes(x=c11, y = c12, color="EGB2"), lty=1, 
                         lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_2sk, aes(x=c21, y = c22, color="EGB2"), lty=1, 
                         lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_1sk_, aes(x=c11, y = c12, color="EGB2"), lty=1, 
                         lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_2sk_, aes(x=c21, y = c22, color="EGB2"), lty=1,
                         lwd=line_width)+
      ggplot2::stat_function(fun = c_both_JSU,      lty=1,  lwd=line_width, 
                             ggplot2::aes(color="JSU"))+
      ggplot2::stat_function(fun = c_both_SHASH, xlim=c(-1, 1), lty=1,  lwd=line_width,                              ggplot2::aes(color="SHASHo"))+
      ggplot2::stat_function(fun = c_both_SEP3,  xlim=c(-1, 1),   lty=1,  lwd=line_width,                            ggplot2::aes(color="SEP3"))+
      ggplot2::stat_function(fun = c_both_ST3, xlim=c(-1, 1), lty=1, lwd=line_width,                          ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = c_both_SB,        lty=1,  lwd=line_width, 
                             ggplot2::aes(color="SB"))+
      ggplot2::geom_segment(aes(x = -1, y =     1, xend = 1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1, y = -0.7101, xend = 1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = -1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = 1, yend = -0.7101), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }    
  if (type=='tail')
  {
    dEGB2_1sk = data.frame(c11 = cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk = data.frame(c21 = cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    dEGB2_1sk_ = data.frame(c11 = -cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk_ = data.frame(c21 = -cEGB2_2_data$cskew[c(-1, -2)], 
                            c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    gg <-  ggplot2::ggplot(data = data.frame(x = c(0,1), y=c(-1,1)), 
                           ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "tail centile skewness", 
                    y = "transformed centile excess kurtosis", 
                    color = "Distributions")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::stat_function(fun = t_both_SHASH, xlim=c(-1, 1), lty=1,  lwd=line_width, 
                    ggplot2::aes(color="SHASHo"))+
      ggplot2::stat_function(fun = t_both_SEP3,  xlim=c(-1, 1), lty=1,  lwd=line_width, 
                    ggplot2::aes(color="SEP3"))+
      ggplot2::stat_function(fun = t_both_JSU,   xlim=c(-1, 1), lty=1,  lwd=line_width, 
                    ggplot2::aes(color="JSU"))+
      ggplot2::stat_function(fun = t_both_SB,    xlim=c(-1, 1), lty=1,  lwd=line_width,                     ggplot2::aes(color="SB"))+
      ggplot2::stat_function(fun = t_both_ST3,   xlim=c(-1, 1),   lty=1, lwd=line_width,                     ggplot2::aes(color="ST3"))+
      ggplot2::stat_function(fun = t_both_EGB2_1, xlim=c(-1, 1), lty=1, lwd=line_width,                     ggplot2::aes(color="EGB2"))+
      ggplot2::stat_function(fun = t_both_EGB2_2, xlim=c(-1, 1), lty=1, lwd=line_width,  ggplot2::aes(color="EGB2"))+
      ggplot2::geom_segment(aes(x = -1, y =     1, xend = 1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1, y = -0.7101, xend = 1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = -1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = 1, yend = -0.7101), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }  
  if (legend==FALSE) gg<- gg + theme(legend.position = "none")
  return(suppressWarnings(gg))  
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 7
################################################################################
################################################################################
################################################################################
################################################################################
centile_gray_both <- function(type=c("tail", "central"), legend=TRUE, line_width=.5) 
{
################################################################################ 
  cEGB2_1_data <- cEGB2_2_data <- cJSU <- cSB <- cSEP3 <- cSHASH <- NULL
  cST3_1 <- cST3_2 <- fEGB2_1  <- fEGB2_2 <- fJSU <- fSEP3 <- NULL
  fSHASHo <- fST3_1 <- fST3_2 <- tEGB2_1 <- tEGB2_2 <- tJSU <- tSB <- NULL
  tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- NULL
  load(system.file("extdata", "CentileSkewKurt.RData", package="gamlss.ggplots"))
################################################################################  
################################################################################
c_both_SEP3 <- function(x) 
  {
    ccSEP3 <- function(x) cSEP3(-x) 
    if (length(x)>1)
    {
      out <- ifelse( x<0, ifelse(x>-1e-14, ccSEP3(-0.00000000000001), ccSEP3(x)), 
                     ifelse(x< 1e-14,  cSEP3(0.00000000000001),   cSEP3(x)))
    } else
    {
      out <- if (x<0)  ccSEP3(x) else cSEP3(x)
    }   
    out
  }
#curve(c_both_SEP3, -1,1)
################################################################################
t_both_SEP3 <- function(x) 
  {
    ttSEP3 <- function(x) tSEP3(-x) 
    if (length(x)>1)
    {
      out <- ifelse( x<0, ifelse(x>-1e-14, ttSEP3(-0.00000000000001), ttSEP3(x)), 
                     ifelse(x< 1e-14,  tSEP3(0.00000000000001),   tSEP3(x)))
    } else
    {
      out <- if (x<0)  ttSEP3(x) else tSEP3(x)
    }   
    out
  }
# curve(t_both_SEP3, -1,1)
################################################################################
c_both_JSU <- function(x) 
  {
    ccJSU <- function(x) cJSU(-x) 
    if (length(x)>1)
    {
      out <- ifelse( x<0, ifelse(x>-1e-14, ccJSU(-0.00000000000001), ccJSU(x)), 
                     ifelse(x< 1e-14,  cJSU(0.00000000000001),   cJSU(x)))
    } else
    {
      out <- if (x<0)  ccJSU(x) else cJSU(x)
    }   
    out
  }
#curve(c_both_JSU, -1,1)
################################################################################
  t_both_JSU <- function(x) 
  {
    ttJSU <- function(x) tJSU(-x) 
    if (length(x)>1)
    {
      out <- ifelse( x<0, ifelse(x>-1e-14, ttJSU(-0.00000000000001), ttJSU(x)), 
                     ifelse(x< 1e-14,  tJSU(0.00000000000001),   tJSU(x)))
    } else
    {
      out <- if (x<0)  ttJSU(x) else tJSU(x)
    }   
    out
  }
#curve(t_both_JSU, -1,1)
################################################################################
  c_both_SHASH <- function(x) 
  {
    ccSHASH <- function(x) cSHASH(-x)
    if (length(x)>1) out <- ifelse(x<0, ccSHASH(x), cSHASH(x))
    else out <- if (x<0)  ccSHASH(x) else cSHASH(x)
    out
  }
#curve(c_both_SHASH, -1,1)
################################################################################
t_both_SHASH <- function(x) 
  {
    ttSHASH <- function(x) tSHASH(-x)
    if (length(x)>1) out <- ifelse(x<0, ttSHASH(x), tSHASH(x))
    else out <- if (x<0)  ttSHASH(x) else tSHASH(x)
    out
  }
#curve(t_both_SHASH, -1,1)
################################################################################
  c_both_SB <- function(x) 
  {
    ccSB <- function(x) cSB(-x)
    if (length(x)>1) out <- ifelse(x<0, ccSB(x), cSB(x))
    else out <- if (x<0)  ccSB(x) else cSB(x)
    out
  }
#curve(c_both_SB, -1,1)
################################################################################
  t_both_SB <- function(x) 
  {
    ttSB <- function(x) tSB(-x)
    if (length(x)>1) out <- ifelse(x<0, ttSB(x), tSB(x))
    else out <- if (x<0)  ttSB(x) else tSB(x)
    out
  }
  #curve(t_both_SB, -1,1)
#
################################################################################
c_both_ST3 <- function(x) 
  {
    ccST3_2 <- function(x) cST3_2(-x)
    ccST3_1 <- function(x) cST3_1(-x)
    out <- ifelse(x >= -1 &  x < -0.1441,  ccST3_2(x), 0) +
      ifelse(x >=-0.1442 & x < 0,     ccST3_1(x), 0)+
      ifelse(x >= 0     & x<= 0.1441,    cST3_1(x), 0)+
      ifelse(x >= 0.1442 & x<= 1,      cST3_2(x), 0)  
    out
  }
  #curve(c_both_ST3,-1,1) 
################################################################################
t_both_ST3 <- function(x) 
  {
    ttST3_2 <- function(x) tST3_2(-x)
    ttST3_1 <- function(x) tST3_1(-x)
    out <- ifelse(x >= -1 &  x < -0.484,  ttST3_2(x), 0) +
      ifelse(x >=-0.484 & x < 0,     ttST3_1(x), 0)+
      ifelse(x >= 0     & x<0.484,    tST3_1(x), 0)+
      ifelse(x >= 0.484 & x<= 1,       tST3_2(x), 0)  
    out
  }
  #curve(t_both_ST3,-1,1) 
################################################################################
t_both_EGB2_1 <- function(x) 
  {
    ttEGB2_1 <- function(x) tEGB2_1(-x)
    out <- ifelse(x >= -1 &  x <= -0.7,  NA, 0) +
      ifelse(x > - 0.7 & x < 0,  ttEGB2_1(x), 0)+
      ifelse(x >= 0     & x<=0.7,   tEGB2_1(x), 0)+
      ifelse(x >= 0.7 & x<= 1,       NA, 0)  
    out
  }
#curve(t_both_EGB2_1, -1,1, ylim=c(-1,1))
#
################################################################################
  t_both_EGB2_2 <- function(x) 
  {
    ttEGB2_2 <- function(x) tEGB2_2(-x)
    out <- ifelse(x >= -1 &  x <= -0.7,  NA, 0) +
      ifelse(x > - 0.7 & x < 0,  ttEGB2_2(x), 0)+
      ifelse(x >= 0     & x<=0.7,   tEGB2_2(x), 0)+
      ifelse(x >= 0.7 & x<= 1,       NA, 0)  
    out
  }
  #curve(t_both_EGB2_2, add=T)
################################################################################
################################################################################
# end of local functions  
################################################################################
################################################################################  
  colors <- c("JSU" = "darkgreen","SHASHo" = "orange", "SEP3" = "brown",
              "ST3"="blue", "EGB2"= "magenta", "SB"="red")
  type <- match.arg(type)
  c11 <- c12 <- c21 <- c22 <-  NULL 
  if (type=='central')
  {
    dEGB2_1sk = data.frame(c11 = cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk = data.frame(c21 = cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    dEGB2_1sk_ = data.frame(c11 = -cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk_ = data.frame(c21 = -cEGB2_2_data$cskew[c(-1, -2)], 
                            c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    gg <-  ggplot2::ggplot(data = data.frame(x = c(-1,1), y=c(-1,1)), 
                           ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "trans. central centile skewness", 
                    y = "transformed centile excess kurtosis", 
                    color = "Distributions")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::geom_line(data=dEGB2_1sk,  aes(x=c11, y = c12), lty=9,
                         colour=gray(.6), lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_2sk,  aes(x=c21, y = c22), lty=9, 
                         colour=gray(.6), lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_1sk_, aes(x=c11, y = c12), lty=9, 
                         colour=gray(.6), lwd=line_width)+
      ggplot2::geom_line(data=dEGB2_2sk_, aes(x=c21, y = c22), lty=9, 
                         colour=gray(.6), lwd=line_width)+
      ggplot2::stat_function(fun = c_both_JSU,lty=2, colour=gray(.2), 
                             lwd=line_width)+
      ggplot2::stat_function(fun = c_both_SHASH,  lty=8, colour=gray(.1), 
                             lwd=line_width)+
      ggplot2::stat_function(fun = c_both_SEP3,  xlim=c(-1, 1),lty=4, 
                             colour=gray(.4), lwd=line_width)+
      ggplot2::stat_function(fun = c_both_ST3, xlim=c(-1, 1),  lty=5, 
                             colour=gray(.5), lwd=line_width)+
      ggplot2::stat_function(fun = c_both_SB,   lty=7, colour=gray(.4), 
                             lwd=line_width)+
      ggplot2::geom_segment(aes(x = -1, y =     1, xend = 1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1, y = -0.7101, xend = 1, yend = 1), lty=1,
                            colour="black", lwd=line_width+.5)+
    #  geom_segment(aes(x = 0, y = -0.7101, xend = 0, yend = 1), lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = -1, yend = 1), lty=1, 
                            colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = 1, yend = -0.7101), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }    
  if (type=='tail')
  {
    dEGB2_1sk = data.frame(c11 = cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk = data.frame(c21 = cEGB2_2_data$cskew[c(-1, -2)], 
                           c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    dEGB2_1sk_ = data.frame(c11 = -cEGB2_1_data$cskew, c12 = cEGB2_1_data$ckurt)
    dEGB2_2sk_ = data.frame(c21 = -cEGB2_2_data$cskew[c(-1, -2)], 
                            c22 = cEGB2_2_data$ckurt[c(-1, -2)])
    gg <- 
      ggplot2::ggplot(data = data.frame(x = c(0,1), y=c(-1,1)), 
                      ggplot2::aes_string(x = "x", y="y"))+
      ggplot2::labs(x = "tail centile skewness", 
                    y = "transformed centile excess kurtosis")+ 
      ggplot2::scale_color_manual(values = colors)+
      ggplot2::ylim(c(-1,1))+
      ggplot2::stat_function(fun = t_both_SHASH,  xlim=c(-1, 1),   lty=8, 
                             colour=gray(.1), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_SEP3,   xlim=c(-1, 1),   lty=4, 
                             colour=gray(.4), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_JSU,    xlim=c(-1, 1),   lty=2, 
                             colour=gray(.2), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_SB,     xlim=c(-1, 1),   lty=7, 
                    colour=gray(.4), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_ST3,    xlim=c(-1, 1),   lty=5, 
                             colour=gray(.5), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_EGB2_1, xlim=c(-1, 1),   lty=9, 
                             colour=gray(.6), lwd=line_width)+
      ggplot2::stat_function(fun = t_both_EGB2_2, xlim=c(-1, 1),   lty=9, 
                             colour=gray(.6), lwd=line_width)+
      ggplot2::geom_segment(aes(x = -1, y =     1, xend = 1, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = 1, y = -0.7101, xend = 1, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
    #  geom_segment(aes(x = 0, y = -0.7101, xend = 0, yend = 1), lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = -1, yend = 1), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_segment(aes(x = -1,y = -0.7101, xend = 1, yend = -0.7101), 
                            lty=1, colour="black", lwd=line_width+.5)+
      ggplot2::geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4)
  }  
  if (legend==FALSE) gg<- gg + theme(legend.position = "none")
  return(suppressWarnings(gg))  
}
################################################################################
################################################################################
################################################################################
################################################################################
# FUNCTION 8
################################################################################
################################################################################
################################################################################
################################################################################
moment_bucket <- function(x,...,
                          weights = NULL,
                     no_bootstrap = 99,
                    col_bootstrap = hcl.colors(length.obj, palette = "Set 2"),# see hcl.pals()
                  alpha_bootstrap = 1,
                     text_to_show = NULL,
                         cex_text = 5,
                         col_text = "black",
                    colour_bucket = FALSE,
                       line_width = .5,
                      col_JB_test = gray(0.7),
                    alpha_JB_test = 0.1
)
{
################################################################################
################################################################################
# local functions here
################################################################################
################################################################################
CI95 <- function(x)#
  {
    if (any(abs(x) >= 1)) stop(" x should be in (-1,1)")
    gamma.1 <- x/(1 - x)
    gamma.2 <- ((24/n)*(qchisq(.95, df = 2) - (n/6)*gamma.1^2))^0.5
    gamma.2t <- gamma.2/(1 + abs(gamma.2))
    gamma.2t
  }
################################################################################
CI95.2 <- function(x) CI95(-x)
################################################################################
CI95.3 <- function(x) -CI95(-x)
################################################################################
CI95.4 <- function(x) -CI95(x)
################################################################################
################################################################################
# main function stats here
y <- Y <- X <- NULL
# checking whether model or get the residuals otherwise data
     object <- list(x, ...)
   isvector <- all(unlist(lapply(object, is.numeric)))
  # isgamlss <- all(unlist(lapply(object, is.gamlss)))
       Call <- match.call()          ## trying to get the names
# set null to avoid warnings
  Call$weights <- Call$bootstrap <- Call$no_bootstrap <- Call$col_bootstrap <- NULL
  Call$text_to_show <- Call$cex_text <- Call$col_text <- Call$show.legend <- NULL
  Call$colour_bucket <- NULL
  the.names <- if (is.null(text_to_show)) as.character(Call[2:(length(object) + 1)])
             else text_to_show
         n <- if(isvector) length(object[[1]]) else object[[1]]$N
length.obj <- length(object)
    lresid <- if (is(object[[1]],"gamlss")) length(resid(object[[1]]))
              else length(object[[1]])
       DA0 <- matrix(0, nrow = lresid, ncol = length.obj )
for (j in 1:length(object))
  {
    DA0[,j] <-  if (is(object[[j]],"gamlss")) resid(object[[j]])
    else object[[j]]
  }
       DA0 <- as.data.frame(DA0)
colnames(DA0) <- the.names
       DA1 <- da2 <- NULL
################################################################################
for (j in 1:length(the.names))
  {
    #if (bootstrap)
    #    {
    bootx <- booty <- rep(0,no_bootstrap)
    for (i in 1:no_bootstrap)
    {
      ind <- sample(n, n,  replace = TRUE)

      sk <-  momentSK(DA0[ind,j], weights = weights[ind])
      bootx[i] <- sk$trans.mom.skew
      booty[i] <- sk$trans.mom.kurt
    }
#--------------------
    DA1  <- rbind(DA1, data.frame(X = bootx, Y=booty,
                       Model = rep(the.names[j], no_bootstrap),
                       Col = rep(col_bootstrap[j], no_bootstrap)))
       k <- momentSK(DA0[,j], weights = weights)
     da2 <- rbind(da2, data.frame(X = k$trans.mom.skew, Y = k$trans.mom.kurt, Model = j))
  }# end different models
DA1$Model <- factor(DA1$Model)
da2$Model <- factor(da2$Model)
# fist the background
  gg <- if(colour_bucket==TRUE) moment_colour_both(legend=FALSE, line_width=line_width) else moment_gray_both(line_width=line_width)
  xx <- seq(0, 0.99, length=101)
  y1 <- CI95(xx)
  y2 <- CI95.2(-xx)
  y3 <- CI95.3(-xx)
  y4 <- CI95.4(xx)
  xy <- na.omit(data.frame(x=c(xx,xx[101:1],-xx,-xx[101:1]), y=c(y1,y4[101:1],y3,y2[101:1])))
  #    basic <- gg +  geom_polygon(data=xy, aes(x=x, y=y, alpha=0.1),  fill="lightgray")
################################################################################
# test 1
#  ggplot(data=DA1, aes(X,Y, color=Model))+geom_point()+ylim(c(-1,1))+xlim(c(-1,1))
# test 2
#  gg +  geom_polygon(data=xy, aes(x=x, y=y, alpha=0.1),  fill="lightgray")+
#    geom_point(data=DA1, aes(X,Y, group=Model))# NOT colour
# test 3
  gg <- gg +  geom_polygon(data=xy, aes(x=x, y=y, alpha=alpha_JB_test),  fill=col_JB_test)+
    geom_point(data=DA1, aes(X,Y), colour=DA1$Col, alpha=alpha_bootstrap)+
    annotate(geom="text", x=da2$X, y=da2$Y, label=the.names, color=col_text, cex=cex_text)+
    theme(legend.position = "none")
  #gg <- basic +  theme(legend.position = "none")
  #+scale_color_brewer(palette="Dark2")
  return(suppressWarnings(gg))
}
################################################################################
################################################################################
################################################################################
################################################################################
#FUNCTION 9
################################################################################
################################################################################
################################################################################
################################################################################
centile_bucket <- function(x,...,
                          type = c("tail", "central"),
                       weights = NULL,
                  no_bootstrap = 99,
                 col_bootstrap = hcl.colors(length.obj, palette = "Set 2"),# see hcl.pals()
               alpha_bootstrap = 1,
                  text_to_show = NULL,
                      cex_text = 5,
                      col_text = "black",#
                 colour_bucket = FALSE,
                    line_width = 0.5,
                      sim_test = FALSE,
                   no_sim_test = 1000,
                  col_sim_test = gray(.7),
                alpha_sim_test = .1,
                     seed_test = 1234)
{
################################################################################
################################################################################
# local functions here
################################################################################
################################################################################
# main function starts here
# checking whether model or get the residuals otherwise data
    object <- list(x, ...)
      type <- match.arg(type)
        Ke <- St <- Sc <- NULL
  isvector <- all(unlist(lapply(object, is.numeric)))
  #isgamlss <- all(unlist(lapply(object, is.gamlss)))
      Call <- match.call()          ## trying to get the names
  # set null to avoid warnings
  Call$weights <- Call$bootstrap <- Call$no_bootstrap <- Call$col_bootstrap <- NULL
  Call$text_to_show <- Call$cex_text <- Call$col_text <- Call$show.legend <- NULL
  Call$colour_bucket <- NULL
 the.names <- if (is.null(text_to_show)) as.character(Call[2:(length(object)+1)])
                else text_to_show
         n <- if(isvector) length(object[[1]]) else object[[1]]$N
length.obj <- length(object)
    lresid <- if (is(object[[1]],"gamlss")) length(resid(object[[1]]))
              else length(object[[1]])
       DA0 <- matrix(0, nrow=lresid, ncol= length.obj )
for (j in 1:length(object))
 {
    DA0[,j] <-  if (is(object[[j]],"gamlss")) resid(object[[j]])
    else object[[j]]
 }
      DA0 <- as.data.frame(DA0)
colnames(DA0) <- the.names
     DA1 <- da2 <- NULL
################################################################################
for (j in 1:length(the.names))
  {
    bootx <- booty <- bootz <- rep(0,no_bootstrap)
    for (i in 1:no_bootstrap)
    {
           ind <- sample(n, n,  replace = TRUE)
            sk <-  centileSK(DA0[ind,j], weights=weights[ind])
      bootx[i] <- sk$S0.25
      booty[i] <- sk$S0.01
      bootz[i] <- sk$trans.K0.01
    }
#--------------------
    DA1  <- rbind(DA1, data.frame(S0.25 = bootx,
                                  S0.01 = booty,
                                  K0.01 = bootz,
                                  Model = rep(the.names[j], no_bootstrap),
                                    Col = rep(col_bootstrap[j], no_bootstrap)))
      k <- centileSK(DA0[,j], weights=weights)
    da2 <- rbind(da2, data.frame(S0.25 = k$S0.25,
                                 S0.01 = k$S0.01,
                                 K0.01 = k$trans.K0.01,
                                 Model = j))
  }# end different models
     Sc <- S0.25 <- K0.01 <- S0.01 <- NULL
     # K0.01 <- Kt <- Model <-  S0.01 <- S0.25 <- Sc <- X <- Y <- NULL
     # c11 <- c12 <- c21 <- c22 <-  cEGB2_1_data <- cEGB2_2_data <- NULL
     # cJSU <- cSB <- cSEP3 <- cSHASH < cST3_1 <- cST3_2 <- tEGB2_1 <- NULL
     # tEGB2_2 <-  tJSU <- tSB <- tSEP3 <- tSHASH <- tST3_1 <- tST3_2 <- y <- NULL
DA1$Model <- factor(DA1$Model)
da2$Model <- factor(da2$Model)
# fist the background
gg <- if (colour_bucket == TRUE) centile_colour_both(type=type, legend = FALSE, line_width = line_width) 
  else  centile_gray_both(type=type, line_width=line_width)
if (sim_test)
{
  if (type == "central")
  {
    CC <- centSim(n=n, no_sim_test,  seed.number=seed_test)
    DD <- drawEllipse2(CC, type=type)
    gg <- gg +  geom_polygon(data=DD, aes(x=Sc, y=Ke, alpha=alpha_sim_test),  fill=col_sim_test)+
      geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4) +
      geom_point(data=DA1, aes(x=S0.25,y= K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      annotate(geom="text", x=da2$S0.25, y=da2$K0.01, label=the.names, color=col_text, cex=cex_text)+
      theme(legend.position = "none")
  } else
  {
    CC <- centSim(n=n, no_sim_test,  seed.number=seed_test)
    DD <- drawEllipse2(CC, type=type)
    #DD <- drawEllipse(n=n, n.sim=no_sim_test, type=type, seed.number=seed_test)
    gg <- gg +  geom_polygon(data=DD, aes(x=St, y=Ke, alpha=alpha_sim_test),  fill=col_sim_test)+
      geom_point(aes(x=0, y=0), colour="black", pch=20, size = 4) +
      geom_point(data=DA1, aes(x=S0.01,y= K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      annotate(geom="text", x=da2$S0.01, y=da2$K0.01, label=the.names, color=col_text, cex=cex_text)+
      theme(legend.position = "none")
  }
} else
{
  if (type == "central")
  {
    gg <- gg +
      geom_point(data=DA1, aes(x=S0.25,y= K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      annotate(geom="text", x=da2$S0.25, y=da2$K0.01, label=the.names, color=col_text, cex=cex_text)+
      theme(legend.position = "none")
  } else
  {
    gg <- gg +
      geom_point(data=DA1, aes(x=S0.01,y= K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      annotate(geom="text", x=da2$S0.01, y=da2$K0.01, label=the.names, color=col_text, cex=cex_text)+
      theme(legend.position = "none")
  }
}
  return(suppressWarnings(gg))
}
#################################################################################
#################################################################################
#################################################################################
#################################################################################
# FUNCTION 10
#################################################################################
#################################################################################
#################################################################################
#################################################################################
moment_bucket_wrap <- function(x,...,
                            weights = NULL,
                               xvar = NULL,
                            n_inter = 4,
                       no_bootstrap = 99,
                      col_bootstrap = hcl.colors(length.obj, palette="Set 2"),# see hcl.pals()
                    alpha_bootstrap = 1,
                       text_to_show = NULL,
                 check_overlap_text = FALSE,
                           cex_text = 5,
                           col_text = "black",#
                      colour_bucket = FALSE,
                        col_JB_test = gray(.7),
                      alpha_JB_test = .1
                               )
{
################################################################################
# local functions here
  CI95 <- function(x, n)#
  {
    if (any(abs(x)>=1)) stop(" x should be in (-1,1)")
    gamma.1 <- x/(1-x)
    gamma.2 <- ( (24/n)*(qchisq(.95, df=2)-(n/6)*gamma.1^2))^0.5
    gamma.2t <- gamma.2/(1+abs(gamma.2))
    gamma.2t
  }
  CI95.2 <- function(x, n) CI95(-x, n)
  CI95.3 <- function(x, n) -CI95(-x, n)
  CI95.4 <- function(x, n) -CI95(x, n)
################################################################################
# main function stats here
  Model <- Y <- X <- NULL
# checking whether model ro get the residual otherwise data
    object <- list(x, ...)
 # isvector <- all(unlist(lapply(object, is.numeric)))
 # isgamlss <- all(unlist(lapply(object, is.gamlss)))
      Call <- match.call()          ## trying to get the names
Call$weights <- Call$bootstrap <- Call$no_bootstrap <- Call$col_bootstrap <- Call$xvars<- NULL
Call$text_to_show <- Call$cex_text <- Call$col_text <- Call$show.legend <- Call$colour_bucket <- NULL
 the.names <- if (is.null(text_to_show)) as.character(Call[2:(length(object)+1)])
              else text_to_show
#         n <- if(isvector) length(object[[1]]) else object[[1]]$N
length.obj <- length(object)
       DA0 <- DA <- NULL
# we need a data.frame with models
################################################################################
    lresid <- if (is(object[[1]],"gamlss")) length(resid(object[[1]]))
              else length(object[[1]])
# declaring DA0
      DA0 <- matrix(0, nrow=lresid, ncol= length(object))
# get the residuals matrix
for (j in 1:length(object))
  {
    DA0[,j] <-  if (is(object[[j]],"gamlss")) resid(object[[j]])
    else object[[j]]
  }
#  get the x-variable
if (missing(xvar)) stop("moment_buckets_wrap() expects one xvar")
      z <- if (is.factor(xvar))  xvar else cut_number(xvar,n_inter)
# add it to DA0 you needed to create DA1
    DA0 <- as.data.frame(DA0)
colnames(DA0) <- the.names
    DA0 <- as.data.frame(cbind(DA0, z))
    DA1 <- da2 <- DA3 <- NULL # declaring all data.frame's
for (i in levels(DA0$z))# get the right subset
  {
      DA <- DA0[DA0$z==i,] # DA is the right subset from DA0
     nDA <- dim(DA)[1]     # hthe length of the DA
for (j in 1:length(the.names))
    {
      # if (bootstrap)
      #   {
      bootx <- booty <- rep(0,no_bootstrap)
    for (b in 1:no_bootstrap)
      {
        ind <- sample(nDA, nDA,  replace = TRUE)
         sk <- momentSK(DA[ind,j], weights=weights[j])
   bootx[b] <- sk$trans.mom.skew
   booty[b] <- sk$trans.mom.kurt
      }# finish bootstrap
        DA1 <- rbind(DA1, data.frame(X = bootx, Y = booty,
                  Model = rep(the.names[j], no_bootstrap),
                    Col = rep(col_bootstrap[j], no_bootstrap),
                      Z = rep(i, no_bootstrap)) )
         k <-  momentSK(DA[,j], weights=weights)
       da2 <- rbind(da2, data.frame(X=k$trans.mom.skew, Y=k$trans.mom.kurt, Model=the.names[j], Z=i))
        xx <- seq(0, 0.99, length=101)
        y1 <- CI95(xx, n=nDA)
        y2 <- CI95.2(-xx,  n=nDA)
        y3 <- CI95.3(-xx, n=nDA)
        y4 <- CI95.4(xx, n=nDA)
        xy <- na.omit(data.frame(X=c(xx,xx[101:1],-xx,-xx[101:1]), Y=c(y1,y4[101:1],y3,y2[101:1]), Z=i))
        DA3 <- rbind(DA3, xy)
    } # end of j for models 1 to number of models
  } # end of the data subsets loop

#---------------------------------------------------------
   GG <- if(colour_bucket == TRUE) moment_colour_both(legend=FALSE,line_width=.3)
            else moment_gray_both(line_width=.3)
   gg <- GG +  geom_polygon(data=DA3, aes(x=X, y=Y, alpha = alpha_JB_test), show.legend=FALSE, fill=col_JB_test)+
    geom_point(data=DA1, aes(x=X,y=Y), colour=DA1$Col, alpha=alpha_bootstrap)+
    geom_text(data=da2, aes(x=X,y=Y, label=Model), colour="black", check_overlap =check_overlap_text )+
    facet_wrap(~Z)
  return(suppressWarnings(gg))
}
#################################################################################
#################################################################################
#################################################################################
#################################################################################
# # FUNCTION 11
#################################################################################
#################################################################################
#################################################################################
#################################################################################
centile_bucket_wrap <- function(x,...,
                            type = c("tail", "central"),
                         weights = NULL,
                            xvar = NULL,
                         n_inter = 4,
                    no_bootstrap = 99,
                   col_bootstrap = hcl.colors(length.obj, palette="Set 2"),# see hcl.pals()
                 alpha_bootstrap = 1,
                    text_to_show = NULL,
              check_overlap_text = FALSE,
                        cex_text = 5,
                        col_text = "black",#
                   colour_bucket = FALSE,
                      line_width = 0.5,
                        sim_test = FALSE,
                     no_sim_test = 1000,
                    col_sim_test = gray(.7),
                  alpha_sim_test = 0.1,
                       seed_test = 1234)
{
################################################################################
  # centSim <- function(n, nboot)
  # {
  #   boot <- matrix(0, nrow=nboot, ncol=3)
  #   for (j in 1:nboot)
  #   {
  #     ind <- rNO(n)
  #     sk <-  centileSK(ind)
  #     boot[j,] <- c(sk$S0.25, sk$S0.01, sk$trans.K0.01)
  #   }
  #   colnames(boot) <- c("Sc", "St", "Kt")
  #   as.data.frame(boot)
  # }
################################################################################
  # main function stats here
  # checking whether model ro get the residual otherwise data
      object <- list(x, ...)
        type <- match.arg(type)
          Ke <- St <- Sc <- NULL
    isvector <- all(unlist(lapply(object, is.numeric)))
   # isgamlss <- all(unlist(lapply(object, is.gamlss)))
        Call <- match.call()          ## trying to get the names
Call$weights <- Call$bootstrap <- Call$no_bootstrap <- Call$col_bootstrap <- Call$xvars<- NULL
Call$text_to_show <- Call$cex_text <- Call$col_text <- Call$show.legend <- Call$colour_bucket <- NULL
   the.names <- if (is.null(text_to_show)) as.character(Call[2:(length(object)+1)])
                else text_to_show
          n <- if(isvector) length(object[[1]]) else object[[1]]$N
  length.obj <- length(object)
  S0.25 <- S0.01 <- K0.01 <- Model <- NULL
         DA0 <- matrix(0, nrow=n, ncol= length.obj )
for (j in 1:length(object))
 {
    DA0[,j] <-  if (is(object[[j]],"gamlss")) resid(object[[j]])
    else object[[j]]
}
  DA0 <- as.data.frame(DA0)
  colnames(DA0) <- the.names
  DA1 <- da2 <- NULL
################################################################################
#  get the x-variable
if (missing(xvar)) stop("centile_buckets_wrap() expects one xvar")
   z <- if (is.factor(xvar))  xvar else cut_number(xvar,n_inter)
# add it to DA0
          DA0 <- as.data.frame(DA0)
colnames(DA0) <- the.names
          DA0 <- as.data.frame(cbind(DA0, z))
   DA1 <- da2 <- DA3 <- NULL # declaring all data.frame's
for (i in levels(DA0$z))# get the right subset
  {
     DA <- DA0[DA0$z==i,] # DA is the right subset from DA0
    nDA <- dim(DA)[1]     # hthe length of the DA
for (j in 1:length(the.names))
    {
  bootx <- booty <- bootz <- rep(0,no_bootstrap)
    for (b in 1:no_bootstrap)
      {
        ind <- sample(nDA, nDA,  replace = TRUE)
         sk <- centileSK(DA[ind,j], weights=weights[j])
         bootx[b] <- sk$S0.25
         booty[b] <- sk$S0.01
         bootz[b] <- sk$trans.K0.01
      }# finish bootstrap
      DA1 <- rbind(DA1, data.frame(S0.25 = bootx,
                                   S0.01 = booty,
                                   K0.01 = bootz,
                                   Model = rep(the.names[j], no_bootstrap),
                                     Col = rep(col_bootstrap[j], no_bootstrap),
                                       Z = rep(i, no_bootstrap)) )
      k <-  centileSK(DA[,j], weights=weights)
      da2 <- rbind(da2, data.frame(S0.25 = k$S0.25,
                                   S0.01 = k$S0.01,
                                   K0.01 = k$trans.K0.01,
                                   Model=the.names[j],
                                   Z=i))
      if (sim_test)
      {
        CC <- centSim(n=nDA, n.sim=no_sim_test, seed.number=seed_test)
        DD <- drawEllipse2(CC, type=type)
        DD <- data.frame(DD, Z=i) 
       DA3 <- rbind(DA3, DD) 
      }
      
    } # end of j for models 1 to number of models
  } # end of the data subsets loop
  #---------------------------------------------------------
  GG <- if (colour_bucket == TRUE) centile_colour_both(type=type, legend = FALSE, line_width = line_width) else  centile_gray_both(type=type, line_width=line_width)

   
if (sim_test)   
{
  if ( type  == "central")
  {
    gg <- GG +
      ggplot2::geom_polygon(data=DA3, 
            ggplot2::aes(x=Sc, y=Ke, alpha=alpha_sim_test),  fill=col_sim_test)+
      ggplot2::geom_point(data=DA1, ggplot2::aes(x=S0.25,y=K0.01), 
                          colour=DA1$Col, alpha=alpha_bootstrap)+
      ggplot2::geom_text(data=da2, ggplot2::aes(x=S0.25,y=K0.01, label=Model), 
                         colour="black", check_overlap =check_overlap_text )+
      ggplot2::facet_wrap(~Z)+ 
      ggplot2::theme(legend.position = "none")
  } else
  {
    gg <- GG +
      ggplot2::geom_polygon(data=DA3, 
          ggplot2::aes(x=St, y=Ke, alpha=alpha_sim_test),  fill=col_sim_test)+
      ggplot2::geom_point(data=DA1, 
         ggplot2::aes(x=S0.01, y=K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      ggplot2::geom_text(data=da2,  
        ggplot2::aes(x=S0.01, y=K0.01,label=Model), colour="black", 
        check_overlap =check_overlap_text )+
      ggplot2::facet_wrap(~Z)+ 
      ggplot2::theme(legend.position = "none")
  }
} else
{
  if ( type  == "central")
  {
    gg <- GG +
      ggplot2::geom_point(data=DA1, 
        ggplot2::aes(x=S0.25,y=K0.01), colour=DA1$Col, alpha=alpha_bootstrap)+
      ggplot2::geom_text(data=da2, 
      ggplot2::aes(x=S0.25,y=K0.01, label=Model), colour="black", 
      check_overlap =check_overlap_text )+
      ggplot2::facet_wrap(~Z)+ 
      ggplot2::theme(legend.position = "none")
  } else
  {
    gg <- GG +
      ggplot2::geom_point(data=DA1, ggplot2::aes(x=S0.01, y=K0.01), 
                          colour=DA1$Col, alpha=alpha_bootstrap)+
      ggplot2::geom_text(data=da2,  ggplot2::aes(x=S0.01, y=K0.01,label=Model), 
                         colour="black", check_overlap =check_overlap_text )+
      ggplot2::facet_wrap(~Z)+ 
      ggplot2::theme(legend.position = "none")
  }  
}  
  
  return(suppressWarnings(gg))
}
################################################################################
################################################################################
################################################################################
################################################################################
  model_mom_bucket <- moment_bucket
 model_cent_bucket <- centile_bucket
################################################################################
################################################################################
################################################################################
################################################################################
# EXTRA FUNCTIONS used in centile_bucket
 centSim <- \(n, n.sim=1000, seed.number=123)
 {
   set.seed(seed.number)
     simu <- matrix(0, nrow=n.sim, ncol=5)
       pb <- txtProgressBar(max = n.sim, style = 3)
   for (j in 1:n.sim)
   {
     setTxtProgressBar(pb, j)
       ind <- rNO(n)
        sk <-  centileSK(ind)
  simu[j,] <- c(sk$S0.25, sk$S0.01, sk$K0.01, sk$exc.K0.01, sk$trans.K0.01)
   }
   colnames(simu) <- c("Sc", "St", "K", "Ke", "Kt")
   return(as.data.frame(simu))
 }
################################################################################
################################################################################
################################################################################
################################################################################
# graw the ellipse
drawEllipse2 <- function(simulation,  type = c("tail", "central"))
 {
     Sx <- cov(simulation) 
     Rx <- cor(simulation)
   Mean <- colMeans(simulation)
   type <- match.arg(type) 
theList <- list(tail=c("St","Ke"),  central=c("Sc", "Ke"))  
   type <-  theList[[type]] 
     sx <- Sx[type,type]
   corx <- Rx[type,type] 
     mx <- Mean[type]
     DD <- ellipse::ellipse(corx,  scale=sqrt(diag(sx)),  centre=mx, level=0.95)
 DD[,2] <- DD[,2]/(1 + abs(DD[,2]))
   return(as.data.frame(DD))  
 } 
################################################################################
################################################################################
################################################################################
################################################################################