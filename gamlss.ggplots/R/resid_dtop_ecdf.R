######################################################################
###################################################################### 
#TODO:  Create a plot_ECDF() function
#ggplot version of dtop()
# i)  resid_dtop()
# ii) and resid_ecdf()
#  It uses the NPL.bands() function of Ross Darnell from package SMIR
# detrented Owen's plot 
#######################################################################
#######################################################################
resid_dtop <- function(obj, resid,
                    type = c("Owen", "JW"), 
              conf.level = c("95", "99"),
                   value = 2, 
              points.col = "steelblue4", 
           check_overlap = TRUE,
            title, ylim, ...)
{
########################################################################
# local functions
########################################################################  
##----------------------------------------------------------------------
## this is the NPL.bands() function of  Ross Darnell from package SMIR
##----------------------------------------------------------------------	
NPOwen.bands <- function (x, conf.level = c("95", "99")) 
{
if (!is.numeric(x)) 
      stop("argument must be numeric")
conf.level <- match.arg(conf.level)
        yn <- table(x)
        yi <- as.numeric(names(yn))
        cn <- as.numeric(cumsum(yn))
        nn <- rep(sum(yn) + 1, length(cn))
         p <- as.numeric(cn/nn)
if (conf.level == "95") 
  {
    lambda <- ifelse(nn <= 100, (3.0123 + 0.4835 * log(nn) - 
                      0.00957 * (log(nn))^2 - 0.001488 * (log(nn))^3), 
                       (3.0806 + 0.4894 * log(nn) - 0.02086 * (log(nn))^2))
  }
else 
  {
    if (conf.level == "99") 
      {
        lambda <- ifelse(nn <= 100, (-4.626 - 0.541 * log(nn) + 
                  0.0242 * (log(nn))^2), (-4.71 - 0.512 * log(nn) + 
                                              0.0219 * (log(nn))^2))
      }
    }
    lambda <- sqrt(2 * lambda)
       phi <- pbeta(p, 1/3, 1/3)
        se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
      phiu <- phi + lambda * se
      phiu <- ifelse(phiu > 1, 1, phiu)
      phil <- phi - lambda * se
      phil <- ifelse(phil < 0, 0, phil)
        pu <- qbeta(phiu, 1/3, 1/3)
        pl <- qbeta(phil, 1/3, 1/3)
    list(x = yi, lower = pl, upper = pu)
}
#######################################################################  
NPJagerWellner.bands <- function (x, conf.level = c("95", "99")) 
{
if (!is.numeric(x)) 
      stop("argument must be numeric")
    conf.level <- match.arg(conf.level)
    yn <- table(x)
    yi <- as.numeric(names(yn))
    cn <- as.numeric(cumsum(yn))
    nn <- rep(sum(yn) + 1, length(cn))
     p <- as.numeric(cn/nn)
if (conf.level == "95") 
  {
lambda <- ifelse(nn <= 100, (3.6792 + 0.5720 * log(nn) - 0.0567 * (log(nn))^2 + 
                   0.0027 * (log(nn))^3), 
                   (3.7752 + 0.5062 * log(nn) - 0.0417 * (log(nn))^2)+
                    0.0016* (log(nn)^3))
  }
else 
  {
      if (conf.level == "99") {
        lambda <- ifelse(nn <= 100, (5.3318 + 0.5539 * log(nn) - 
                                       0.00370 * (log(nn))^2), 
                         (5.6392 + 0.4018 * log(nn) - 
                            0.0183 * (log(nn))^2))
      }
  }
  lambda <- sqrt(2 * lambda)
     phi <- pbeta(p, 1/3, 1/3)
      se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
    phiu <- phi + lambda * se
    phiu <- ifelse(phiu > 1, 1, phiu)
    phil <- phi - lambda * se
    phil <- ifelse(phil < 0, 0, phil)
      pu <- qbeta(phiu, 1/3, 1/3)
      pl <- qbeta(phil, 1/3, 1/3)
    list(x = yi, lower = pl, upper = pu)
}
########################################################################
########################################################################
gamlss_prep_data <- function (obj, value=2) 
{
    #  color <- NULL
      rqres <- obj$residuals
#rqres_out <- abs(rqres) > value
        obs <- seq_len(length(rqres))
    #outlier <- rqres[rqres_out]
        obs <- obs[obj$weights!=0]
      rqres <- rqres[obj$weights!=0]
       fcdf <- ECDF(resid) # create a function 
         mm <- fcdf(resid) # evaluate it 
     zscore <- qNO(mm) # normalize it 
    dzscore <- (zscore-resid) # detrend it
        out <- data.frame(obs = obs[order(resid)], rqres = rqres[order(resid)], dzscores=dzscore[order(resid)])
  out$color <- ifelse((abs(out$rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
     out$txt <- ifelse(out$color == "outlier", out$obs, NA)
return(out)
}
#####################################################################
other_prep_data <- function (resid, value=2) 
{
    #   color <- NULL
       rqres <- resid
  # rqres_out <- abs(rqres) > value
         obs <- seq_len(length(rqres))
  #   outlier <- rqres[rqres_out]
         obs <- obs[!is.na(resid)]
       rqres <- rqres[!is.na(resid)]
        fcdf <- ECDF(resid) # create a function 
          mm <- fcdf(resid) # evaluate it 
      zscore <- qNO(mm) # normalize it 
     dzscore <- (zscore-resid) # detrend it 
         out <- data.frame(obs = obs[order(resid)], rqres = rqres[order(resid)], dzscores=dzscore[order(resid)])   
   out$color <- ifelse(((out$rqres >= value) | (out$rqres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                                                        "outlier"))
     out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}    
#######################################################################
#######################################################################   
# main starts here
if (missing(obj)&&missing(resid))   stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
  conf.level <- match.arg(conf.level)
        type <- match.arg(type)
    #    args <- list(...)
       resid <- if (missing(obj)) resid else obj$residuals
           d <- if (missing(obj)) other_prep_data(resid, value=value) 
                else              gamlss_prep_data(obj, value=value)
  #N <- if (missing(obj)) length(resid) else obj$noObs
           x <- rqres <- lower <- upper <- dzscores <- txt <- NULL
   txt.title <- if (missing(title))  paste("Owen-plot for model",deparse(substitute(obj)))
                else title  
       # ymax <- if (missing(ylim))  max(abs(d$rqres))+0.1 else ylim
          xx <- switch(type, "Owen" = NPOwen.bands(resid, conf.level=conf.level),
                       "JW"=NPJagerWellner.bands(resid, conf.level=conf.level))  
          dx <- as.data.frame(xx)
    dx$lower <-  qnorm(xx$lower)-xx$x      
    dx$upper <- qnorm(xx$upper)-xx$x
  gg <- ggplot() +
    geom_ribbon(data=dx, aes(ymin = lower, ymax = upper, x = x), alpha = 0.2)+
    geom_point(data=d, aes(x = rqres, y = dzscores), color=points.col)+
    xlab("ordered norm. quan. residuals") + 
    ylab(paste(paste(conf.level, "%", sep=""), "confident intevals")) +
    #coord_cartesian(ylim = c(-ymax, ymax)) +
    geom_hline(yintercept = 0, colour = "gray")+
    geom_text(data = d, aes(x = rqres, y = dzscores, label = txt),
              hjust = -0.2, nudge_x = 0.05, size = 3,
              check_overlap = check_overlap, family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE)+
    ggtitle(txt.title)
  #suppressWarnings(print(gg))
  return(gg)
}
########################################################################
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
resid_ecdf <- function(obj, resid,
                    type = c("Owen", "JW"), 
              conf.level = c("95", "99"),
                   value = 2, 
              points.col = "steelblue4", 
           check_overlap = TRUE,
           show.outliers = TRUE,
                    title,  ...)
{
########################################################################
# local functions
########################################################################  
##----------------------------------------------------------------------
## this is the NPL.bands() function of  Ross Darnell from package SMIR
##----------------------------------------------------------------------	
NPOwen.bands <- function (x, conf.level = c("95", "99")) 
{
if (!is.numeric(x)) stop("argument must be numeric")
 conf.level <- match.arg(conf.level)
         yn <- table(x)
         yi <- as.numeric(names(yn))
         cn <- as.numeric(cumsum(yn))
         nn <- rep(sum(yn) + 1, length(cn))
          p <- as.numeric(cn/nn)
if (conf.level == "95") 
  {
     lambda <- ifelse(nn <= 100, (3.0123 + 0.4835 * log(nn) - 
                      0.00957 * (log(nn))^2 - 0.001488 * (log(nn))^3), 
                      (3.0806 + 0.4894 * log(nn) - 0.02086 * (log(nn))^2))
  }
else 
  {
  if (conf.level == "99") 
   {
    lambda <- ifelse(nn <= 100, (-4.626 - 0.541 * log(nn) + 
                      0.0242 * (log(nn))^2), (-4.71 - 0.512 * log(nn) + 
                      0.0219 * (log(nn))^2))
   }
}
    lambda <- sqrt(2 * lambda)
       phi <- pbeta(p, 1/3, 1/3)
        se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
      phiu <- phi + lambda * se
      phiu <- ifelse(phiu > 1, 1, phiu)
      phil <- phi - lambda * se
      phil <- ifelse(phil < 0, 0, phil)
        pu <- qbeta(phiu, 1/3, 1/3)
        pl <- qbeta(phil, 1/3, 1/3)
list(x = yi, lower = pl, upper = pu)
}
#######################################################################  
NPJagerWellner.bands <- function (x, conf.level = c("95", "99")) 
  {
    if (!is.numeric(x)) 
      stop("argument must be numeric")
    conf.level <- match.arg(conf.level)
    yn <- table(x)
    yi <- as.numeric(names(yn))
    cn <- as.numeric(cumsum(yn))
    nn <- rep(sum(yn) + 1, length(cn))
    p <- as.numeric(cn/nn)
    if (conf.level == "95") 
    {
      lambda <- ifelse(nn <= 100, (3.6792 + 0.5720 * log(nn) - 0.0567 * (log(nn))^2 + 
                                     0.0027 * (log(nn))^3), 
                       (3.7752 + 0.5062 * log(nn) - 0.0417 * (log(nn))^2)+
                         0.0016* (log(nn)^3))
    }
    else 
    {
      if (conf.level == "99") {
        lambda <- ifelse(nn <= 100, (5.3318 + 0.5539 * log(nn) - 
                                       0.00370 * (log(nn))^2), 
                         (5.6392 + 0.4018 * log(nn) - 
                            0.0183 * (log(nn))^2))
      }
    }
    lambda <- sqrt(2 * lambda)
    phi <- pbeta(p, 1/3, 1/3)
    se <- 1/(5.3 * sqrt(nn * (p * (1 - p))^(1/3)))
    phiu <- phi + lambda * se
    phiu <- ifelse(phiu > 1, 1, phiu)
    phil <- phi - lambda * se
    phil <- ifelse(phil < 0, 0, phil)
    pu <- qbeta(phiu, 1/3, 1/3)
    pl <- qbeta(phil, 1/3, 1/3)
    list(x = yi, lower = pl, upper = pu)
  }
######################################################################
gamlss_prep_data <- function (obj, value=2) 
  {
   #  color <- NULL
     rqres <- obj$residuals
# rqres_out <- abs(rqres) > value
       obs <- seq_len(length(rqres))
  # outlier <- rqres[rqres_out]
       obs <- obs[obj$weights!=0]
     rqres <- rqres[obj$weights!=0]
      fcdf <- ECDF(resid) # create a function 
        mm <- fcdf(resid) # evaluate it 
       out <- data.frame(obs = obs, rqres = rqres, scores=mm)
     out$color <- ifelse((abs(out$rqres) >= value), 
                    c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
   out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
  }
#####################################################################
  other_prep_data <- function (resid, value=2) 
  {
   #  color <- NULL
     rqres <- resid
# rqres_out <- abs(rqres) > value
       obs <- seq_len(length(rqres))
 #  outlier <- rqres[rqres_out]
       obs <- obs[!is.na(resid)]
     rqres <- rqres[!is.na(resid)]
      fcdf <- ECDF(resid) # create a function 
        mm <- fcdf(resid) # evaluate it 
       out <- data.frame(obs = obs, rqres = rqres, scores=mm)
 out$color <- ifelse(((out$rqres >= value) | (out$rqres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                                                           "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
  }    
#######################################################################
#######################################################################   
  # main starts here
if (missing(obj)&&missing(resid))   stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
  conf.level <- match.arg(conf.level)
        type <- match.arg(type)
 # args <- list(...)
      resid <- if (missing(obj)) resid else obj$residuals
          d <- if (missing(obj)) other_prep_data(resid, value=value) 
  else              gamlss_prep_data(obj, value=value)
  #N <- if (missing(obj)) length(resid) else obj$noObs
          x <- rqres <- lower <- upper <- scores <- txt <- NULL
  txt.title <- if (missing(title))  paste("ECDF of residuals from model", deparse(substitute(obj)))
  else title  
  dx <- as.data.frame(switch(type, "Owen" = NPOwen.bands(resid, conf.level=conf.level),
               "JW"=NPJagerWellner.bands(resid, conf.level=conf.level)) ) 
 
  gg <- ggplot() +
    geom_ribbon(data=dx, aes(ymin = lower, ymax = upper, x = x), alpha = 0.2)+
    stat_ecdf( data=d, aes(x = rqres, y = scores), geom = "step", color=points.col)+
   # geom_point(data=d, aes(x = rqres, y = scores), color=points.col)+
    xlab("residuals") + 
    ylab(paste("ECDF and", paste0(conf.level,"%", sep=""),"C.I.")) +
    geom_hline(yintercept = 0, colour = "gray")+
    geom_hline(yintercept = 1, colour = "gray")+
    ggtitle(txt.title)+
    if (show.outliers) geom_text(data = d, aes(x = rqres, y = scores, label = txt),
              hjust = -0.2, nudge_x = 0.05, size = 3,
              check_overlap = check_overlap, family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE)
   
 # suppressWarnings(print(gg))
  return(gg)
}
########################################################################
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################