resid_density <- function(obj, resid,
                          hist.col = "black", 
                          hist.fill = "white",
                          dens.fill = "#FF6666",
                          title)
{
  ########################################################################
  # local functions 
  gamlss_prep_data <- function (obj) 
  {
    rqres <- residuals(obj)
    obs <- seq_len(length(rqres))
    obs <- obs[obj$weights!=0]
    rqres <- rqres[obj$weights!=0]
    out <- data.frame(obs = obs, rqres = rqres)
    return(out)
  }  
  ########################################################################
  other_prep_data <- function (resid) 
  {
    rqres <- resid
    obs <- seq_len(length(rqres))
    obs <- obs[!is.na(resid)]
    rqres <- rqres[!is.na(resid)]
    out <- data.frame(obs = obs, rqres = rqres)
    return(out)
  }  
  ######################################################################## 
  rqres <- NULL 
  if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) stop("the model is not a gamlss model")
  d <- if (missing(obj)) other_prep_data(resid) 
  else             gamlss_prep_data(obj) 
  txt.title <- if (missing(title))   paste("Quantile residuals of model",deparse(substitute(obj)))
  else title
  f <- d[d$color == "outlier", c("obs", "rqres")]
  colnames(f) <- c("observation", "quan_resid")
  gg <- ggplot(d, aes(x=rqres))+
    geom_histogram(aes(y=after_stat(density)),binwidth = 0.2, colour=hist.col, 
                   fill=hist.fill)+
    geom_density(alpha=0.2, fill=dens.fill)+
    xlab("Quantile Residuals") + 
    ylab("density") + 
    ggtitle(txt.title) 
  return(gg)
}
#########################################################################
#########################################################################
#########################################################################
#########################################################################