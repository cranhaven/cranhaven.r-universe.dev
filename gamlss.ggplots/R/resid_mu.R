################################################################################
################################################################################
################################################################################
################################################################################
#   first function
################################################################################
################################################################################
################################################################################
################################################################################   
resid_mu <- function (obj, resid, plot = TRUE, value=2, title, annotate=TRUE) 
{
# Note that I am taking the 
# obj$resid rather resid(obj) so I can preserve the no of the observations
################################################################################
# local functions 
gamlss_prep_data <- function (obj, value=2) 
{
    sdres <- residuals(obj)
       fv <- obj$mu.fv
#sdres_out <- abs(sdres) > value
      obs <- seq_len(length(sdres))
#  outlier <- sdres[sdres_out]
      obs <- obs[obj$weights!=0]
    sdres <- sdres[obj$weights!=0]
       fv <- fv[obj$weights!=0]
      out <- data.frame(obs = obs, sdres = sdres, fv)
out$color <- ifelse(((out$sdres >= value) | (out$sdres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
  return(out)
  } 
################################################################################
  other_prep_data <- function (resid, value=2) 
  {
    sdres <- resid
#sdres_out <- abs(sdres) > value
      obs <- seq_len(length(sdres))
#  outlier <- sdres[sdres_out]
      obs <- obs[!is.na(resid)]
    sdres <- sdres[!is.na(resid)]
      out <- data.frame(obs = obs, sdres = sdres)
out$color <- ifelse(((out$sdres >= value) | (out$sdres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                                                           "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
return(out)
}  
################################################################################
  # the main function starts here  
if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
  if (!missing(obj)&&!(is.gamlss(obj)|is(obj, "gamlss2"))) stop("the model is not a gamlss model")
d <- if (missing(obj)) other_prep_data(resid, value=value) 
     else             gamlss_prep_data(obj,   value=value) 
txt.title <- if (missing(title))  paste("Residuals & fitted vals of model",deparse(substitute(obj)))
else title
#      obs <- NULL
    sdres <- NULL
      txt <- NULL
        f <- d[d$color == "outlier", c("obs", "sdres")]
colnames(f) <- c("observation", "quan_resid")
# try colors() for different colors
  p <- ggplot2::ggplot(d, 
       ggplot2::aes(x = fv, y = sdres, label = txt, ymin = 0, ymax = sdres)) + 
       ggplot2::geom_linerange(colour =  "steelblue4" ) + 
       ggplot2::geom_point(shape = 1, colour = "steelblue4"  ) + 
       ggplot2::geom_hline(yintercept = 0, colour = "gray") + 
       ggplot2::geom_hline(yintercept = c(value, -value), colour = "red") + 
       ggplot2::xlab("mu fitted values") + 
       ggplot2::ylab("Quantile Residuals") + 
       ggplot2::ggtitle(txt.title) + 
       ggplot2::geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, 
              family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE) + 
      if(annotate) 
        ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = value, 
             family = "serif", fontface = "italic", colour = "darkred", 
             label = paste0("Threshold: abs(", value, ")"))
  if (plot) {
    suppressWarnings(return(p))
  }
  else {
    return(list(plot = p, outliers = f, threshold = value))
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
