######################################################################## 
######################################################################## 
######################################################################## 
######################################################################## 
add_resid_qqplot <- function(gg,  obj,
                             value = 3, 
                             points.col = "sienna", 
                             line.col = "darkgray",
                             check_overlap = TRUE,
                             title)
{
########################################################################
# local function 
gamlss_prep_data <- function (obj, value=3) 
  {
        rqres <- obj$residuals
 #   rqres_out <- abs(rqres) > value
          obs <- seq_len(length(rqres))
#      outlier <- rqres[rqres_out]
          obs <- obs[obj$weights!=0]
        rqres <- rqres[obj$weights!=0]
            x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
          out <- data.frame(obs = obs, rqres = rqres, x=x)
    out$color <- ifelse((abs(out$rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
      out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}
#####################################################################

#######################################################################
if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
          d <-  gamlss_prep_data(obj,   value=value) 
  txt.title <- if (missing(title))  paste("QQ-plot of the residuals of model",deparse(substitute(obj)))
  else title   
        gg <- gg + ggplot2::geom_point(data=d,  colour = points.col) +
          ggplot2::ggtitle(txt.title) +
          ggplot2::geom_text(hjust = -0.2, nudge_x = 0.05, size = 3,
              check_overlap = check_overlap, family = "serif", 
              fontface = "italic", colour = "darkblue", na.rm = TRUE) 
  return(gg)
}
#########################################################################
#########################################################################
#########################################################################
#########################################################################