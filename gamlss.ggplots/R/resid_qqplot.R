################################################################################
################################################################################
################################################################################
################################################################################
resid_qqplot <- function(obj, resid,
                              value = 3, 
                         points.col = "steelblue4", 
                           line.col = "darkgray",
                      check_overlap = TRUE,
                               title)
{
################################################################################
# local function 
  gamlss_prep_data <- function (obj, value=3) 
  {
      rqres <- residuals(obj)
#  rqres_out <- abs(rqres) > value
        obs <- seq_len(length(rqres))
   #outlier <- rqres[rqres_out]
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
################################################################################
other_prep_data <- function (resid, value=2) 
  {
      rqres <- resid
 # rqres_out <- abs(rqres) > value
        obs <- seq_len(length(rqres))
 #   outlier <- rqres[rqres_out]
        obs <- obs[!is.na(resid)]
      rqres <- rqres[!is.na(resid)]
          x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
        out <- data.frame(obs = obs, rqres = rqres, x=x)
  out$color <- ifelse(((out$rqres >= value) | (out$rqres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                                                           "outlier"))
    out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
  }    
################################################################################
if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
            d <- if (missing(obj)) other_prep_data(resid, value=value) 
                else               gamlss_prep_data(obj,   value=value) 
    txt.title <- if (missing(title))  
      paste("QQ-plot of the residuals of model",deparse(substitute(obj)))
            else title   
x <- rqres <- txt <- NULL    
gg <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = rqres, label = txt)) + 
  ggplot2::geom_point(colour = points.col ) + # shape = 1, 
  ggplot2::xlab("Theoretical") + 
  ggplot2::ylab("Residuals") +
  ggplot2::ggtitle(txt.title) +
  ggplot2::geom_point(shape = 1, colour = points.col  ) + 
  ggplot2::geom_line(ggplot2::aes(x, x), color=line.col)+
  ggplot2::geom_text(hjust = -0.2, nudge_x = 0.05, size = 3,
                check_overlap = check_overlap, family = "serif", 
            fontface = "italic", colour = "darkred", na.rm = TRUE) 
    return(gg)
}
######################################################################## 
######################################################################## 
######################################################################## 
######################################################################## 
# add_resid_qqplot <- function(gg,  obj, resid,
#                              value = 3, 
#                              points.col = "sienna", 
#                              line.col = "darkgray",
#                              check_overlap = TRUE,
#                              title)
# {
#   ########################################################################
#   # local function 
#   gamlss_prep_data <- function (obj, value=3) 
#   {
#     color <- NULL
#     rqres <- obj$residuals
#     rqres_out <- abs(rqres) > value
#     obs <- seq_len(length(rqres))
#     outlier <- rqres[rqres_out]
#     obs <- obs[obj$weights!=0]
#     rqres <- rqres[obj$weights!=0]
#     x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
#     out <- data.frame(obs = obs, rqres = rqres, x=x)
#     out$color <- ifelse((abs(out$rqres) >= value), 
#                         c("outlier"), c("normal"))
#     out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
#     out$txt <- ifelse(out$color == "outlier", out$obs, NA)
#     return(out)
#   }
#   #####################################################################
#   other_prep_data <- function (resid, value=2) 
#   {
#     color <- NULL
#     rqres <- resid
#     rqres_out <- abs(rqres) > value
#     obs <- seq_len(length(rqres))
#     outlier <- rqres[rqres_out]
#     obs <- obs[!is.na(resid)]
#     rqres <- rqres[!is.na(resid)]
#     x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
#     out <- data.frame(obs = obs, rqres = rqres, x=x)
#     out$color <- ifelse(((out$rqres >= value) | (out$rqres <= -value)), 
#                         c("outlier"), c("normal"))
#     out$fct_color <- ordered(factor(out$color), levels = c("normal", 
#                                                            "outlier"))
#     out$txt <- ifelse(out$color == "outlier", out$obs, NA)
#     return(out)
#   }    
#   #######################################################################
#   if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
#   if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
#   d <- if (missing(obj)) other_prep_data(resid, value=value) 
#   else               gamlss_prep_data(obj,   value=value) 
#   txt.title <- if (missing(title))  paste("QQ-plot of the residuals of model",deparse(substitute(obj)))
#   else title   
#   x <- rqres <- txt <- NULL    
#   gg <- gg + geom_point(data=d,  colour = points.col) +
#     ggtitle(txt.title) +
#     # geom_point(shape = 1, colour = "steelblue4"  ) + 
#     # geom_line(aes(x, x), color=line.col)+
#     geom_text(hjust = -0.2, nudge_x = 0.05, size = 3,
#               check_overlap = check_overlap, family = "serif", 
#               fontface = "italic", colour = "darkblue", na.rm = TRUE) 
#   return(gg)
# }
# #########################################################################
# #########################################################################
# #########################################################################
# #########################################################################
