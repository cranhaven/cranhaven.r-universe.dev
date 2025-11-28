################################################################################
################################################################################
################################################################################
################################################################################
resid_wp <- function(obj, resid,
                     value = 3, 
                points_col = "steelblue4", 
                  poly_col = "darkred",
             check_overlap = TRUE,
                     title,
                     ylim)
{
################################################################################
# local function 
gamlss_prep_data <- function (obj, value=3) 
  {
  #  color <- NULL
    rqres <- obj$residuals
#rqres_out <- abs(rqres) > value
      obs <- seq_len(length(rqres))
  #outlier <- rqres[rqres_out]
      obs <- obs[obj$weights!=0]
    rqres <- rqres[obj$weights!=0]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      # s <- splinefun(x, fitted(fit))
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))
out$color <- ifelse((abs(rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}
################################################################################
other_prep_data <- function (resid, value=2) 
{
   # color <- NULL
    rqres <- resid
#rqres_out <- abs(rqres) > value
      obs <- seq_len(length(rqres))
#  outlier <- rqres[rqres_out]
      obs <- obs[!is.na(resid)]
    rqres <- rqres[!is.na(resid)]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))    
out$color <- ifelse((abs(rqres) >= value), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", 
                         "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}    
################################################################################
getSE <- function(xlim, level=0.95)
{
      lz <- -xlim
      hz <- xlim 
      dz <- 0.25
       z <- seq(lz,hz,dz)
       p <- pnorm(z)   
      se <- (1/dnorm(z))*(sqrt(p*(1-p)/N))    
     low <- qnorm((1-level)/2)*se
    high <- -low
data.frame(high=high, low=low, z=z)
}
################################################################################    
if (missing(obj)&&missing(resid))   stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
        N <- if (missing(obj)) length(resid) else obj$noObs
        d <- if (missing(obj)) other_prep_data(resid, value=value) 
             else               gamlss_prep_data(obj, value=value)
        x <- rqres <- z <- low <- high <- txt <- NULL
txt.title <- if (missing(title))  paste("Worm-plot for model", deparse(substitute(obj)))
             else title  
       se <- getSE(max(abs(d$x))+.5)
     ymax <- if (missing(ylim))  max(abs(d$rqres))+0.1 else ylim
       gg <- ggplot2::ggplot() + 
         ggplot2::geom_ribbon(data=se, ggplot2::aes(ymin = low, ymax = high, x = z), 
                              alpha = 0.1)+
         ggplot2::geom_point(data = d, ggplot2::aes(x = x, y = rqres),  color =  
                               points_col, alpha=.8 ) +
         ggplot2::geom_line(data = d, ggplot2::aes(x = x, y = fv), lty=1, 
                            colour=poly_col)+
         ggplot2::geom_line(data = se, ggplot2::aes(x = z, y = low), lty=2)+
         ggplot2::geom_line(data = se, ggplot2::aes(x = z, y = high), lty=2)+
         ggplot2::xlab("Unit normal quantile") + 
         ggplot2::ylab("Deviation") +
         ggplot2::coord_cartesian(ylim = c(-ymax, ymax)) +
         ggplot2::geom_hline(yintercept = 0, colour = "gray")+
         ggplot2::geom_vline(xintercept = 0, colour = "gray")+
         ggplot2::geom_text(data = d, ggplot2::aes(x = x, y = rqres, label = txt),
              hjust = -0.2, nudge_x = 0.05, size = 3,
              check_overlap = check_overlap, family = "serif", 
              fontface = "italic", colour = "darkred", na.rm = TRUE)+
         ggplot2::ggtitle(txt.title)
  suppressWarnings(return(gg))
}
################################################################################
################################################################################
################################################################################
################################################################################

