#########################################################################
#########################################################################
# function resid_wp_wrap
######################################################################### 
resid_wp_wrap <- function(obj, resid,
                       value = 3, 
                        xvar = NULL,
                     n_inter = 4,
                  points_col = "steelblue4", 
                    poly_col = "darkred",
                 alpha_bound = 0.1,
               check_overlap = TRUE,
                     title,
                     ylim)
{
########################################################################
# local function 
gamlss_prep_data <- function (obj, value=3, i) 
  {
    rqres <- obj$residuals
      obs <- seq_len(length(obj$residuals))
      obs <- obs[obj$weights!=0]
      obs <- obs[z==i]
    rqres <- rqres[obj$weights!=0&z==i]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      # s <- splinefun(x, fitted(fit))
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))
out$color <- ifelse((abs(rqres) >= value), c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}
#####################################################################
other_prep_data <- function (resid, value=3, i) 
{
    rqres <- resid
      obs <- seq_len(length(rqres))
      obs <- obs[!is.na(resid)]
      obs <- obs[z==i]
    rqres <- rqres[!is.na(resid)&z==i]
        x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
      fit <- lm(I(rqres-x) ~ x+I(x^2)+I(x^3)) #poly(qq$x,3)) 
      out <- data.frame(obs = obs, rqres = rqres-x, x=x, fv=fitted(fit))    
out$color <- ifelse(abs(rqres) >= value, c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
 out$txt <- ifelse(out$color == "outlier", out$obs, NA)
    return(out)
}    
###########################################################################
getSE <- function(xlim, level=0.95, N=NULL)
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
####################################################################### 
#######################################################################    
if (missing(obj)&&missing(resid))  stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
  #      N <- if (missing(obj)) length(resid) else obj$noObs
if (missing(xvar)) stop("moment_buckets_wrap() expects one xvar")
        z <- if (is.factor(xvar))  xvar else cut_number(xvar, n_inter)
# loop for i lebvels z   
  DA1 <- DA2 <- NULL
for (i in levels(z))# get the right subset
{
  N.i <- sum(z==i)
        d <- if (missing(obj)) other_prep_data(resid, value=value, i) 
             else               gamlss_prep_data(obj, value=value, i)
        se <- getSE(max(abs(d$x))+.5, N=N.i)
         d <- data.frame(d,Z=i)
       DA1 <- data.frame(rbind(DA1,d))
        se <- data.frame(se,Z=i)
       DA2 <- data.frame(rbind(DA2,se))
}
        x <- rqres <- z <- low <- high <- txt <- NULL
txt.title <- if (missing(title))  paste("Worm-plot for model", deparse(substitute(obj)))
             else title  
     ymax <- if (missing(ylim))  (max(abs(DA1$rqres))+0.1) else ylim
       gg <- ggplot2::ggplot(data=DA1, aes(x = x, y = rqres)) + 
         ggplot2::geom_point(  color =  points_col, alpha=.8 ) + 
         ggplot2::geom_line(data = DA1, aes(x = x, y = fv), lty=1, colour=poly_col)+
         ggplot2::geom_ribbon(data=DA2, aes(ymin = low, ymax = high, x = z), 
                              alpha = alpha_bound)+
         ggplot2::geom_line(data = DA2, aes(x = z, y = low), lty=2)+
         ggplot2::geom_line(data = DA2, aes(x = z, y = high), lty=2)+
         ggplot2::xlab("Unit normal quantile") + 
         ggplot2::facet_wrap(~Z)+ theme(legend.position = "none")+
         ggplot2::ylab("Deviation") +
         ggplot2::coord_cartesian(ylim = c(-ymax, ymax)) +
         ggplot2::geom_hline(yintercept = 0, colour = "gray")+
         ggplot2::geom_vline(xintercept = 0, colour = "gray")+
         ggplot2::geom_text(data = DA1, aes(x = x, y = rqres, label = txt),
             hjust = -0.2, nudge_x = 0.05, size = 3,
             check_overlap = check_overlap, family = "serif", 
             fontface = "italic", colour = "darkred", na.rm = TRUE)+
         ggplot2::ggtitle(txt.title)
  suppressWarnings(return(gg))
}
#######################################################################
#######################################################################
#######################################################################
#######################################################################
