################################################################################
################################################################################
################################################################################
################################################################################ 
model_wp <- function(obj,..., 
                         title)
{
################################################################################
# local function
gamlss_prep_data <- function (obj, ... ) 
  {
         rqres <- obj$residuals
           obs <- seq_len(length(rqres))
         rqres <- rqres[!is.na(rqres)]  
           obs <- obs[!is.na(rqres)]
             x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
           out <- data.frame(obs = obs, rqres = rqres-x, x=x, model=rep(names[[1]], length(rqres))) 
    if (length(list(...)) > 0) 
    {
      i=1
      for (resp in list(...)) 
      {
        i= i+1
          res <- resp[["residuals"]] 
          obs <- seq_len(length(res))
          wei <- resp[["weights"]] 
          res <- res[wei!=0]
          obs <- obs[wei!=0]
          res <- res[!is.na(res)]
          obs <- obs[!is.na(res)]
            x <- qnorm(ppoints(length(res)))[order(order(res))]
         resa <- data.frame(obs = obs, rqres=res-x, x=x, model=rep(names[[i]], length(rqres))) 
         out <- rbind(out, resa)
      }
    }
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
################################################################################
x <- rqres  <- model <-  low <- high <- z <- NULL   
   names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
if (length(names)<=1) stop("you need more than two models")
       N <-  obj$noObs
       d <- gamlss_prep_data(obj, ...)
       table(d$model)
txt.title <- if (missing(title))  "worm-plots of residuals from different models" else title   
   se <- getSE(max(abs(d$x))+.5)
gg <- ggplot2::ggplot() + 
  ggplot2::geom_ribbon(data=se, 
          ggplot2::aes(ymin = low, ymax = high, x = z), alpha = 0.1)+
  ggplot2::geom_point(data = d, aes(x = x, y = rqres, color=model),  alpha=.8 ) + 
  # shape = 1, must include argument label "data"
  ggplot2::geom_line(data = se, ggplot2::aes(x = z, y = low), lty=2)+
  ggplot2::geom_line(data = se, ggplot2::aes(x = z, y = high), lty=2)+
  ggplot2::xlab("Unit normal quantile") + 
  ggplot2::ylab("Deviation")+
  ggplot2::geom_hline(yintercept = 0, colour = "gray")+
  ggplot2::geom_vline(xintercept = 0, colour = "gray")+
  ggplot2::ggtitle(txt.title)
    return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################

