################################################################################
################################################################################
################################################################################
################################################################################
model_wp_wrap <- function(obj,..., 
                       xvar = NULL,
                      value = 3,
                    n_inter = 4,
                 points_col = "steelblue4", 
                alpha_bound = 0.1,
              check_overlap = TRUE,
                          ylim,
                         title)
{
################################################################################
# local function
gamlss_prep_data <- function (obj, value=3, i, ...) 
  {
         rqres <- obj$residuals
           obs <- seq_len(length(obj$residuals))
           obs <- obs[obj$weights!=0]
           obs <- obs[z==i]
         rqres <- rqres[obj$weights!=0&z==i] 
             x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
           out <- data.frame(obs = obs, rqres = rqres-x, x=x, model=rep(names[[1]], length(rqres))) 
    if (length(list(...)) > 0) 
    {
      JJ=1
      for (resp in list(...)) 
      {
        JJ= JJ+1
          res <- resp[["residuals"]] 
          obs <- seq_len(length(res))
          wei <- resp[["weights"]] 
         # res <- res[wei!=0]
          obs <- obs[wei!=0]
          obs <- obs[z==i]
          res <- res[wei!=0]
          res <- res[z==i]
        # res <- res[!is.na(res)]
         #obs <- obs[!is.na(res)]
            x <- qnorm(ppoints(length(res)))[order(order(res))]
         resa <- data.frame(obs = obs, rqres=res-x, x=x, model=rep(names[[JJ]], length(res))) 
         out <- rbind(out, resa)
      }
    }
    return(out)    
}  
################################################################################
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
################################################################################
################################################################################
x <- rqres  <- model <-  low <- high <- z <- NULL   
   names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
if (length(names)<=1) stop("you need more than two models")
if (missing(xvar)) stop("moment_buckets_wrap() expects one xvar")
       z <- if (is.factor(xvar))  xvar else cut_number(xvar, n_inter) 
# loop for i levels   
     DA1 <- DA2 <- NULL
for (i in levels(z))# get the right subset
{
       N.i <- sum(z==i)
         d <- gamlss_prep_data(obj, value=value, i, ...)
        se <- getSE(max(abs(d$x))+.5, N=N.i)
         d <- data.frame(d,Z=i)
       DA1 <- data.frame(rbind(DA1,d))
        se <-  data.frame(se,Z=i)
       DA2 <- data.frame(rbind(DA2,se))
}     
txt.title <- if (missing(title))  "worm-plots of residuals from different models" 
      else title   
ymax <- if (missing(ylim))  (max(abs(DA1$rqres))+0.1) else ylim
gg <- ggplot2::ggplot(data=DA1, ggplot2::aes(x = x, y = rqres,  color=model)) + 
  ggplot2::geom_point(  alpha=.8 ) + # shape = 1, must include argument label "data"
  ggplot2::geom_ribbon(data=DA2, aes(ymin = low, ymax = high, x = z), 
                       alpha = alpha_bound)+
  ggplot2::geom_line(data = DA2, aes(x = z, y = low), lty=2)+
  ggplot2::geom_line(data = DA2, aes(x = z, y = high), lty=2)+
  ggplot2::xlab("Unit normal quantile") + 
  ggplot2::facet_wrap(~Z)+ 
  ggplot2::ylab("Deviation") +
  ggplot2::coord_cartesian(ylim = c(-ymax, ymax)) +
  ggplot2::geom_hline(yintercept = 0, colour = "gray")+
  ggplot2::geom_vline(xintercept = 0, colour = "gray")+
  ggplot2::ggtitle(txt.title)
suppressWarnings(return(gg))
}
################################################################################
################################################################################
################################################################################
################################################################################ 
