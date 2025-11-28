################################################################################
################################################################################
################################################################################
################################################################################
model_qqplot <- function(obj,..., 
                         # value = 3, 
                       line.col = "steelblue4",
                         title)
{
################################################################################
# local function
gamlss_prep_data <- function (obj, ... ) 
  {
         rqres <- obj$residuals
           obs <- seq_len(length(rqres))
         rqres <- rqres[obj$weights!=0]
           obs <- obs[obj$weights!=0]
         rqres <- rqres[!is.na(rqres)]  
           obs <- obs[!is.na(rqres)]
             x <- qnorm(ppoints(length(rqres)))[order(order(rqres))]
           out <- data.frame(obs = obs, rqres = rqres, x=x, model=rep(names[[1]], length(rqres))) 
  #  rqres <- rqres[obj$weights!=0]
   # out <- data.frame(rqres = rqres, )
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
         resa <- data.frame(obs = obs, rqres=res, x=x, model=rep(names[[i]], length(rqres))) 
          out <- rbind(out, resa)
      }
    }
    return(out)    
}    
################################################################################
################################################################################
x <- rqres  <- model <-  NULL   
   names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
if (length(names)<=1) stop("you need more than two models")
       d <- gamlss_prep_data(obj, ...)
       table(d$model)
txt.title <- if (missing(title))  "QQ-plot of the residuals of different models" else title   
gg <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = rqres, color = model)) + 
  ggplot2::geom_point( ) + # shape = 1, 
  ggplot2::xlab("Theoretical") + 
  ggplot2::ylab("Residuals") +
  ggplot2::ggtitle(txt.title) +
  ggplot2::geom_line(aes(x, x), color=line.col)
    return(gg)
}
################################################################################
################################################################################
################################################################################
################################################################################

