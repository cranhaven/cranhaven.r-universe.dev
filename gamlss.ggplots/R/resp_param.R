################################################################################
################################################################################
################################################################################
################################################################################
resp_param <- function (obj,
                        param = c("mu", "sigma", "nu", "tau"), 
                        title, 
                     line.col = "darkred", 
                    point.col = "steelblue4",
                  point.shape = 20
                         ) 
{
################################################################################
# local functions 
gamlss_prep_data <- function (obj, param) 
  {
             FV <- fitted(obj, param)
            obs <- obs <- seq_len(length(FV))
             FV <- FV[obj$weights!=0]
            obs <- obs[obj$weights!=0]
           yVal <- obj$y[obj$weights!=0]
            out <- data.frame(obs = obs, y = yVal, fv=FV)
#names(out)[[2]] <- paste(obj$call$formula[[2]])
  return(out)
  } 
################################################################################
################################################################################
# the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
       param <- match.arg(param)
           d <- gamlss_prep_data(obj, param) 
           y <- NULL
        corr <- with(d,cor(y,fv)) 
   txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
                else title    
          pp <- try(obj$call$formula[[2]], silent=TRUE)
  txt_ylabel <-  if (any(class(pp)%in%"try-error")) txt_ylab = "response" 
                  else  paste(obj$call$formula[[2]])
  txt_xlabel <- param
          gg <- ggplot2::ggplot(d, ggplot2::aes(x = fv, y = y)) + 
            ggplot2::geom_point(shape = point.shape, colour = point.col) + 
            ggplot2::geom_smooth(method="gam",col=line.col)+
            ggplot2::xlab(txt_xlabel) + # working  with facet_wrap 
            ggplot2::ylab(txt_ylabel) + # working  with facet_wrap 
            ggplot2::ggtitle(txt.title)   # working  with facet_wrap 
    suppressWarnings(return(gg))
}
#resp_fitted(m6)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################
resp_quantile <- function (obj, 
                         quantile = 0.5,  
                         title, 
                         newdata,
                         line.col = "darkred", 
                        point.col = "steelblue4",
                      point.shape = 20
) 
{
################################################################################
# local functions 
gamlss_prep_data <- function (obj, median) 
  {
     FV <- median
    obs <- obs <- seq_len(length(FV))
     FV <- FV[obj$weights!=0]
    obs <- obs[obj$weights!=0]
   yVal <- obj$y[obj$weights!=0]
    out <- data.frame(obs = obs, y = yVal, fv=FV)
    return(out)
} 
################################################################################
################################################################################
# the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
################################################################################
################################################################################
    median <-  quantile_gamlss(obj, quantile, newdata)
         d <- gamlss_prep_data(obj, median) 
#color <- obs <-  hat <- 
         y <- NULL
      corr <- with(d,cor(y,fv)) 
 txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
              else title    
        pp <- try(obj$call$formula[[2]], silent=TRUE)
txt_ylabel <-  if (any(class(pp)%in%"try-error")) txt_ylab = "response" 
               else  paste(obj$call$formula[[2]])
txt_xlabel <-  paste(quantile, "quantile", sep=" ")
        gg <- ggplot(d, aes(x = fv, y = y)) + 
              geom_point(shape = point.shape, colour = point.col) + 
              geom_smooth(method="gam",col=line.col)+
              xlab(txt_xlabel) + # working  with facet_wrap 
              ylab(txt_ylabel) + # working  with facet_wrap 
              ggtitle(txt.title)   # working  with facet_wrap 
suppressWarnings(return(gg))
}
# resp_quantile(m6)+facet_wrap(~ cut_number(rent$A, 6))
################################################################################
################################################################################
################################################################################
################################################################################
quantile_gamlss <- function (obj,
                             quantile = 0.5,
                             newdata) 
{
################################################################################
################################################################################
      pdf <- obj$family[1]
    binom <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
     qfun <- paste("q", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
if (binom) {bd <- obj$bd ; Y <- obj$y}
       pp <-  if (missing(newdata)) predictAll(obj,output="matrix")
               else  predictAll(obj, newdata=newdata, output="matrix")
   median <- switch(lpar, 
                   eval(call(qfun, p= quantile, mu=pp[,"mu"])),       # 1
                   eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
                   eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
                   eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"])))
median
}
#resp_fitted(m6, "sigma")+facet_wrap(~ cut_number(rent$Fl, 6))
################################################################################
################################################################################
################################################################################
################################################################################


