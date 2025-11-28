########################################################################
########################################################################
#   first function
########################################################################
########################################################################
########################################################################
########################################################################   
resp_mu <- function (obj, title, 
                             line.col = "darkred", 
                            point.col = "steelblue4",
                          point.shape = 20
                         ) 
{
########################################################################
# local functions 
gamlss_prep_data <- function (obj) 
  {
             FV <- fitted(obj)
            obs <- obs <- seq_len(length(FV))
             FV <- FV[obj$weights!=0]
            obs <- obs[obj$weights!=0]
           yVal <- obj$y[obj$weights!=0]
            out <- data.frame(obs = obs, y = yVal, fv=FV)
#names(out)[[2]] <- paste(obj$call$formula[[2]])
  return(out)
  } 
#####################################################################
#####################################################################
# the main function starts here  
  if (missing(obj))  stop("A GAMLSS fitted object should be used")
  if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
          # N <- obj$N
           d <- gamlss_prep_data(obj) 
      #color <- obs <-  hat <- 
      y <- NULL
#           f <- d[d$color == "outlier", c("obs", "hat")]
# colnames(f) <- c("observation", "quan_resid")
# try colors() for different colors
#facet_wrap(~ cut_number(rent$A, 6))
     corr <- with(d,cor(y,fv)) 
txt.title <- if (missing(title))  paste(paste0("r = ",sprintf("%.3f",corr )))
               else title    
pp <- try(obj$call$formula[[2]], silent=TRUE)
txt_ylabel <-  if (any(class(pp)%in%"try-error")) txt_ylab = "response" 
              else  paste(obj$call$formula[[2]])
       gg <- ggplot(d, aes(x = fv, y = y)) + 
             geom_point(shape = point.shape, colour = point.col) + 
             geom_line(aes(x = fv, y = fv), col=line.col)+
             xlab("mu fitted values") + # working  with facet_wrap 
             ylab(txt_ylabel) + # working  with facet_wrap 
             ggtitle(txt.title)   # working  with facet_wrap 

    suppressWarnings(return(gg))

}
#resid_plot(r1, no.lines=T)+facet_wrap(~ cut_number(rent$A, 6))
# #########################################################################
# #########################################################################
# #########################################################################
# #########################################################################