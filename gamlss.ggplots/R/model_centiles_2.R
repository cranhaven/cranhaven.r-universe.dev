# function model_centiles
################################################################################
################################################################################
################################################################################
################################################################################
model_centiles <- function(obj,...,
                           cent = c(97, 90, 75, 50, 25, 10, 3),
                           xvar ,
                           xlab = "age",
                         points = TRUE,
                      point.col = gray(.8),
                     point.size = 0.05,
                      line.size = .7, 
                       line.col = hcl.colors(ncent, palette="Dark 2"),
                           ncol = 2,
                           nrow = ceiling(nnames/ncol), 
                         in.one = FALSE,
                           title)
{
################################################################################
# local function
gamlss_prep_data <- function (obj, xvar, ... ) 
  {
    out <- fitted_centiles_legend(obj, cent=cent, save.data=T) 
    out <- data.frame(out, Y= names[1])
  #out$x <- out$x^x_trans[1]
if (length(list(...)) > 0) 
      {
        i  <- 1    
      for (model in list(...)) 
      {
         i <- i+1
        DF <- fitted_centiles_legend(model, cent=cent, save.data=T) 
        DF <- data.frame(DF, Y= names[i])
      #DF$x <- DF$x^x_trans[i]
          out <- rbind(out, DF)
      }
    }    
    return(out)    
  }    
################################################################################
################################################################################
      Y <- x <- y <-  rqres  <- model <-  NULL   
   names <- as.character(match.call()[-1])[1:(length(list(...))+1)]
   ncent <- length(cent)
  nnames <- length(names)
#x_trans <- if (is.null(x_trans)) rep(1, length(names)) else x_trans
#  if (length(x_trans) != length(names)) stop("the x-transformations should be equal to the number ofmodel")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
#if (length(names)<=1) stop("you need more than two models")
       d <- gamlss_prep_data(obj, xvar=xvar, ...)
if (missing(title))  show.title = FALSE 
               else {txt.title <- title ; show.title=TRUE}
if(in.one==FALSE)
{
  gg <- ggplot(data = d, mapping=aes(x, c, colour = centiles), 
               line.col = line.col)+
    {if(points) geom_point(aes(x=x, y=y), colour = point.col, size = point.size)} +
    geom_path(size = line.size)  + ylab("centiles")+ xlab(xlab)+
    facet_wrap(~Y, scales = 'free_y', ncol=ncol, nrow=nrow)
} else
{
  gg <- ggplot2::ggplot(data = d, mapping=ggplot2::aes(x, c, colour = centiles), 
               line.col = line.col)+
   {if(points) ggplot2::geom_point(aes(x=x, y=y), colour = point.col, 
                                   size = point.size)} +
    ggplot2::geom_line(ggplot2::aes(linetype = Y))+
    ggplot2::ylab("centiles")+ 
    ggplot2::xlab(xlab)
}     
      if (show.title) gg <- gg+ ggplot2::ggtitle( txt.title)
    return(gg)
}
################################################################################ 
################################################################################
################################################################################
################################################################################ 
