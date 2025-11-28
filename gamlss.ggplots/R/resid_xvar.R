########################################################################
########################################################################
#   first function
########################################################################
########################################################################
########################################################################
########################################################################   
resid_xvar <- function (obj, xvar, plot = TRUE, value=2, title, annotate=TRUE) 
{
# Note that I am taking the 
# obj$resid rather resid(obj) so I can preserve the no of the observations
########################################################################
# local functions 
gamlss_prep_data <- function (obj, xvar, value=2) 
{
    sdres <- residuals(obj)
       fv <- obj$mu.fv
#sdres_out <- abs(sdres) > value
      obs <- seq_len(length(sdres))
#  outlier <- sdres[sdres_out]
      obs <- obs[obj$weights!=0]
    sdres <- sdres[obj$weights!=0]
       fv <- fv[obj$weights!=0]
      out <- data.frame(obs = obs, sdres = sdres, fv)
out$color <- ifelse(((out$sdres >= value) | (out$sdres <= -value)), 
                        c("outlier"), c("normal"))
out$fct_color <- ordered(factor(out$color), levels = c("normal", "outlier"))
  out$txt <- ifelse(out$color == "outlier", out$obs, NA)
if (!missing(xvar)) 
{
  #if (is(xvar,"formula"))
  #{
   # Xvar <- get_all_vars(xvar, data=DaTa)
    Xvar <- if (DataExist==TRUE)  DaTa[,Xchar] else get(Xchar)
   # if (is.factor(Xvar)) stop("this function is only for continuous xvars")
    Xvar <- subset(Xvar,obj$weights!=0)
    out  <- cbind(out, Xvar)
 # } else stop("xvar should be a formula") 
}  
  return(out)
  } 
#####################################################################
#####################################################################
  # the main function starts here  
if (missing(obj))  stop("A GAMLSS fitted object or the argument resid should be used")
if (!missing(obj)&&!is.gamlss(obj)) stop("the model is not a gamlss model")
# we need to get the xvar here 
DataExist <- FALSE
if (!missing(obj)&&any(grepl("data", names(obj$call))))
 {
       DaTa <-  eval(obj$call[["data"]]) 
  DataExist <- TRUE
 }
     Xchar <- deparse(substitute(xvar))
         d <- gamlss_prep_data(obj,  xvar = xvar, value = value) 
 txt.title <- if (missing(title))  paste("Residuals against", deparse(substitute(xvar)), "of model",deparse(substitute(obj))) else title
      Xvar <- sdres <- txt <- NULL
         f <- d[d$color == "outlier", c("obs", "sdres")]
colnames(f) <- c("observation", "quan_resid")
if (is.factor(d$Xvar))
{
  p <- ggplot(data=d, aes(x = Xvar, y = sdres, col=Xvar, fill=Xvar))+
    geom_boxplot()+
    geom_jitter(color="black", size=0.3, alpha=0.9)+
    theme(legend.position="right", plot.title=element_text(size=10))+
    #  coord_flip()  +
    xlab(Xchar) +
    ylab("Quantile Residuals")+
    ggtitle(txt.title) 
} else
{
  p <- ggplot(d, aes(x = Xvar, y = sdres, label = txt, ymin = 0, ymax = sdres)) +
    geom_linerange(colour =  "steelblue4" ) +
    geom_point(shape = 1, colour = "steelblue4"  ) +
    geom_hline(yintercept = 0, colour = "gray") +
    geom_hline(yintercept = c(value, -value), colour = "red") +
    xlab(Xchar) +
    ylab("Quantile Residuals") +
    ggtitle(txt.title) +
    geom_text(hjust = -0.2, nudge_x = 0.15, size = 3, family = "serif",
              fontface = "italic", colour = "darkred", na.rm = TRUE) +
    if(annotate) annotate("text", x = Inf, y = Inf, hjust = 1.5, vjust = value,
                          family = "serif", fontface = "italic", colour = "darkred",
                          label = paste0("Threshold: abs(", value, ")"))
}
 
  if (plot) {
    suppressWarnings(return(p))
  } else
  {
    return(list(plot = p, outliers = f, threshold = value))
  }
}
################################################