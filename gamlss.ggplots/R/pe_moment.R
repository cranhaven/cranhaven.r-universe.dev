# create 08-6-21
################################################################################
################################################################################
################################################################################
################################################################################
# TO DO
# i)   function output 
# ii)  check for existance of first two moments
# iii) check binomal data 
################################################################################
################################################################################
################################################################################
################################################################################
pe_moment <- function   (obj = NULL, 
                        term = NULL, 
                      moment = c("mean", "variance", "sd"),
                        data = NULL, 
                    n.points = 100, 
                         how = c("median", "last"), 
                    scenario = list(), 
                         col = "darkblue",
                        size = 1.3,
                    factor.size = 15,
                        plot = TRUE,
                        title) 
{
if (is.null(obj) || !class(obj)[1] == "gamlss") 
    stop("Supply a standard GAMLSS model as first argument")
       how <- match.arg(how)
    moment <-  match.arg(moment)
   moments <- x <-  y <- sd <- NULL
if (any(grepl("data", names(obj$call)))) 
  {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit")) 
                   eval(parse(text = as.character(obj$call["data"])))
              else get(as.character(obj$call["data"]))
  }
else if (is.null(data)) 
    stop("The data argument is needed in obj")
   v.names <- names(DaTa)
       pos <- which(v.names==term)
if (pos<1)stop("The model term is not set")
if (is.factor(DaTa[,pos])) 
   {
         xvar <- levels(DaTa[,pos])
     n.points <- nlevels(DaTa[,pos])
 it.is.factor <- TRUE
   } else
   {
        xvar <-  seq(from = min(DaTa[,pos]), to=max(DaTa[,pos]), length.out=n.points)
it.is.factor <- FALSE
   }                 
         mat <- matrix(0, nrow = dim(DaTa)[1] + n.points, ncol = dim(DaTa)[2])
    dat.temp <- as.data.frame(mat)
names(dat.temp) <- v.names 
if (pos < 1)  stop("supply a term")
#  xvar <- seq(from = min(DaTa[, pos]), to = max(DaTa[, pos]), 
#              length.out = n.points)
  for (i in 1:dim(dat.temp)[2]) 
    {
    if (pos == i) {
      dat.temp[, i] <- c(DaTa[, i], xvar)
    }
    else 
    {
      ma <- scenario[[v.names[i]]]
      if (is.null(ma)) 
        {
        if (how == "median") {
          ma <- if (is.factor(DaTa[, i])) 
            levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else if (is.character(DaTa[, 3])&&(!is.factor(DaTa[, i]))) 
                0
          else median(DaTa[, i])
        }
        if (how == "last") {
          ma <- if (is.factor(DaTa[, i])) 
            levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else tail(DaTa[, i], 1)
        }
       }
      dat.temp[, i] <- c(DaTa[, i], rep(ma, n.points))
     }
    }
      pdf <- obj$family[1]
   binom  <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
     dfun <- paste("d", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
 if (binom) {bd <- obj$bd ; Y <- obj$y}
       pp <-  predictAll(obj, newdata = tail(dat.temp, n.points), output = "matrix")
if (is.null(as.gamlss.family(pdf)$mean)) stop("the moments are not defined currenly in the fitted distribution")
Mean <- switch(lpar, 
     eval(parse(text=pdf))()$mean(mu=pp[,"mu"]),       # 1
     eval(parse(text=pdf))()$mean(mu=pp[,"mu"], sigma=pp[,"sigma"]), # 2
     eval(parse(text=pdf))()$mean(mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"]),  # 3
     eval(parse(text=pdf))()$mean(mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"]))  # 4                   
if (!it.is.factor) meanFun  <- splinefun(xvar, Mean)
Variance <- switch(lpar, 
      eval(parse(text=pdf))()$variance(mu=pp[,"mu"]),       # 1
      eval(parse(text=pdf))()$variance(mu=pp[,"mu"], sigma=pp[,"sigma"]), # 2
      eval(parse(text=pdf))()$variance(mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"]),  # 3
      eval(parse(text=pdf))()$variance(mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"]))  # 4    
if (!it.is.factor) varianceFun  <- splinefun(xvar, Variance)
txt.title <- if (missing(title))  
    paste("Partial effect of",term, "on the", moment, "for model",deparse(substitute(obj)))
   else title
  da <- data.frame(mean=Mean, sd =sqrt(Variance), x=xvar)
if (plot)
{  
  if (it.is.factor)
{
  if (moment=="mean")
  {
yaxislabel <- paste0("PE_E(",term,")")
    pp <-  ggplot2::ggplot(data=da, ggplot2::aes(x=x, y=mean))+
      ggplot2::geom_point(color=col, size=factor.size, shape="-")+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  }
  if (moment=="variance")
  {
    yaxislabel <- paste0("PE_V(",term,")")    
    pp <-  ggplot2::ggplot(data=da, aes(x=x, y=sd^2))+
      ggplot2::geom_point(color=col, size=factor.size, shape="-")+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  } 
  if (moment=="sd")
  {
    yaxislabel <- paste0("PE_sd(",term,")")
    pp <-  ggplot2::ggplot(data=da, ggplot2::aes(x=x, y=sd))+
      ggplot2::geom_point(color=col, size=factor.size, shape="-")+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  } 
 } else 
 {
  if (moment=="mean")
  {
    yaxislabel <- paste0("PE_E(",term,")")
    pp <- ggplot2::ggplot(data=da)+
      ggplot2::geom_line( ggplot2::aes(x=x, y=mean), color=col, size=size)+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  }
  if (moment=="variance")
  {
    yaxislabel <- paste0("PE_V(",term,")")    
    pp <- ggplot2::ggplot(data=da)+
      ggplot2::geom_line( ggplot2::aes(x=x, y=sd^2), color=col, size=size)+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  } 
  if (moment=="sd")
  {
    yaxislabel <- paste0("PE_sd(",term,")")
    pp <- ggplot2::ggplot(data=da)+
      ggplot2::geom_line( ggplot2::aes(x=x, y=sd), color=col, size=size)+
      ggplot2::ylab(yaxislabel)+ 
      ggplot2::xlab(term)+ 
      ggplot2::ggtitle(txt.title)
  } 
}          
  return(pp)
} else
{
  if ((it.is.factor)) stop("there is no function saved for factors")
  else invisible(list(mean=meanFun, variance=varianceFun))
}  
}
################################################################################
################################################################################
################################################################################
################################################################################
