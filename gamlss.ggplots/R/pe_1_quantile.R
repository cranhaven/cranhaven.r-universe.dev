# create 08-6-21
################################################################################
################################################################################
################################################################################
################################################################################
# TO DO
# i)   what action we should have with factors?
# ii)  what happents if linear terms? the first derivative look funny
# iii) check for different values of the other variables what happening  
# iv)  binomial or count data? check
# v)   different colour scheme?
################################################################################
################################################################################
################################################################################
################################################################################
pe_1_quantile <- function (obj = NULL, 
                        term = NULL, 
                    quantile = c(0.95, 0.50, 0.05),
                        data = NULL, 
                    n.points = 100, 
                         how = c("median", "last", "fixed"), 
                    scenario = list(), 
                         col = "darkblue",
                   linewidth = 1.3,
                      legend = TRUE,
                        ylim = NULL,
                        ylab = NULL,
                        xlab = NULL,
                    title, ...) 
{
if (is.null(obj) || !class(obj)[1] == "gamlss") stop("Supply a standard GAMLSS model in obj")
       how <- match.arg(how)
 quantiles <- x <-  y <- NULL
if (any(grepl("data", names(obj$call)))) {
      DaTa <- if (startsWith(as.character(obj$call["data"]), "na.omit")) 
                   eval(parse(text = as.character(obj$call["data"])))
              else get(as.character(obj$call["data"]))
  }
else if (is.null(data)) 
    stop("The data argument is needed in obj")
   v.names <- names(DaTa)
       pos <- which(v.names==term)
if (pos<1) stop("supply a  term")
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
#  xvar <- seq(from = min(DaTa[, pos]), to = max(DaTa[, pos]), 
#              length.out = n.points)
  for (i in 1:dim(dat.temp)[2]) {
    if (pos == i) 
      {
      dat.temp[, i] <- c(DaTa[, i], xvar)
       }
    else 
       {
      ma <- scenario[[v.names[i]]]
      if (is.null(ma)) 
        {
          if (how == "median") 
            {
             if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
             ma <- if (is.factor(DaTa[, i])) levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else median(DaTa[, i])
        }
        if (how == "last") {
          if (is.character(DaTa[,i])) DaTa[,i] <- as.factor(DaTa[,i])
          ma <- if (is.factor(DaTa[, i])) 
            levels(DaTa[, i])[which.max(table(DaTa[, i]))]
          else tail(DaTa[, i], 1)
        }
      }
      dat.temp[, i] <- c(DaTa[, i], rep(ma, n.points))
    }
  } # end going thought the variables
      pdf <- obj$family[1]
    binom <- pdf%in%gamlss::.gamlss.bi.list # whether binomial
     qfun <- paste("q", obj$family[[1]],sep="")
     lpar <- eval(parse(text=pdf))()$nopar
  if (binom) {bd <- obj$bd ; Y <- obj$y}
       pp <-  predictAll(obj, newdata = tail(dat.temp, n.points), output="matrix")
       qq <- list()
      lqq <- length(quantile) 
if (lqq==1)
{
   qq[[1]] <- switch(lpar, 
                     eval(call(qfun, p= quantile, mu=pp[,"mu"])),       # 1
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
                     eval(call(qfun, p= quantile, mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"])))
 } else
 {
  for (i in 1:lqq)
  {
   qq[[i]] <- switch(lpar, 
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"])),       # 1
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"])),        # 2
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"])),  # 3                   
                eval(call(qfun, p= quantile[i], mu=pp[,"mu"], sigma=pp[,"sigma"],  nu=pp[,"nu"], tau=pp[,"tau"])))
  }
 }  
    yaxislabel <- paste0("PE_quan(", term, ")")
if (lqq==1)
{
  txt.title <- if (missing(title))  
                  paste("Partial effect of",term, "quantile", quantile, "for model",deparse(substitute(obj)))
               else title
  da <- data.frame(y=qq[[1]], x=xvar)
  pp <- ggplot2::ggplot(data=da)+
    ggplot2::geom_line( ggplot2::aes(x=x, y=y), color=col, linewidth=linewidth)+
    ggplot2::ylab(yaxislabel)+ 
    ggplot2::xlab(term)+ 
    ggplot2::ggtitle(txt.title)
  return(pp)
} else
{
  txt.title <- if (missing(title))  
    paste("Partial effect quantiles", term, "for model", deparse(substitute(obj)))
  else title  
 #da1= subset(da, da$quantiles==.5)
#  ggplot(data=da1)+geom_line(aes(x=x,y=y))
  da <- data.frame(y=unlist(qq), x=rep(xvar,lqq), quantiles=gl(lqq,length(qq[[1]]), labels = quantile)) 
        #ggplot(DataM, aes(x=x, y=c, col=centiles))

if (it.is.factor)
{
  pp <-  ggplot2::ggplot(data=da, ggplot2::aes(x=x, y=y, group=factor(quantiles), colour=quantiles))+
    ggplot2::geom_line(linewidth=linewidth)+
    ggplot2::geom_point( size=linewidth+2)+
    ggplot2::ylab(yaxislabel) + xlab(term)+ 
    ggplot2::ggtitle(txt.title)
if (!is.null(ylim)) pp <- pp + ggplot2::ylim(ylim)   
if (!is.null(ylab)) pp <- pp + ggplot2::ylab(ylab) 
if (!is.null(xlab)) pp <- pp + ggplot2::xlab(xlab) 
if (legend=="FALSE") pp <- pp+ ggplot2::theme(legend.position = "none")
} else 
{
  pp <- ggplot2::ggplot(data=da, ggplot2::aes(x=x, y=y, col=quantiles))+
    ggplot2::geom_line(linewidth=linewidth)+
    ggplot2::ylab(yaxislabel)+
    ggplot2::xlab(term)+ 
    ggplot2::ggtitle(txt.title)
if (!is.null(ylim)) pp <- pp + ggplot2::ylim(ylim)
if (!is.null(ylab)) pp <- pp + ggplot2::ylab(ylab) 
if (!is.null(xlab)) pp <- pp + ggplot2::xlab(xlab) 
if (legend=="FALSE") pp <- pp + 
    ggplot2::theme(legend.position = "none")
}          
  return(pp)
}
}
################################################################################
################################################################################
################################################################################
################################################################################