# functions for centile plots in ggplots 
# last change on may 2021
# there are two functions 
# fitted_centile_1()
# which use sort data 
# centiles x y 
################################################################################
################################################################################
################################################################################
################################################################################
fitted_centiles <-function (obj, 
                     xvar, 
                     cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4),
                   points = TRUE,
                point.col = "gray",
               point.size = 1,
                line.size = .8, 
                 line.col = hcl.colors(lc, palette="Dark 2"),
                line.type = rep(1, length(cent)),
                     xlab = NULL,
                     ylab = NULL,
                    title, ...)        
{
if (!is.gamlss(obj))  stop(paste("This is not an gamlss object", "\n", ""))
if (missing(xvar))
{
  xvarCh <-  all.vars(obj$call$formula)[[2]]
  if (any(grepl("data", names(obj$call))))
    {
    DaTa <- eval(obj$call[["data"]]) #get(as.character(obj$call["data"])) get(as.character(obj$call["data"]))
    xvar <- get(xvarCh, envir=as.environment(DaTa))
    }
} else
{
  xvarCh <-   deparse(substitute(xvar))
    DaTa <- eval(obj$call[["data"]])#get(as.character(obj$call["data"]))
}
  x <- y <-   NULL
   xvar <- try(xvar, silent = TRUE)    # get the vector
if  (any(class(xvar)%in%"try-error"))# if vector in DaTa not in the global Env
{ # will fail therefore get it from DaTa
   xvar <- get(xvarCh, envir=as.environment(DaTa))
}
       fname <- obj$family[1]
        qfun <- paste("q",fname,sep="")
   txt.title <- if (missing(title)) 
                paste("Centile curves using", fname, sep = " ")
                else title
       oxvar <- xvar[order(xvar)]
       oyvar <- obj$y[order(xvar)]  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
  {
   oyvar <-  obj$y[,1][order(xvar)] 
   ylim  <-  range(obj$y[,1])
    yleg <- max(obj$y[,1])
  }
     col <- 3 # set this to 1 if you do not want colour 
    lpar <- length(obj$parameters)
      ii <- 0
     per <- rep(0,length(cent))
   centM <- matrix(0, ncol=length(cent), nrow= dim(DaTa)[1])
colnames(centM) <- cent 
for(var in cent) 
{ 
    if(lpar==1) 
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)]) 
    }
    else if(lpar==2)
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)]) 
    }
    else if(lpar==3)
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)],
                     nu=fitted(obj,"nu")[order(xvar)])
    }
    else 
    {
      newcall <-call(qfun,var/100, mu=fitted(obj,"mu")[order(xvar)],
                     sigma=fitted(obj,"sigma")[order(xvar)],
                     nu=fitted(obj,"nu")[order(xvar)],
                     tau=fitted(obj,"tau")[order(xvar)]) 
    }   
            ii <- ii+1
    centM[,ii] <- eval(newcall)
} 
  yvarCh <- paste(obj$call$formula[[2]])
#       N <- length(xvar) 
      lc <- length(cent)
   DataC <-  data.frame(c = centM, 
                       x = oxvar, 
                       y = oyvar)
  Cnames <- colnames(DataC)
     ggc <- ggplot(DataC)
if (points) 
    {
    ggc <- ggc + geom_point(aes(x=x, y=y), colour=point.col, size=point.size)
     }
for (i in 1:lc)
    {
  #fcol <-rep(line.col[i], N)
      ggc <- ggc + geom_line(aes_string(x="x", y=Cnames[i]), linetype=line.type[i],
                             color=line.col[i], linewidth=line.size)
} 
  xvarCh <-   if (is.null(xlab)) xvarCh else xlab  
  yvarCh <-   if (is.null(ylab)) yvarCh else ylab  
  ggc <- ggc+ ggtitle( txt.title)+ylab(yvarCh)+xlab(xvarCh)
  ggc
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
fitted_centiles_legend <-function (obj, 
                              xvar, 
                              cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4),
                            points = TRUE,
                         point.col = "gray",
                        point.size = 1,
                         line.size = .8, 
                          line.col = hcl.colors(ncent, palette="Dark 2"),
                         line.type = rep(1, length(cent)),
                       show.legend = TRUE,
                         save.data = FALSE,
                             title,
                              xlab = NULL,
                              ylab = NULL,
                       ...)        
{
if (!is.gamlss(obj))  stop(paste("This is not an gamlss object", "\n", ""))
     ncent <- length(cent)
if (missing(xvar))
  { 
    xvarCh <-  all.vars(obj$call$formula)[[2]]
  if (any(grepl("data", names(obj$call))))
    {
      DaTa <- eval(obj$call[["data"]]) #get(as.character(obj$call["data"])) get(as.character(obj$call["data"])) 
      xvar <- get(xvarCh, envir=as.environment(DaTa))
    } 
  } else
  {
   xvarCh <- deparse(substitute(xvar))
     xvar <- try(xvar, silent = TRUE) 
  }  
     xvar <- try(xvar, silent = TRUE)    # get the vector
if  (any(class(xvar)%in%"try-error"))# if vector in DaTa not in the global Env 
  { # will fail therefore get it from DaTa
     DaTa <- eval(obj$call[["data"]])#get(as.character(obj$call["data"])) 
     x <- y <- NULL
     xvar <- get(xvarCh, envir=as.environment(DaTa))
  }
# end of new 
   fname <- obj$family[1]
    qfun <- paste("q",fname,sep="")
txt.title <- if (missing(title)) 
             paste("Centile curves using", fname, sep = " ")
            else title
   oxvar <- xvar[order(xvar)]
   oyvar <- obj$y[order(xvar)]  
if (is.matrix(obj$y)) # Monday, March 26, 2007 at 14:12
  {
   oyvar <- obj$y[,1][order(xvar)] 
   ylim  <- range(obj$y[,1])
    yleg <- max(obj$y[,1])
}
    lpar <- length(obj$parameters)
      ii <- 0
     per <- rep(0,length(cent))
# per <- cent/100 
   centM <- matrix(0, ncol=length(cent), nrow= length(obj$y))
colnames(centM) <- cent 
for(var in cent) 
  { 
  if(lpar==1) 
    {
newcall <-call(qfun,var/100,
                     mu=fitted(obj,"mu")[order(xvar)]) 
    }
  else if(lpar==2)
    {
newcall <-call(qfun,var/100,
              mu=fitted(obj,"mu")[order(xvar)],
              sigma=fitted(obj,"sigma")[order(xvar)]) 
    }
  else if(lpar==3)
    {
newcall <- call(qfun,var/100,
              mu = fitted(obj,"mu")[order(xvar)],
              sigma = fitted(obj,"sigma")[order(xvar)],
              nu = fitted(obj,"nu")[order(xvar)])
    }
  else 
    {
newcall <- call(qfun,var/100,
              mu = fitted(obj,"mu")[order(xvar)],
              sigma = fitted(obj,"sigma")[order(xvar)],
              nu = fitted(obj,"nu")[order(xvar)],
              tau = fitted(obj,"tau")[order(xvar)]) 
    }   
    ii <- ii+1
centM[,ii] <- eval(newcall)
  } 
yvarCh <- paste(obj$call$formula[[2]])
     N <- length(xvar) 
    lc <- length(cent)
 DataM <- data.frame(c = as.vector(centM),
                      x = rep(oxvar, lc),
                      y = rep(oyvar, lc),
               centiles = gl(length(cent), N, labels = as.character(cent)),
                  color = gl(length(cent), N, labels = line.col)
              # typeline = factor(rep(line.type, each = length(oxvar))) 
                        )
  if (save.data) return(DataM)  
  gg <- ggplot2::ggplot(DataM, ggplot2::aes(x=x, y=c, col=centiles, group=centiles)) 
  #linetype = typeline
  if (points) 
  {
    gg <-  gg +
      ggplot2::geom_point(ggplot2::aes(x=x, y=y), colour=point.col, size=point.size)+
      ggplot2::geom_line(linewidth=line.size,  
                    show.legend = show.legend)+#, color=DataM$color
          ggtitle( txt.title)
   } else
  {
    gg <- gg +
      ggplot2::geom_line(linewidth=line.size, linetype=line.type,
                    show.legend = show.legend)+
      ggplot2::ggtitle( txt.title)
  }
  xvarCh <-   if (is.null(xlab)) xvarCh else xlab  
  yvarCh <-   if (is.null(ylab)) yvarCh else ylab  
  gg <- gg+ ggplot2::ylab(yvarCh)+ggplot2::xlab(xvarCh)
  gg
}
################################################################################
################################################################################
################################################################################
################################################################################