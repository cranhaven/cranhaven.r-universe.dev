################################################################################
################################################################################
################################################################################
################################################################################
# maximal correlation coefficient
# functions 
# i)   ACE3
# ii)  ACE
# iii) print.ACE                      
# iv)  plot.ACE
# v)   mcor
# vi)  data_mcor:   m-correlations for data using foreach 
# vii) data_mcor_old:   m-correlations for data old
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# this function plots the iterations and it design for seeing what is 
#function I
# going in the transformation  
 ACE3 <- function(x, y, weights, 
                  data = NULL, 
              con_crit = 0.001, 
            fit.method = c("loess", "P-splines"),
                  nseg = 10,
                max.df = 6,
                  ...) 
 {
   tx <- ty <- NULL   
   fit.method <-  match.arg(fit.method)
if (missing(y)|missing(x)) stop("one of two variables is missing")
   xname <- deparse(substitute(x))
   yname <- deparse(substitute(y)) 
   wname <- if (missing(weights)) "w"
            else deparse(substitute(y))
 if (!is.null(data))
 {
   y <- data[,yname]
   x <- data[,xname]
   w <- if (!missing(weights)) data[,wname]
 }   
if (missing(weights))  w <- rep(1, length(x))
# standardise
        sy <- (y-mean(y))/sd(y)# robustify
        sx <- (x-mean(x))/sd(x)
  #     wh <- c(which(abs(sy)>robust), which(abs(sx)>robust))
  #     lwh <- length(wh)
  #       y <- if(lwh>1) y[-wh] else y
  #       x <- if(lwh>1) x[-wh] else x
         y <- y-mean(y)
         x <- x-mean(x)
   #      w <- if(lwh>1) w[-wh] else w
        r1 <- 1
        r0 <- ro <- cor(y,x)
if (any(r0>0.99)){ 
       out <- list(y=y, x=x, ty=NULL, tx=NULL, rsq=r0^2, cor=r0, dist=0) 
class(out) <- "PB_ACE"
invisible(out)
    stop("ACE failed: very high linear correlation in the original variables")
  }
e2 <- sum((y-x)^2)        
       di <- e2
       #di <- r1-r0
theta_1_y <- y/sqrt(sum(y^2))
switch(fit.method,
       "P-splines"={
         while (abs(di)>con_crit)
         {
# regress theta(y) against x     
          ma <- fit_PB(x,theta_1_y, weights=w, plot=TRUE, nseg=nseg, max.df = 6, ...)
          Sys.sleep(1)
       #   cat(ma$df, "\n")
      phi_1_x <- as.vector(ma$fv)
        phi_x <- phi_1_x/sqrt(sum(phi_1_x^2))
# regress phi(x) against Y  
           mb <-  fit_PB(y,phi_x,weights=w, plot=TRUE,  nseg=nseg,  max.df = 6, ...) 
           Sys.sleep(1)
         #  cat(mb$df, "\n")
    theta_1_y <- as.vector(mb$fv)     
      theta_y <- theta_1_y/sqrt(sum(theta_1_y^2))
           r1 <- cor(theta_y, phi_x)
          e21 <- sum((theta_y-phi_x)^2) 
           di <- abs(e21-e2)
          # di <- r1-r0
     #      cat(di, "\n")
           e2 <- e21
         } 
       },
        "loess"= {
# regress theta(y) against x     
     phi_1_x <- fitted(loess(theta_1_y~x, weights=w, ...))   
     plot(theta_1_y~x); points(phi_1_x~x, col="red")
     Sys.sleep(1)
       phi_x <- phi_1_x#/sqrt(sum(phi_1_x^2))
# regress phi(x) against Y      
   theta_1_y <- fitted(loess(phi_x~y, weights=w, ...)) 
   plot(phi_x~y); points(theta_1_y~y, col="red")
   Sys.sleep(1)
     theta_y <- theta_1_y/sqrt(sum(theta_1_y^2))
          r1 <- cor(theta_y, phi_x)
         e21 <- sum((theta_y-phi_x)^2) 
          di <- abs(e21-e2)
          e2 <- e21          
        }
       )
     tyname <- paste0("t(", yname,")")
     txname <- paste0("t(", xname,")")
       out <- list(y = y, x = x, 
                   ty=theta_y, tx=phi_x, rsq=r1^2, cor=r1, 
                   r=ro, diff=r1-ro, fit.method = fit.method) 
    names(out) <- c(yname, xname, tyname, txname, "rsq", "cor", "r", "diff", "method")   
class(out) <- "ACE" 
 invisible(out)
 }
################################################################################
################################################################################
################################################################################
################################################################################
# function II
# This is the main ace function
# it works only for one x and one y
ACE <- function(x, y, weights, 
                  data = NULL, 
              con_crit = 0.001, 
            fit.method = c("loess", "P-splines"),
                  nseg = 10,
                max.df = 6,
                  ...) 
 {
    fit.method <-  match.arg(fit.method)
if (missing(y)|missing(x)) stop("one of two variables is missing")
      xname <- deparse(substitute(x))
      yname <- deparse(substitute(y)) 
      wname <- if (missing(weights)) "w"
               else deparse(substitute(y))
if (!is.null(data))
   {
     y <- data[,yname]
     x <- data[,xname]
     w <- if (!missing(weights)) data[,wname]
   }   
if (missing(weights))  w <- rep(1, length(x))
# standardise
#browser()
    sy <- (y-mean(y))/sd(y)# robustify
    sx <- (x-mean(x))/sd(x)
   # wh <- c(which(abs(sy)>robust), which(abs(sx)>robust))
   #lwh <- length(wh)
#y <- if(lwh>1) y[-wh] else y
   #  x <- if(lwh>1) x[-wh] else x
 # browser()
     y <- y-mean(y)
     x <- x-mean(x)
    # w <- if(lwh>1) w[-wh] else w
    r1 <- 1
    r0 <- ro <- cor(y,x)
if (any(abs(r0>0.99))){ 
   out <- list(y=y, x=x, ty=NULL, tx=NULL, rsq=r0^2, cor=r0, dist=0) 
     class(out) <- "ACE"
#     invisible(out)
     stop("ACE failed: very high linear correlation in the original variables")
   }
    e2 <- sum((y-x)^2)        
    di <- e2
#di <- r1-r0
   theta_1_y <- y/sqrt(sum(y^2))
switch(fit.method,
    "P-splines"={
            while (abs(di)>con_crit)
            {
              # regress theta(y) against x     
               ma <- fit_PB(x, theta_1_y, weights = w, plot = FALSE, nseg=nseg,  max.df = 6, ...)
          phi_1_x <- as.vector(ma$fv)
            phi_x <- phi_1_x/sqrt(sum(phi_1_x^2))
              # regress phi(x) against Y  
               mb <- fit_PB(y, phi_x, weights = w, plot = FALSE, nseg=nseg,  max.df = 6, ...) 
        theta_1_y <- as.vector(mb$fv)     
          theta_y <- theta_1_y/sqrt(sum(theta_1_y^2))
               r1 <- cor(theta_y, phi_x)
              e21 <- sum((theta_y-phi_x)^2) 
               di <- abs(e21-e2)
               e2 <- e21
            } 
          },
    "loess"= {
            # regress theta(y) against x     
            phi_1_x <- fitted(loess(theta_1_y~x, weights=w, ...))            
              phi_x <- phi_1_x#/sqrt(sum(phi_1_x^2))
            # regress phi(x) against Y      
          theta_1_y <- fitted(loess(phi_x~y, weights=w, ...)) 
            theta_y <- theta_1_y/sqrt(sum(theta_1_y^2))
                 r1 <- cor(theta_y, phi_x)
                e21 <- sum((theta_y-phi_x)^2) 
                 di <- abs(e21-e2)
                 e2 <- e21          
          }
   )
   tyname <- paste0("t(", yname,")")
   txname <- paste0("t(", xname,")")
   out <- list(y = y, x = x, 
               ty=theta_y, tx=phi_x, rsq=r1^2, cor=r1, 
               r=ro, diff=r1-ro,fit.method =fit.method) 
   names(out) <- c(yname, xname, tyname, txname, "rsq", "cor", "r", "diff", "method")   
   class(out) <- "ACE" 
   invisible(out)
 }
################################################################################
################################################################################
################################################################################
################################################################################
# function III
print.ACE <- function (x, digits = max(3, getOption("digits") - 3), ...) 
 {   
    yname <- names(x)[1]
    xname <- names(x)[2]
  fit.method <-  x$fit.method
   cat("P-spline ACE fit of", yname, "against", xname, "using", fit.method, "\n")
   cat("Estimated maximal correlation:", format(signif(x$cor)), "\n")
 }
################################################################################
################################################################################
################################################################################
################################################################################
# function IV
plot.ACE <- function(x, what = c("transformed", "x", "y", "resid"), 
                       points.col="steelblue4", points.size=1,
                       line.col="darkgray",line.size=1.5,
                       save.data=FALSE, ...)
 {
  tx <- ty <- y <- NULL   
  what <-  match.arg(what)
   yname <- names(x)[1]
   xname <- names(x)[2]
  tyname <- names(x)[3]
  txname <- names(x)[4]
#  if (is.null(x$tx)|is.null(x$tx)) return("no transformation, nothing to plot")
   da <- data.frame(y=x[[yname]], x=x[[xname]], ty=x[[tyname]], tx=x[[txname]])
   m1 <- lm(ty~tx, data=da)
  da$fv <- fitted(m1) 
  da$resid <- resid(m1) 
  switch(what,
         "transformed"={
           pp <- ggplot2::ggplot(data=da, ggplot2::aes(x=tx, y=ty))+
                 ggplot2::geom_point(col=points.col, size=points.size)+
                 ggplot2::geom_line(ggplot2::aes(x=tx, y=fv), col=line.col, 
                                    size=line.size)+
                 ggplot2::ylab(tyname)+ 
                 ggplot2::xlab(txname)
         },
         "x"={
           pp <- ggplot2::ggplot(data=da, ggplot2::aes(x=x, y=tx))+
                 ggplot2::geom_line(col=line.col, size=line.size)+
                 ggplot2::geom_rug(sides="b")+
                 ggplot2::ylab(txname)+ 
                 ggplot2::xlab(xname)
         },
         "y"={
           pp <- ggplot2::ggplot(data=da, aes(x=y, y=ty))+
                 ggplot2::geom_line(col=line.col, size=line.size)+
                 ggplot2::geom_rug(sides="b")+
                 ggplot2::ylab(tyname)+ 
                 ggplot2::xlab(yname)
         }, 
        "resid"={
           pp <- ggplot2::ggplot(data=da, ggplot2::aes(x=y, y=resid))+
                 ggplot2::geom_point(col=points.col, size=points.size)+
                 ggplot2::ylab("residuals")+ 
                 ggplot2::xlab(yname)
         }, 
         )
  da$resid <- resid(m1)
if (save.data){return(da)}
pp
}
################################################################################
################################################################################
################################################################################
################################################################################
# function V
# how to generalised to matrices?
mcor <- function(x,y, data = NULL, 
                fit.method = c("loess", "P-splines"), 
                      nseg = 10,  max.df = 6,...)
{
  fit.method <-  match.arg(fit.method)
  xname <- deparse(substitute(x))
  yname <- deparse(substitute(y)) 
  if (!is.null(data))
  {
    y <- data[,yname]
    x <- data[,xname]
  }     
cor <- ACE(x, y, fit.method = fit.method, nseg = nseg, ...)$cor 
cor
}
################################################################################
################################################################################
################################################################################
################################################################################
# function VI
#  correlation for a data.frame
# it check whether they are factor and only show the
# correlation for continuous variables
# This version uses foreach
data_mcor <- function(data,   
                  digits = 3,
                    plot = TRUE,
                diag.off = TRUE,
           lower.tri.off = FALSE,  
                  method = c("square", "circle"),
              fit.method = c("P-splines", "loess"),
           outline.color = "gray",
                  colors = c("blue", "white", "red"),
            legend.title = "Corr",
                  title,
                 ggtheme = ggplot2::theme_minimal(),
                  tl.cex = 12,
                  tl.col = "black", 
                  tl.srt = 45,
                     lab = TRUE, 
                 lab_col = "black", 
                lab_size = 3,
             circle.size = 20,
                      ...) # c(1,15) maybe will do
{
####################################################################
####################################################################
#require(foreach)
####################################################################
####################################################################
  # local function 
meltit <- function(mat)
  {
     rna <- rownames(mat)
    lrna <- length(rna)
   value <- as.vector(mat)
    Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
    Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
     daf <-  na.omit(data.frame(Var1, Var2, value=value)) 
    daf
  }
#################################################################### 
####################################################################    
# data.frame missing 
 i <- j <- NULL
if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this data frame")
# data obs na's
if (is(data, "list"))  stop("the data is list  the function needs a data.frame") 
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
  if (is(data, "array")) stop("the data is an array the function needs a data.frame")    
  dimD <- dim(data)
if (any(is.na(data)))
  {
      l1 <- dim(data)[1]
    data <- na.omit(data)
      l2 <- dim(data)[1]
  warning(cat(l1-l2, "observations were omitted from the data", "\n"))
  }
#  if is a list or table
if (is.null(dimD)) stop("only one variable in the data") 
if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                             "to detect non-linear correlations", "\n"))   
TT <- lapply(data, unique)
daTa <- subset(data, select=
                   ifelse(sapply(data,is.factor) | sapply(data,is.character) |
                            sapply(data,is.integer)&lapply(TT, length)<10, FALSE, TRUE))
  Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")             
if (Dim[2]==1) stop("only one variable is left after taking out the factors")              
  diffDim  <- dimD[2]-Dim[2]
  if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
  }
   cnames <- names(daTa)
  lcnames <- length(cnames)
       CC <- matrix(0, ncol=lcnames, nrow=lcnames)
#get CC using foreach
       CC <- foreach(i=1:lcnames, .combine='rbind') %dopar% 
         {
            xi <- if(is.null(dim(daTa[,i]))) daTa[,i] else daTa[,i][,1]
            foreach(j=1:lcnames, .combine='c') %do% 
              {
              xj <- if(is.null(dim(daTa[,j]))) daTa[,j] else daTa[,j][,1] 
              if (i<j) CC[i,j]  <- mcor(xi, xj, ...)
               else CC[i,j] <- 0
              }
         }
          CC <- CC+t(CC)  # to get the full matrix (rather than diagonal)
         CC1 <- cor(daTa)
         CC1 <- base::round(x = CC, digits = digits)            
rownames(CC) <- cnames
colnames(CC) <- cnames
    diag(CC) <- 1
          CC <- base::round(x = CC, digits = digits)   
if (diag.off) diag(CC) <- NA
if  (lower.tri.off) CC[lower.tri(CC)] <- NA
if (plot==FALSE) return(CC)
      method <- match.arg(method)
  fit.method <- match.arg(fit.method)
        corr <- meltit(CC)
    lowerLim <- 25-floor((range(corr$value)[2]-range(corr$value)[1])*20)
colnames(corr) <- c("var_1", "var_2", "value")
   txt.title <- if (missing(title))
                paste("Maximal correlations from data:", deparse(substitute(data)))
                else title
corr$abs_corr <- abs(corr$value) * 10
     p <- ggplot2::ggplot(data = corr,
                mapping = ggplot2::aes_string(x = "var_1", y = "var_2", 
                                   fill = "value"))
if (method == "square")
  {
     p <- p + ggplot2::geom_tile(color = outline.color)
  }
  else if (method == "circle") 
    {
     p <- p + geom_point(color = outline.color, shape = 21,
                          ggplot2::aes_string(size = "abs_corr")) +
      # scale_size(range = circle_scale_size_range) +
      scale_size_area(max_size = circle.size) +
      guides(size = "none")
    }
  label <- round(x = CC, digits = digits)
      p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3],
                  mid = colors[2],  midpoint = 0.5, limit = c(0, 1),
                  space = "Lab",
                  name = legend.title)+ggplot2::ggtitle(txt.title)
if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = tl.srt,
                    vjust = 1, size = tl.cex, colour=tl.col, hjust = 1),
                    axis.text.y = ggplot2::element_text(size = tl.cex, 
                                                        colour=tl.col)) +
            ggplot2::coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)
  if (lab) {
    p <- p + ggplot2::geom_text(
      mapping = ggplot2::aes_string(x = "var_1", y = "var_2"),
      label = label, color = lab_col, size = lab_size)
  }
  p
}          
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# function VII
# too slow it is replaced by foreach version
# correlation for a data.frame
# it check whether they are factor and only show the
# correlation for continuous variables
data_mcor_old <- function(data,
                     digits = 3,
                       plot = TRUE,
                   diag.off = TRUE,
              lower.tri.off = FALSE,
                     method = c("square", "circle"),
                 fit.method = c("P-splines", "loess"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Corr",
                     title,
                    ggtheme = ggplot2::theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black",
                     tl.srt = 45,
                        lab = TRUE,
                    lab_col = "black",
                   lab_size = 3,
                circle.size = 20,
              ...) # c(1,15) maybe will do
{
####################################################################
####################################################################
# local function
  meltit <- function(mat)
  {
      rna <- rownames(mat)
     lrna <- length(rna)
    value <- as.vector(mat)
     Var1 <- gl(length(rna), 1, length = lrna*lrna, labels=rna)
     Var2 <- gl(length(rna), lrna, length = lrna*lrna, labels=rna)
      daf <-  na.omit(data.frame(Var1, Var2, value=value))
     daf
  }
####################################################################
####################################################################
####################################################################
# data.frame missing
if (missing(data) || NROW(data) <= 1)
     stop("nothing to do for this data frame")
# data obs na's
if (is(data, "list"))  stop("the data is list  the function needs a data.frame")
if (is(data, "table")) stop("the data is a table the function needs a data.frame")
if (is(data, "matrix"))    data <- as.data.frame(data)
if (is(data[1],"mts"))     data <- as.data.frame(data)
if (is(data, "array")) stop("the data is an array the function needs a data.frame")
          dimD <- dim(data)
if (any(is.na(data)))
{
    l1 <- dim(data)[1]
  data <- na.omit(data)
    l2 <- dim(data)[1]
  warning(cat(l1-l2, "observations were omitted from the data", "\n"))
}
#  if is a list or table
if (is.null(dimD)) stop("only one variable in the data")
if (dimD[1]<20)   stop(cat("the size of the data set is too small", "\n",
                          "to detect non-linear correlations", "\n"))
# translate any character vectors in he data as factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)],as.factor)
# the problem remains that integers can have very few observations
# a <- data[sapply(data, is.integer)] |> sapply(FUN=unique) |> lapply(FUN=length)|>
#        simplify2array()
# b <- a[which(a<10)]
# if (length(b)>1)
# warning("the integer vectors",  names(b), "have less than 10 district values", "\n")
           TT <- sapply(data, table)
         daTa <- subset(data, select=
ifelse(sapply(data,is.factor) | sapply(data,is.character) |
                sapply(data,is.integer)&lapply(TT, length)<10, FALSE, TRUE))
          Dim <- dim(daTa)
if (Dim[2]==0) stop("no variable is left after taking out the factors")
if (Dim[2]==1) stop("only one variable is left after taking out the factors")
    diffDim  <- dimD[2]-Dim[2]
if (diffDim > 0)
   {
     warning(cat(diffDim, 'factors have been omited from the data', "\n"))
   }
        cnames <- names(daTa)
       lcnames <- length(cnames)
            CC <- matrix(0, ncol=lcnames, nrow=lcnames)
           # CC1<-matrix(0, ncol=lcnames, nrow=lcnames)
#get CC
 for (i in 1:lcnames)
    {
      for (j in 2:lcnames)
      {
     #   cat(names(daTa)[i],names(daTa)[j], "\n")
        xi <- if(is.null(dim(daTa[,i]))) daTa[,i] else daTa[,i][,1]
        xj <- if(is.null(dim(daTa[,j]))) daTa[,j] else daTa[,j][,1]
        if (i<j) CC[i,j] <- CC[j,i] <- mcor(xi, xj, ...)
      }
 }
             CC1 <- cor(daTa)
             CC1 <- base::round(x = CC, digits = digits)
    rownames(CC) <- cnames
    colnames(CC) <- cnames
        diag(CC) <- 1
              CC <- base::round(x = CC, digits = digits)
 if (diag.off) diag(CC) <- NA
if  (lower.tri.off) CC[lower.tri(CC)] <- NA
if (plot==FALSE) return(CC)
     method <- match.arg(method)
 fit.method <-  match.arg(fit.method)
       corr <- meltit(CC)
   lowerLim <- 25-floor((range(corr$value)[2]-range(corr$value)[1])*20)
colnames(corr) <- c("var_1", "var_2", "value")
txt.title <- if (missing(title))
    paste("Maximal correlations from data:", deparse(substitute(data)))
    else title
corr$abs_corr <- abs(corr$value) * 10
   p <- ggplot2::ggplot(data = corr,
               mapping = ggplot2::aes_string(x = "var_1", y = "var_2",
                                    fill = "value"))
  if (method == "square")
   {
      p <- p + ggplot2::geom_tile(color = outline.color)
    }
   else if (method == "circle") {
      p <- p + ggplot2::geom_point(color = outline.color, shape = 21,
                        ggplot2::aes_string(size = "abs_corr")) +
                        ggplot2:: scale_size_area(max_size = circle.size) +
                        ggplot2::guides(size = "none")
    }
    label <- round(x = CC, digits = digits)
    p <- p + ggplot2::scale_fill_gradient2(low = colors[1], high = colors[3],
           mid = colors[2],  midpoint = 0.5, limit = c(0, 1),
           space = "Lab",
           name = legend.title)+ 
          ggplot2::ggtitle(txt.title)
    if (class(ggtheme)[[1]] == "function") {
      p <- p + ggtheme
    }
    else if (class(ggtheme)[[1]] == "theme") {
      p <- p + ggtheme
    }
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = tl.srt,
               vjust = 1, size = tl.cex, colour=tl.col, hjust = 1),
             axis.text.y = ggplot2::element_text(size = tl.cex, colour=tl.col)) +
             coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)
if (lab) {
      p <- p + ggplot2::geom_text(
                     mapping = ggplot2::aes_string(x = "var_1", y = "var_2"),
                     label = label, color = lab_col, size = lab_size)
    }
    p
  }
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################ 