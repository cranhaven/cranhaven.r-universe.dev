##################################################################################
##################################################################################
##################################################################################
##################################################################################
# Mikis concurvity 
# What are the arguments?
# data   :is a data frame 
# full   : whether to condition to all the rest of variables or variable wise comparison 
# linear : linear part of the model (not implemented) 
#          If linear is TRUE we will have to separate the linear 
#          and the non-linear part. This will be using the function 
#          getZmatrix(). The linear part will be included in the 
#          beginning to get her woth one It fails to produce anything sensible
#          see function concurvityL() in Concurvity_with_linear_option.R
#--------------------------------------------------------------------------------
# to do
#   i) at the moment there is no data argument 
#  ii) no full option (OK fixed)
# iii) no linear part yest but  vector of ones is added 
#  iv)  the ndx in bbase has to change to something more meaningful
#      maybe nknots
#   v) maybe we should introduce formulea for both X and Linear
#################################################################################
#################################################################################
#################################################################################
#################################################################################
# authors Mikis Stasinopoulos
# created fist Jan 2018 
# revisted by Mikis Dec 2018
# revised on Marcg 2022
#--------------------------------------------------------------
#-----------------------------------------------------------------
get_concurvity <- function(data, full=TRUE,  ...)
{
  #-----------------------------------------------------------------
  #####local function
  #-----------------------------------------------------------------  
  bbasis <- function(x, xl=NULL, xr=NULL, inter=15, deg=3)
  {
    # DS xl= min, xr=max, inter= number of points within 
    # Construct B-spline basis
    tpower <- function(x, t, p) (x - t) ^ p * (x > t)
    xl <- if(is.null(xl)) min(x)
    xr <- if(is.null(xr)) max(x)
    xmax <- xr+0.01*(xr-xl)
    xmin <- xl-0.01*(xr-xl)
    dx <- (xr - xl) / inter # DS increment 
   # if false use Paul's
     
      knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
      P <- outer(x, knots, tpower, deg)# calculate the power in the knots
      n <- dim(P)[2]
      D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg) # 
      B <- (-1) ^ (deg + 1) * P %*% t(D) 
    
    return(resid(lm(B~1))) #
  }
################################################################################
################################################################################
################################################################################
  if (missing(data) || NROW(data) <= 1) 
    stop("nothing to do for this model")
    
      dimD <- dim(data)
      daTa <- subset(data, select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
       Dim <- dim(daTa)
  diffDim  <- dimD[2]-Dim[2]
if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
}
  
   cnames <-  names(daTa)
  if (is.null(cnames)) cnames <- as.vector(paste("V", seq(1,NCOL(x)), sep=""))
       n <- dim(daTa)[1]
       m <- dim(daTa)[2]
  if (m==1) {x <- as.matrix(daTa)}
    stop <- start <- rep(1, m) 
  #------------------------------------------------
  for (i in 1:m) # start loop
  {
              B2 <- as.matrix(bbasis(daTa[,i],...))
    colnames(B2) <- as.vector(paste(cnames[i], seq(1,dim(B2)[2]), sep="."))
               B <- if (i==1) B2 else cbind(B, B2, deparse.level=2)
          wherev <- grep(cnames[i],colnames(B))
        start[i] <- wherev[1]
         stop[i] <- wherev[length(wherev)]
  }          # finish loop
              B <- cbind(one=rep(1,dim(B)[1]), B) # add column of ones or possible linear
if (dim(B)[2]>n) stop("the number of colums is greater than n \n use smaller basis i.e. inter=5")        
           start <- c(1, start + 1)
            stop <- c(0, stop + 1)
              m  <- m+1
               B <- B[rowSums(is.na(B)) == 0, ]   # dim(B)
               B <- qr.R(qr(B, tol = 0, LAPACK = FALSE))
      n.measures <- 2
   measure.names <- c("worst",  "estimate")
if (full) 
  {  
    res <- matrix(0, n.measures, m)
    for (i in 1:(m)) 
    {
      Xi <- B[, -(start[i]:stop[i]), drop = FALSE]  #take all the rest
      Xj <- B[,   start[i]:stop[i], drop = FALSE]   #take the current basis
      if (ncol(Xi)>n) stop("the number of colums is greater than n \n use smaller basis i.e. inter=5")
      r <- ncol(Xi)   # the number of colums in the rest of the bases
      # here we take the R of QR of the combined Xi and Xj  but we exclude the Xi part
      # the whole R should have been a pxp matrix (where p is the combined number of bases)
      # but we save only  R pxm (where m is number of columns in the current basis)
      # r is p-m number-of all bases minus number-of current basis 
      R <- qr.R(qr(cbind(Xi, Xj), LAPACK = FALSE, tol = 0))[, -(1:r), drop = FALSE]
      # now we take R (mxp) and create from the  QR of R the triangular Rt (pxp) matrix       
      Rt <- qr.R(qr(R))
      # mikis I am not sure if d[3]^2 is correct it used to be d[1]^2  
      # note that forwardsolve solves Ax=b for x when A is a lower triangular  
      res[1, i] <- svd(forwardsolve(t(Rt), t(R[1:r, ,drop = FALSE])))$d[1]^2
      res[2, i] <- sum(R[1:r, ]^2)/sum(R^2)# fitted sum of sqares / totall sum of squares
    }
    colnames(res) <- c("one", cnames)
    rownames(res) <- measure.names
  } else 
  {
    res <- list()
    for (i in 1:n.measures) res[[i]] <- matrix(1, m, m)
    for (i in 1:m) {
      Xi <- B[, start[i]:stop[i], drop = FALSE]
      r <- ncol(Xi)
      for (j in 1:m) if (i != j) {
        Xj <- B[, start[j]:stop[j], drop = FALSE]
        R <- qr.R(qr(cbind(Xi, Xj), LAPACK = FALSE, tol = 0))[, -(1:r), drop = FALSE]
        Rt <- qr.R(qr(R))
        res[[1]][i, j] <- svd(forwardsolve(t(Rt), t(R[1:r, , drop = FALSE])))$d[1]^2
        #beta <- b$coef[start[j]:stop[j]]
        #conc[[2]][i, j] <- sum((R[1:r, , drop = FALSE] %*% 
        #                         beta)^2)/sum((Rt %*% beta)^2)
        res[[2]][i, j] <- sum(R[1:r, ]^2)/sum(R^2)
        rm(Xj, R, Rt)
      }
    }
    for (i in 1:n.measures) rownames(res[[i]]) <- colnames(res[[i]]) <- c("one", cnames)
    names(res) <- measure.names
  }
  res  
}
##################################################################################
##################################################################################
##################################################################################
##################################################################################
data_concurvity <- function(data, 
                     digits = 2,
                       type = c("estimate", "worst"),
                   diag.off = FALSE,
              lower.tri.off = FALSE, 
                       plot = TRUE,
                     method = c("circle","square"),
              outline.color = "gray",
                     colors = c("blue", "white", "red"),
               legend.title = "Concurvity",
                     title,
                    ggtheme = theme_minimal(),
                     tl.cex = 12,
                     tl.col = "black", 
                     tl.srt = 45,
                        lab = FALSE, 
                    lab_col = "black", 
                   lab_size = 3,
              ...) 
{
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
  #require(reshape2)  
         dimD <- dim(data)
         daTa <- subset(data,  select=ifelse(sapply(data,is.factor)|sapply(data,is.character)==TRUE, FALSE, TRUE))
          Dim <- dim(daTa)
     diffDim  <- dimD[2]-Dim[2]
if (diffDim > 0)
  {
    warning(cat(diffDim, 'factors have been omited from the data', "\n"))
}
           type <- match.arg(type)
             CC <- get_concurvity(daTa, full=FALSE, ...)[[type]]
             CC <- base::round(x = CC, digits = digits)
       if ( diag.off) diag(CC) <- NA
       if (lower.tri.off)  CC[lower.tri(CC)]<-NA
      txt.title <- if (missing(title))  
                   paste("Concurvity from data",deparse(substitute(data)))
                   else title  
if (plot==FALSE)  return(CC)
        method <- match.arg(method)
          corr <- meltit(CC)
colnames(corr) <- c("var_1", "var_2", "value")
 corr$abs_corr <- abs(corr$value) * 10
      p <- ggplot(data = corr, 
              mapping = aes_string(x = "var_1", y = "var_2", fill = "value"))
  if (method == "square") {
    p <- p + geom_tile(color = outline.color)
  }
  else if (method == "circle") {
    p <- p + geom_point(color = outline.color, shape = 21, 
                        aes_string(size = "abs_corr")) +
      scale_size(range = c(4, 10)) +
      guides(size = "none")
  }
  label <- round(x = CC, digits = digits)               
  p <- p + scale_fill_gradient2(low = colors[1], high = colors[3], 
                                mid = colors[2], midpoint = 0.5, limit = c(0, 1), space = "Lab",
                                name = legend.title)+ggtitle(txt.title)
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  }
  else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }
  p <- p + theme(axis.text.x = element_text(angle = tl.srt, 
                                            vjust = 1, size = tl.cex, hjust = 1), 
                 axis.text.y = element_text(size = tl.cex)) + 
    coord_fixed()
  label <- round(x = corr[, "value"], digits = digits)  
  if (lab) {
    p <- p + ggplot2::geom_text(mapping = aes_string(x = "var_1", 
                                                     y = "var_2"), 
                                label = label, color = lab_col, size = lab_size)
  }
  p
}

##################################################################################
##################################################################################
##################################################################################
##################################################################################