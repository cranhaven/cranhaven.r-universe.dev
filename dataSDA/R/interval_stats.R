options <- c("CM", "VM", "QM", "SE", "FV", "EJD", "GQ", "SPT")
#' @name interval_stats
#' @title Statistics for Interval Data
#' @description Functions to compute the mean, variance, covariance, and correlation of interval-valued data.
#' @param x interval-valued data with symbolic_tbl class.
#' @param var_name the vraiable name or the column location (multiple vraiables are allowed).
#' @param var_name1 the vraiable name or the column location (multiple vraiables are allowed).
#' @param var_name2 the vraiable name or the column location (multiple vraiables are allowed).
#' @param method methods to calculate statistics: CM (default), VM, QM, SE, FV, EJD, GQ, SPT.
#' @param ... additional parameters
#' @return A numeric value: the mean, variance, covariance, or correlation.
#' @details ...
#' @author Han-Ming Wu 
#' @seealso int_mean int_var int_cov int_cor
#' @examples
#' data(mushroom.int)
#' int_mean(mushroom.int, var_name = "Pileus.Cap.Width")
#' int_mean(mushroom.int, var_name = 2:3)
#' 
#' var_name <- c("Stipe.Length", "Stipe.Thickness")
#' method <- c("CM", "FV", "EJD")
#' int_mean(mushroom.int, var_name, method)
#' int_var(mushroom.int, var_name, method)
#'
#' var_name1 <- "Pileus.Cap.Width"
#' var_name2 <- c("Stipe.Length", "Stipe.Thickness")
#' method <- c("CM", "VM", "EJD", "GQ", "SPT")
#' int_cov(mushroom.int, var_name1, var_name2, method)
#' int_cor(mushroom.int, var_name1, var_name2, method)
#' @importFrom stats var cov lm
#' @export
int_mean <- function(x, var_name, method = "CM", ...){
 
  # x <- bird.int
  # var_name <- "Density"
  # var_name <- c("Density", "Size")
  # method <- c("CM")

  # x <- mushroom.int
  # var_name <- "Pileus.Cap.Width"
  # method <- options
  # int_mean(mushroom.int, var_name, method)
  #
  # var_name <- c("Stipe.Length", "Stipe.Thickness")
  # method <- "CM"
  # method <- c("CM", "VM")
  
  
  
  at <- options %in% method
  mean_tmp <- matrix(0, nrow = length(options),
                        ncol = length(var_name))
  idata <- symbolic_tbl_to_idata(x[, var_name])
  
  compute_mean <- function(X_tmp){
    ifelse(length(var_name) == 1,
           x <- mean(X_tmp),
           x <- colMeans(X_tmp))      
    x
  }
  
  if(at[1]){ # CM
    X_tmp <- Interval_to_Center(idata)
    mean_tmp[1, ] <- compute_mean(X_tmp)
  }
  if(at[2]){ # VM
    X_tmp <- Interval_to_Vertices(idata)
    mean_tmp[2, ] <- compute_mean(X_tmp)
  }
  if(at[3]){ # QM
    X_tmp <- Interval_to_Quantiles(idata)
    mean_tmp[3, ] <- compute_mean(X_tmp)
  }
  if(at[4]){ # SE
    X_tmp <- Interval_to_SE(idata)
    mean_tmp[4, ] <- compute_mean(X_tmp)
  }
  if(at[5]){ # FV
    X_tmp <- Interval_to_FV(idata)
    mean_tmp[5, ] <- compute_mean(X_tmp)
  }
  if(at[6] | at[7] | at[8]){ # EJD, GQ, SPT
    X_tmp <- Interval_to_Center(idata)
    mean_tmp[6, ] <- mean_tmp[7, ] <- mean_tmp[8, ] <- compute_mean(X_tmp)
  }
    
  mean_output <- matrix(mean_tmp[at, ],
                           nrow = length(method),
                           ncol = length(var_name))
  
  
  if(is.numeric(var_name)){
    colnames(mean_output) <- colnames(x)[var_name]
  }else{
    colnames(mean_output) <- var_name
  }
  rownames(mean_output) <- options[at]
  
  mean_output
}



#' @rdname interval_stats
#' @export  
int_var <- function(x, var_name, method = "CM", ...){
  
  # x <- bird.int
  # var_name <- "Density"
  # var_name <- c("Density", "Size")
  # method <- c("CM")
  
  # x <- mushroom.int
  # var_name <- "Pileus.Cap.Width"
  # var_name <- c("Stipe.Length", "Stipe.Thickness")
  # method <- "CM"
  # method <- c("CM", "VM")
  #
  # x <- mushroom.int
  # var_name <- "Pileus.Cap.Width"
  # method <- options
  # int_var(mushroom.int, var_name, method)
  at <- options %in% method
  var_tmp <- matrix(0, nrow = length(options),
                     ncol = length(var_name))
  idata <- symbolic_tbl_to_idata(x[, var_name])
  
  n <- nrow(idata)
  p <- ncol(idata)
  compute_var <- function(X_tmp){
    ifelse(length(var_name) == 1,
           x <- stats::var(X_tmp),
           x <- apply(X_tmp, 2, stats::var))      
    x
  }
  
  if(at[1]){ # CM
    X_tmp <- Interval_to_Center(idata)
    var_tmp[1, ] <- compute_var(X_tmp)
  }
  if(at[2]){ # VM
    X_tmp <- Interval_to_Vertices(idata)
    var_tmp[2, ] <- compute_var(X_tmp)
  }
  if(at[3]){ # QM
    X_tmp <- Interval_to_Quantiles(idata)
    var_tmp[3, ] <- compute_var(X_tmp)
  }
  if(at[4]){ # SE
    X_tmp <- Interval_to_SE(idata)
    var_tmp[4, ] <- compute_var(X_tmp)
  }
  if(at[5]){ # FV
    X_tmp <- Interval_to_FV(idata)
    var_tmp[5, ] <- compute_var(X_tmp)
  }
  if(at[6] | at[8]){ # EJD, SPT
    ans <- numeric(length(var_name))
    names(ans) <- var_name
    for(i in var_name){
      a <- sum(idata[, i,2]^2 + idata[, i,1]*idata[, i,2]+idata[,i,1]^2)
      b <- (sum(idata[, i,1] + idata[, i,2]))^2                
      ans[i] <- a/(3*n)-b/(4*n^2)
    }
    var_tmp[6, ] <- var_tmp[8, ] <- ans
  }
  if(at[7]){ # GQ
    ans <- numeric(length(var_name))
    names(ans) <- var_name
    for(i in var_name){
      a <- sum(idata[, i,2]^2 + idata[, i,1]*idata[, i,2]+idata[,i,1]^2)
      b <- (sum(idata[, i,1] + idata[, i,2]))^2                
      ans[i]  <- a/(3*n)-b/(4*n^2)
    }
    var_tmp[7, ] <- ans
  }
  
  var_output <- matrix(var_tmp[at, ],
                        nrow = length(method),
                        ncol = length(var_name))
  
  
  if(is.numeric(var_name)){
    colnames(var_output) <- colnames(x)[var_name]
  }else{
    colnames(var_output) <- var_name
  }
  rownames(var_output) <- options[at]
  
  var_output
}


#' @rdname interval_stats
#' @export
int_cov <- function(x, var_name1, var_name2, method = "CM", ...){
  
  # x <- bird.int
  # var_name1 <- "Density"
  # var_name2 <- "Size"
  # method <- c("CM")
  
  # x <- mushroom.int
  # var_name1 <- "Pileus.Cap.Width"
  # var_name2 <- c("Stipe.Length", "Stipe.Thickness")
  # method <- c("CM", "VM", "EJD", "GQ", "SPT")
  # int_cov(mushroom.int, var_name1, var_name2, method)
  
  # x <- mushroom.int
  # var_name1 <- "Pileus.Cap.Width"
  # var_name2 <- "Stipe.Length"
  # method <- options
  # int_cov(mushroom.int, var_name1, var_name2, method)

  var_name <- c(var_name1, var_name2)
  at <- options %in% method
  cov_tmp <- new.env()
  cov_tmp <- as.list(cov_tmp)
  idata <- symbolic_tbl_to_idata(x[, var_name])
  
  n <- nrow(idata)
  p <- ncol(idata)
  compute_cov <- function(X_tmp){
    ans <- as.matrix(stats::cov(X_tmp[, var_name1], 
                                X_tmp[, var_name2]))
    if(length(var_name1) == 1){
      rownames(ans) <- var_name1
    }
    if(length(var_name2) == 1){
      colnames(ans) <- var_name2
    }
    ans
  }
  
  if(at[1]){ # CM
    X_tmp <- Interval_to_Center(idata)
    cov_tmp$CM <- compute_cov(X_tmp)
  }
  if(at[2]){ # VM
    X_tmp <- Interval_to_Vertices(idata)
    cov_tmp$VM <- compute_cov(X_tmp)
  }
  if(at[3]){ # QM
    X_tmp <- Interval_to_Quantiles(idata)
    cov_tmp$QM <- compute_cov(X_tmp)
  }
  if(at[4]){ # SE
    X_tmp <- Interval_to_SE(idata)
    cov_tmp$SE <- compute_cov(X_tmp)
  }
  if(at[5]){ # FV
    X_tmp <- Interval_to_FV(idata)
    cov_tmp$FV <- compute_cov(X_tmp)
  }
  if(at[6]){ # EJD
    ans <- matrix(0, nrow = length(var_name1), ncol = length(var_name2))
    rownames(ans) <- var_name1
    colnames(ans) <- var_name2
    for(i in var_name1){
      for(j in var_name2){
        a <- sum(idata[, i,1] + idata[, i,2])
        b <- sum(idata[, j,1] + idata[, j,2])
        c <- sum((idata[, i,1] + idata[, i,2])*(idata[, j,1] + idata[, j,2])) 
        ans[i, j] <- c/(4*n)-(a*b)/(4*n^2)
      }
      cov_tmp$EJD <- ans
    }
  }
  if(at[7] | at[8]){ # GQ  
      xbaru <- (idata[, ,1] + idata[, ,2])/2        
      xbar <- colMeans(xbaru)
      xbar ## centered, so approx 0, 
  }
    
  if(at[7]){ # GQ
    ################################
    # Sjj'=QQ                      #
    ################################
    # (12) sign matrix of the correlations
    Gu = matrix(-1, n, p)
    
    for (j in 1:p){
      for (u in 1:n){ 
        if (xbaru[u,j] > xbar[j])  
          Gu[u,j] = 1
      }
    }
    
    colnames(Gu) <- var_name
    
    Qu = matrix(0, n, p)
    for (j in 1:p){
      for (u in 1:n){ 
        Qu[u,j] = (idata[u,j,1] - xbar[j])^2 + 
          (idata[u,j,1] - xbar[j])*(idata[u,j,2] - xbar[j]) + 
          (idata[u,j,2] - xbar[j])^2
      }
    }
    colnames(Qu) <- var_name
    ans <- matrix(0, nrow = length(var_name1), ncol = length(var_name2))
    rownames(ans) <- var_name1
    colnames(ans) <- var_name2
    
    for(i in var_name1){
      for(j in var_name2){
        ans[i,j] <- sum((Gu[,i]*Gu[,j]*sqrt(Qu[,i]*Qu[,j])))/(3*n)
      }
    }
    cov_tmp$GQ <- ans
  }
  
  if(at[8]){ # SPT
    ans <- matrix(0, nrow = length(var_name1), ncol = length(var_name2))
    rownames(ans) <- var_name1
    colnames(ans) <- var_name2
  
    for(i in var_name1){
      for(j in var_name2){
      a2 <- (idata[, i,1] - xbar[i])*(idata[, j,1] - xbar[j])
      ab <- (idata[, i,1] - xbar[i])*(idata[, j,2] - xbar[j]) +
        (idata[, i,2] - xbar[i])*(idata[, j,1] - xbar[j])
      b2 <- (idata[, i,2] - xbar[i])*(idata[, j,2] - xbar[j])               
      ans[i, j] <- sum(2*a2 + ab + 2*b2)/(6*n)
      }
    }
    cov_tmp$SPT <- ans
  }
  
  cov_output <- cov_tmp

  cov_output
}



#' @rdname interval_stats
#' @export
int_cor <- function(x, var_name1, var_name2, method = "CM", ...){
  
  # x <- mushroom.int
  # var_name1 <- "Pileus.Cap.Width"
  # var_name2 <- c("Stipe.Length", "Stipe.Thickness")
  # method <- c("CM", "VM", "EJD", "GQ", "SPT")
  # int_cor(mushroom.int, var_name1, var_name2, method)
  
  # x <- mushroom.int
  # var_name1 <- "Pileus.Cap.Width"
  # var_name2 <- "Stipe.Length"
  # method <- options
  # int_cor(mushroom.int, var_name1, var_name2, method)
  
  var_1 <- int_var(x, var_name1, method)
  var_2 <- int_var(x, var_name2, method)
  cov_12 <- int_cov(x, var_name1, var_name2, method)
  
  cor_output <- cov_12
  for(k in 1:length(method)){
    for(i in var_name1){
      for(j in var_name2){
        cor_output[[k]][i, j] <- cov_12[[k]][i, j]/sqrt(var_1[k,i]*var_2[k,j])    
      }
    }
  }
  cor_output
}
  


symbolic_tbl_to_idata <- function(symbolic_tbl){
  # idata: [n x p x 2] => min:[n, p, 1] max: [n, p, 2]
  
#  symbolic_tbl <- bird.int
  idata <- array(0, dim = c(dim(symbolic_tbl), 2))
  # dimnames(idata) <- list("min", "max")
  p <- dim(symbolic_tbl)[2]
  for(j in 1:p){
    idata[,j,1:2] <- as.matrix(as.data.frame(c(symbolic_tbl[,j])))  
  }
  dimnames(idata) <- list(row.names(symbolic_tbl),
                          colnames(symbolic_tbl),
                          c("min", "max"))
  idata
}




###########################################################
#                                                         #
#                                                         #
###########################################################
## xc <- Interval2Center(idata)
Interval_to_Center <- function(idata){
  
  n <- dim(idata)[[1]]
  p <- dim(idata)[[2]]
  XC <- (idata[,,1]+idata[,,2])/2
  XC
}

###########################################################
#                                                         #
#                                                         #
###########################################################
Interval_to_Midrange <- function(idata){
  
  n <- dim(idata)[[1]]
  p <- dim(idata)[[2]]
  XR <- (idata[,,2]-idata[,,1])/2
  XR
}



###########################################################
#                                                         #
#                                                         #
###########################################################
## M <- Interval2Vertices(sdt[,1:3,])
## idata=[m, p, 2]
## index=[(1 1 1...2..2..3..3..n...n)' (1............mu)'], mu=n*2^p
Interval_to_Vertices <- function(idata) {
  n <- dim(idata)[[1]]
  p <- dim(idata)[[2]]
  XV <- matrix(0, nrow = n * 2^p, ncol = p)
  
  C.code <- F
  if (C.code == F) {
    cc <- 1
    index <- matrix(0, nrow = n * 2^p, ncol = 2)
    for (i in 1:n) {
      for (j in 1:(2^p)) {
        jj <- (j - 1)
        
        for (k in p:1) {
          if (jj %% 2 == 0) {
            XV[j + (i - 1) * 2^p, k] <- idata[i, k, 1]
          }
          else{
            XV[j + (i - 1) * 2^p, k] <- idata[i, k, 2]
          }
          jj <- jj %/% 2
        }
        index[cc, ] <- c(i, j + (i - 1) * 2^p)
        cc <- cc + 1
      }
    }
  }
  
  rownames(XV) <- paste0(rep(dimnames(idata)[[1]], each = (2^p)),
                         "_", rep(1:(2^p), n))
  colnames(XV) <- dimnames(idata)[[2]]
  XV
}


###########################################################
#                                                         #
#                                                         #
###########################################################
## idata=[m, p, 2]
## index=[(1..n)' (1...mu)'], mu=n*2^p
Interval_to_Quantiles <- function(idata, m = 4) {
  n <- dim(idata)[[1]]
  p <- dim(idata)[[2]]
  XQ <- matrix(0, nrow = n * (m + 1), ncol = p)
  cc <- 1
  index <- matrix(0, nrow = n * (m + 1), ncol = 2)
  for (i in 1:n) {
    aij <- idata[i, , 1]
    bij <- idata[i, , 2]
    
    for (k in 0:m) {
      XQ[cc, ] <- aij + (bij - aij) * k / m
      index[cc, ] <- c(i, cc)
      cc <- cc + 1
    }
  }
  
  rownames(XQ) <- paste0(rep(dimnames(idata)[[1]], each = (m+1)),
                         "_", rep(1:(m+1), n))
  colnames(XQ) <- dimnames(idata)[[2]]
  XQ
}



Interval_to_SE <- function(idata) {
  m <- 1
  n <- dim(idata)[[1]]
  XSE <- Interval_to_Quantiles(idata, m)
  rownames(XSE) <- paste0(rep(dimnames(idata)[[1]], each = (m+1)),
                         "_", rep(1:(m+1), n))
  colnames(XSE) <- dimnames(idata)[[2]]
  XSE
}
 

Interval_to_FV <- function(idata) {
  
  n <- dim(idata)[1]
  p <- dim(idata)[2]
  XFV <- matrix(0, ncol = p, nrow = n)
  for (j in 1:p) {
    x <- idata[, j, 1]
    y <- idata[, j, 2]
    my.lm <- stats::lm(y ~ x)
    #summary(my.lm)$r.squared
    XFV[, j]  <- my.lm$fitted.values
  }
  
  rownames(XFV) <- dimnames(idata)[[1]]
  colnames(XFV) <- dimnames(idata)[[2]]
  XFV
}
  



  