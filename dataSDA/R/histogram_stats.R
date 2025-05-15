#' @name histogram_stats
#' @title Statistics for Histogram Data
#' @description Functions to compute the mean, variance, covariance, and correlation of histogram-valued data.
#' @param x histogram-valued data object.
#' @param var_name the vraiable name or the column location.
#' @param var_name1 the vraiable name or the column location.
#' @param var_name2 the vraiable name or the column location.
#' @param method methods to calculate statistics: mean and var: BG (default), L2W; cov and cor: BG (default), BD, B, L2W.
#' @param ... additional parameters.
#' @return A numeric value: the mean, variance, covariance, or correlation.
#' @details ...
#' @author Po-Wei Chen, Han-Ming Wu 
#' @seealso int_mean int_var int_cov int_cor
#' @examples
#' library(HistDAWass)
#' @import HistDAWass
#' @export
hist_mean <- function(x, var_name, method = "BG", ...){
  object <- x
  if(method == "BG"){
    ans <- hist_mean_BG(object, var_name)  
  }
  if(method == "L2W"){
    ans <- hist_mean_w(object, var_name)
  }
  ans
}


hist_mean_BG <- function(object, var){
  location_var <- which(colnames(object@M) == var)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  Sample_mean <- c()
  for (i in 1:nr){
    for (j in 1:nc){
      p1 <- object@M[i, j][[1]]@p[2:length(object@M[i, j][[1]]@p)]
      p2 <- object@M[i, j][[1]]@p[1:length(object@M[i, j][[1]]@p) - 1]
      p <- p1 - p2
      m2 <- c()
      for (k in 1:length(p)){
        m1 <- (object@M[i, j][[1]]@x[k] + object@M[i, j][[1]]@x[k + 1])*p[k]
        m2 <- c(m2, m1)
      }
      m3 <- sum(m2)
      Sample_mean <- c(Sample_mean, m3)
    }
  }
  Mean <- matrix(Sample_mean, nrow = nr, byrow = T,
                 dimnames = list(rownames(object@M), colnames(object@M)))
  Sample.means <- apply(Mean, 2, sum)/(2 * nr)
  mean.df <- t(data.frame(Sample.means))
  row.names(mean.df) <- 'mean'
  colnames(mean.df) <- colnames(object@M)
  result <- mean.df[location_var]
  return(result)
}

hist_mean_w <- function(object, var){
  MatH.mean <- function(object){
    nr <- nrow(object@M)
    nc <- ncol(object@M)
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@m
        }
      }
    }
    return(mat = MAT)
  }
  location_var <- which(colnames(object@M) == var)
  Mean <- apply(MatH.mean(object), 2, mean)
  mean.df <- t(data.frame(Mean))
  row.names(mean.df) <- 'mean'
  colnames(mean.df) <- colnames(object@M)
  result <- mean.df[location_var]
  return(result)
}

#' @rdname histogram_stats
#' @export  
hist_var <- function(x, var_name, method = "BG", ...){
  
  object <- x
  if(method == "BG"){
    ans <- hist_var_BG(object, var_name)  
  }
  if(method == "L2W"){
    ans <- hist_var_w(object, var_name)
  }
  ans
}

hist_var_BG <- function(object, var){
  location_var <- which(colnames(object@M) == var)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  b1 <- c()
  b3 <- c()
  
  for (i in 1:nr){
    for (j in 1:nc){
      p1 <- object@M[i, j][[1]]@p[2:length(object@M[i, j][[1]]@p)]
      p2 <- object@M[i, j][[1]]@p[1:length(object@M[i, j][[1]]@p) - 1]
      p <- p1 - p2
      a <- c()
      a1 <- c()
      for (k in 1:length(p)){
        s <- ((object@M[i, j][[1]]@x[k])^2 + (object@M[i, j][[1]]@x[k + 1])^2 + (object@M[i, j][[1]]@x[k])*(object@M[i, j][[1]]@x[k + 1]))*p[k]
        s1 <- (object@M[i, j][[1]]@x[k] + object@M[i, j][[1]]@x[k + 1])*p[k]
        a <- c(a, s)
        a1 <- c(a1, s1)
      }
      b <- sum(a)
      b1 <- c(b1, b)
      b2 <- sum(a1)
      b3 <- c(b3, b2)
    }
  }
  
  B <- matrix(b1, nrow = nrow(object@M), byrow = T,
              dimnames = list(rownames(object@M), colnames(object@M)))
  C <- matrix(b3, nrow = nrow(object@M), byrow = T,
              dimnames = list(rownames(object@M), colnames(object@M)))
  
  squarefun <- function(x){
    x <- sum(x)^2
  }
  
  S2 <- apply(B, 2, sum)/(3 * nr) - apply(C, 2, squarefun)/(4 * nr^2)
  var.df <- t(data.frame(S2))
  row.names(var.df) <- 'variance'
  colnames(var.df) <- colnames(object@M)
  result <- var.df[location_var]
  return(result)
}


Sd.Wass <- function(object, var){
  location_var <- which(colnames(object@M) == var)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  MatH.sd <- function(object) {
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@s
        }
      }
    }
    return(mat = MAT)
  }
  
  myfun <- function(x){
    sum(x^2)
  }
  
  Sigma <- apply(MatH.sd(object), 2, myfun)/(nr)^2
  R_list <- list()
  
  for(k in 1:nc){
    Correlaiton_table <- matrix(0, nr, nr)
    Sigma_table <- matrix(0, nr, nr)
    R_table <- matrix(0, nr, nr)
    for(i in 1:nr){
      for(j in 1:nr){
        Correlaiton_table[i, j] <- rQQ(object@M[i, k][[1]], object@M[j, k][[1]])
        Sigma_table[i, j] <- MatH.sd(object)[i, k] * MatH.sd(object)[j, k]
      }
    }
    R_table <- Correlaiton_table * Sigma_table
    dimnames(R_table) <- list(rownames(object@M), rownames(object@M))
    assign(paste("R_table", k, sep = ""), R_table)
    R_list[[k]] <- R_table
  }
  
  names(R_list) <- paste0("R_table", 1:nc)
  
  upper_triangle <- function(mat) {
    n <- nrow(mat)
    mat[upper.tri(mat, diag = FALSE)]
  }
  
  sum_upper_triangle <- function(mat) {
    sum(upper_triangle(mat))
  }
  
  sums <- sapply(R_list, sum_upper_triangle)
  Sd <- t(data.frame(sqrt(sums*2/(nr^2) + Sigma)))
  colnames(Sd) <- colnames(object@M)
  rownames(Sd) <- 'variance'
  result <- Sd[location_var]
  return(result)
}

SM2.Wass <- function(object, var){
  location_var <- which(colnames(object@M) == var)
  MatH.mean <- function(object) {
    nr <- nrow(object@M)
    nc <- ncol(object@M)
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@m
        }
      }
    }
    return(mat = MAT)
  }
  
  Mean.MW <- apply(MatH.mean(object), 2, mean)
  
  myfun <- function(x){
    sum(x^2)
  }
  
  SM2.W <- apply(MatH.mean(object), 2, myfun)/nrow(MatH.mean(object)) - Mean.MW ^ 2
  SM2.W.df <- t(data.frame(SM2.W))
  colnames(SM2.W.df) <- colnames(object@M)
  rownames(SM2.W.df) <- 'variance'
  result <- SM2.W.df[location_var]
  return(result)
}


SV2.Wass <- function(object, var){
  location_var <- which(colnames(object@M) == var)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  MatH.sd <- function(object) {
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@s
        }
      }
    }
    return(mat = MAT)
  }
  
  myfun <- function(x){
    sum(x^2)
  }
  
  H <- apply(MatH.sd(object), 2, myfun)/nr
  R_list <- list()
  
  for(k in 1:nc){
    Correlaiton_table <- matrix(0, nr, nr)
    Sigma_table <- matrix(0, nr, nr)
    R_table <- matrix(0, nr, nr)
    for(i in 1:nr){
      for(j in 1:nr){
        Correlaiton_table[i, j] <- rQQ(object@M[i, k][[1]], object@M[j, k][[1]])
        Sigma_table[i, j] <- MatH.sd(object)[i, k] * MatH.sd(object)[j, k]
      }
    }
    R_table <- Correlaiton_table * Sigma_table
    dimnames(R_table) <- list(rownames(object@M), rownames(object@M))
    assign(paste("R_table", k, sep = ""), R_table)
    R_list[[k]] <- R_table
  }
  
  names(R_list) <- paste0("R_table", 1:nc)
  sums <- sapply(R_list, sum)
  SV2 <- t(data.frame(H - sums/(nr^2)))
  colnames(SV2) <- colnames(object@M)
  rownames(SV2) <- 'variance'
  result <- SV2[location_var]
  return(result)
}


hist_var_w <- function(object, var){
  S2.W <- SM2.Wass(object, var) + SV2.Wass(object, var)
  return(S2.W)
}


#' @rdname histogram_stats
#' @export  
hist_cov <- function(x, var_name1, var_name2, method = "BG"){
  object <- x
  var1 <- var_name1
  var2 <- var_name2
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  MatH.mean <- function(object) {
    MAT <- matrix(NA, nr, nc)
    rownames(MAT) <- rownames(object@M)
    colnames(MAT) <- colnames(object@M)
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@m
        }
      }
    }
    return(mat = MAT)
  }
  
  MatH.sd <- function(object) {
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@s
        }
      }
    }
    return(mat = MAT)
  }
  
  Gj <- function(a, b, p, hmean){
    if (sum((a + b) * p) / 2 <= hmean){
      return(-1)
    } else {
      return(1)
    }
  }
  
  Qj <- function(a, b, hmean){
    return((a - hmean)^2 + (a - hmean) * (b - hmean) + (b - hmean)^2)
  }
  
  QQ <- function(a, b, hmean1, hmean2){
    return(outer((a - hmean1), (b - hmean2)))
  }
  
  get_pvars <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    p1 <- object1@p[2:length(object1@p)]
    p2 <- object1@p[1:(length(object1@p) - 1)]
    p3 <- object2@p[2:length(object2@p)]
    p4 <- object2@p[1:(length(object2@p) - 1)]
    pvar1 <- p1 - p2
    pvar2 <- p3 - p4
    return(list(pvar1 = pvar1, pvar2 = pvar2))
  }
  
  get_GQ <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    lenx1 <- length(object1@x)
    lenx2 <- length(object2@x)
    p <- get_pvars(i)
    Q1 <- Qj(object1@x[1:(lenx1 - 1)], object1@x[2:lenx1], hist_mean_BG(object, var1))
    Q2 <- Qj(object2@x[1:(lenx2 - 1)], object2@x[2:lenx2], hist_mean_BG(object, var2))
    G1 <- Gj(object1@x[1:(lenx1 - 1)], object1@x[2:lenx1], p$pvar1, hist_mean_BG(object, var1))
    G2 <- Gj(object2@x[1:(lenx2 - 1)], object2@x[2:lenx2], p$pvar2, hist_mean_BG(object, var2))
    return(list(Q1 = Q1, Q2 = Q2, G1 = G1, G2 = G2))
  }
  
  get_QQ <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    lenx1 <- length(object1@x)
    lenx2 <- length(object2@x)
    Q1 <- QQ(object1@x[2:lenx1], object2@x[2:lenx2],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q2 <- QQ(object1@x[2:lenx1], object2@x[1:(lenx2 - 1)],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q3 <- QQ(object1@x[1:(lenx1 - 1)], object2@x[2:lenx2],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q4 <- QQ(object1@x[1:(lenx1 - 1)], object2@x[1:(lenx2 - 1)],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    return(list(Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4))
  }
  
  if (method == 'BG'){
    result <- sum(MatH.mean(object)[, location_var1] *
                    MatH.mean(object)[, location_var2])/nrow(object@M) -
      hist_mean_BG(object, var1) * hist_mean_BG(object, var2)
    return(result)
  } else if (method == 'BD'){
    ss <- 0
    for (i in 1:nr){
      ss <- ss + sum(get_GQ(i)$G1 * get_GQ(i)$G2 *
                       outer(get_pvars(i)$pvar1, get_pvars(i)$pvar2) *
                       outer(get_GQ(i)$Q1, get_GQ(i)$Q2)^0.5)
    }
    return(ss / (3 * nrow(object@M)))
  } else if (method == 'B'){
    ss <- 0
    for (i in 1:nr){
      ss <- ss + sum((2 * get_QQ(i)$Q1 + get_QQ(i)$Q2 + get_QQ(i)$Q3 + 2 * get_QQ(i)$Q4) *
                       outer(get_pvars(i)$pvar1, get_pvars(i)$pvar2))
    }
    return(ss / (6 * nrow(object@M)))
  } else if (method == 'L2W'){
    CM <- sum(MatH.mean(object)[, location_var1] *
                MatH.mean(object)[, location_var2])/nrow(object@M) -
      hist_mean_w(object, var1) * hist_mean_w(object, var2)
    
    s1 <- 0
    s2 <- 0
    for (i in 1:nr){
      s1 <- s1 + sum(rQQ(object@M[i, location_var1][[1]], object@M[i, location_var2][[1]]) *
                       MatH.sd(object)[i, location_var1] *
                       MatH.sd(object)[i, location_var2]) / nrow(object@M)
    }
    for (i in 1:nr){
      for (j in 1:nr){
        s2 <- s2 + sum(rQQ(object@M[i, location_var1][[1]], object@M[j, location_var2][[1]]) *
                         MatH.sd(object)[i, location_var1] *
                         MatH.sd(object)[j, location_var2]) / nrow(object@M)^2
      }
    }
    CV <- s1 - s2
    result <- CM + CV
    return(result)
  }
}


#' @rdname histogram_stats
#' @export  
hist_cor <- function(x, var_name1, var_name2, method = "BG"){
  object <- x
  var1 <- var_name1
  var2 <- var_name2
  if (method == 'L2W'){
    result <- hist_cov(object, var1, var2, method = method) /
      (sqrt(hist_var_w(object, var1)) * sqrt(hist_var_w(object, var2)))
  } else{
    result <- hist_cov(object, var1, var2, method = method) /
      (sqrt(hist_var_BG(object, var1)) * sqrt(hist_var_BG(object, var2)))
  }
  return(result)
}


Cov_BD <- function(object, var1, var2){
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  
  Gj <- function(a, b, p, hmean){
    if (sum((a + b) * p) / 2 <= hmean){
      return(-1)
    } else {
      return(1)
    }
  }
  
  Qj <- function(a, b, hmean){
    return((a - hmean)^2 + (a - hmean) * (b - hmean) + (b - hmean)^2)
  }
  
  get_pvars <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    p1 <- object1@p[2:length(object1@p)]
    p2 <- object1@p[1:(length(object1@p) - 1)]
    p3 <- object2@p[2:length(object2@p)]
    p4 <- object2@p[1:(length(object2@p) - 1)]
    pvar1 <- p1 - p2
    pvar2 <- p3 - p4
    return(list(pvar1 = pvar1, pvar2 = pvar2))
  }
  
  get_GQ <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    lenx1 <- length(object1@x)
    lenx2 <- length(object2@x)
    p <- get_pvars(i)
    Q1 <- Qj(object1@x[1:(lenx1 - 1)], object1@x[2:lenx1], hist_mean_BG(object, var1))
    Q2 <- Qj(object2@x[1:(lenx2 - 1)], object2@x[2:lenx2], hist_mean_BG(object, var2))
    G1 <- Gj(object1@x[1:(lenx1 - 1)], object1@x[2:lenx1], p$pvar1, hist_mean_BG(object, var1))
    G2 <- Gj(object2@x[1:(lenx2 - 1)], object2@x[2:lenx2], p$pvar2, hist_mean_BG(object, var2))
    return(list(Q1 = Q1, Q2 = Q2, G1 = G1, G2 = G2))
  }
  ss <- 0
  for (i in 1:nrow(object@M)){
    ss <- ss + sum(get_GQ(i)$G1 * get_GQ(i)$G2 *
                     outer(get_pvars(i)$pvar1, get_pvars(i)$pvar2) *
                     outer(get_GQ(i)$Q1, get_GQ(i)$Q2)^0.5)
  }
  return(ss / (3 * nrow(object@M)))
}


Cov_B <- function(object, var1, var2){
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  
  QQ <- function(a, b, hmean1, hmean2){
    return(outer((a - hmean1), (b - hmean2)))
  }
  
  get_pvars <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    p1 <- object1@p[2:length(object1@p)]
    p2 <- object1@p[1:(length(object1@p) - 1)]
    p3 <- object2@p[2:length(object2@p)]
    p4 <- object2@p[1:(length(object2@p) - 1)]
    pvar1 <- p1 - p2
    pvar2 <- p3 - p4
    return(list(pvar1 = pvar1, pvar2 = pvar2))
  }
  
  get_QQ <- function(i){
    object1 <- object@M[i, location_var1][[1]]
    object2 <- object@M[i, location_var2][[1]]
    lenx1 <- length(object1@x)
    lenx2 <- length(object2@x)
    Q1 <- QQ(object1@x[2:lenx1], object2@x[2:lenx2],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q2 <- QQ(object1@x[2:lenx1], object2@x[1:(lenx2 - 1)],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q3 <- QQ(object1@x[1:(lenx1 - 1)], object2@x[2:lenx2],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    Q4 <- QQ(object1@x[1:(lenx1 - 1)], object2@x[1:(lenx2 - 1)],
             hist_mean_BG(object, var1), hist_mean_BG(object, var2))
    return(list(Q1 = Q1, Q2 = Q2, Q3 = Q3, Q4 = Q4))
  }
  
  ss <- 0
  for (i in 1:nrow(object@M)){
    ss <- ss + sum((2 * get_QQ(i)$Q1 + get_QQ(i)$Q2 + get_QQ(i)$Q3 + 2 * get_QQ(i)$Q4) *
                     outer(get_pvars(i)$pvar1, get_pvars(i)$pvar2))
  }
  return(ss / (6 * nrow(object@M)))
}



CM_W <- function(object, var1, var2){
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  MatH.mean <- function(object) {
    r <- nrow(object@M)
    c <- ncol(object@M)
    MAT <- matrix(NA, nrow(object@M), ncol(object@M))
    rownames(MAT) <- rownames(object@M)
    colnames(MAT) <- colnames(object@M)
    for (i in 1:r) {
      for (j in 1:c) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@m
        }
      }
    }
    return(mat = MAT)
  }
  
  result <- sum(MatH.mean(object)[, location_var1] *
                  MatH.mean(object)[, location_var2])/nrow(object@M) -
    hist_mean_w(object, var1) * hist_mean_w(object, var2)
  return(result)
  
}


CV_M <- function(object, var1, var2){
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  MatH.sd <- function(object) {
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@s
        }
      }
    }
    return(mat = MAT)
  }
  s1 <- 0
  s2 <- 0
  for (i in 1:nrow(object@M)){
    s1 <- s1 + sum(rQQ(object@M[i, location_var1][[1]], object@M[i, location_var2][[1]]) *
                     MatH.sd(object)[i, location_var1] *
                     MatH.sd(object)[i, location_var2]) / nrow(object@M)
  }
  for (i in 1:nrow(object@M)){
    for (j in 1:nrow(object@M)){
      s2 <- s2 + sum(rQQ(object@M[i, location_var1][[1]], object@M[j, location_var2][[1]]) *
                       MatH.sd(object)[i, location_var1] *
                       MatH.sd(object)[j, location_var2]) / nrow(object@M)^2
    }
  }
  result <- s1 - s2
  return(result)
}



C_W <- function(object, var1, var2){
  Cw <- CM_W(object, var1, var2) + CV_M(object, var1, var2)
  return(Cw)
}



Cov_W <- function(object, var1, var2){
  location_var1 <- which(colnames(object@M) == var1)
  location_var2 <- which(colnames(object@M) == var2)
  nr <- nrow(object@M)
  nc <- ncol(object@M)
  MatH.mean <- function(object) {
    MAT <- matrix(NA, nr, nc)
    rownames(MAT) <- rownames(object@M)
    colnames(MAT) <- colnames(object@M)
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@m
        }
      }
    }
    return(mat = MAT)
  }
  
  MatH.sd <- function(object) {
    MAT <- matrix(NA, nr, nc,
                  dimnames = list(rownames(object@M), colnames(object@M)))
    for (i in 1:nr) {
      for (j in 1:nc) {
        if (length(object@M[i, j][[1]]@x) > 0) {
          MAT[i, j] <- object@M[i, j][[1]]@s
        }
      }
    }
    return(mat = MAT)
  }
  
  CM <- sum(MatH.mean(object)[, location_var1] *
              MatH.mean(object)[, location_var2])/nrow(object@M) -
    hist_mean_w(object, var1) * hist_mean_w(object, var2)
  
  s1 <- 0
  s2 <- 0
  for (i in 1:nr){
    s1 <- s1 + sum(rQQ(object@M[i, location_var1][[1]], object@M[i, location_var2][[1]]) *
                     MatH.sd(object)[i, location_var1] *
                     MatH.sd(object)[i, location_var2]) / nrow(object@M)
  }
  for (i in 1:nr){
    for (j in 1:nr){
      s2 <- s2 + sum(rQQ(object@M[i, location_var1][[1]], object@M[j, location_var2][[1]]) *
                       MatH.sd(object)[i, location_var1] *
                       MatH.sd(object)[j, location_var2]) / nrow(object@M)^2
    }
  }
  CV <- s1 - s2
  result <- CM + CV
  return(result)
}


