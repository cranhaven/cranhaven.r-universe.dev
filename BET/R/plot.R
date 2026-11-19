uni <- function(x){
  Fx <- ecdf(x)
  Fx(x)
}

frac.2 <- function(a,d){
  a0 <- a
  b <- rep(NA,d)
  for (i in 1:d){
    temp <- 2^i*a0
    if (temp>1){
      b[i] <- 1
      a0 <- a0-1/2^i
    }else{
      b[i] <- 0
    }
  }
  b
}


frac2 <- function(a,d){
  if (d==1){
    t(t(sapply(a,function(x){frac.2(x,d)})))
  }else{
    t(sapply(a,function(x){frac.2(x,d)}))
  }
}


bex.centers <- function(depth){			# depth >=1
  cbind(rep((1:2^depth)/2^depth,2^depth),rep( (1:2^depth)/2^depth, rep(2^depth,2^depth)   ))-1/2^(depth+1)
}


plot_bid <- function(depth, be.ind1, be.ind2){
  xyc <- bex.centers(depth)
  BEx <- frac2(xyc[,1], depth)
  BEy <- frac2(xyc[,2], depth)

  RDx <- 2*BEx-1
  RDy <- 2*BEy-1

  be.ind1.num <- as.numeric(unlist(strsplit(be.ind1,"")))
  be.ind1.num <- which(be.ind1.num == 1)
  x.prod <- apply(RDx[,be.ind1.num,drop=F],1,prod) #1:row

  be.ind2.num <- as.numeric(unlist(strsplit(be.ind2,"")))
  be.ind2.num <- which(be.ind2.num == 1)
  y.prod <- apply(RDy[,be.ind2.num,drop=F],1,prod)

  col.ind <- x.prod*y.prod

  for (i.col in 1: nrow(xyc)){
    if (col.ind[i.col]<0){
      xycc <- xyc[i.col,]
      xp <- c(xycc[1]-1/2^(depth+1),xycc[1]+1/2^(depth+1),xycc[1]+1/2^(depth+1),xycc[1]-1/2^(depth+1))
      yp <- c(xycc[2]-1/2^(depth+1),xycc[2]-1/2^(depth+1),xycc[2]+1/2^(depth+1),xycc[2]+1/2^(depth+1))
      polygon(xp,yp,border=NA,col=rgb(0,0,1,1/4))
    }
  }
}

bet <- function(X, dep, unif.margin = FALSE, cex=0.5, index = list(c(1:ncol(X))), ...) UseMethod("bet")


bet.plot <- function(X, dep, unif.margin = FALSE, cex=0.5, index = list(c(1:ncol(X))), ...){
  if(ncol(X) != 2) stop("X does not have two columns.")
  p = 2
  if(identical(index, list(c(1:p)))){
    # c(1:p):uniformity
    unif.margin = TRUE
    if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
    test.uniformity = TRUE
    test.independence = FALSE
  }else{
    test.uniformity = FALSE
    test.independence = TRUE
    # test index cover all 1:p for only 1 time
    v = c()
    for (i in 1:length(index)) {
      v = c(v, index[[i]])
    }
    if(length(v) != p){
      stop("index out of range of 1:p")
    }else if(!all.equal(sort(v), c(1:p))){
      stop("index should be a list of disjoint subsets of 1:p")
    }
  }
  
  bet.res <- BETCpp(X, dep, unif.margin, asymptotic = T, test.uniformity, test.independence, index)
  be.ind1 <- unlist(strsplit(bet.res$Interaction, "-"))[1]
  be.ind2 <- unlist(strsplit(bet.res$Interaction, "-"))[2]
  # be.ind1 <- unlist(strsplit(i1, " "))[1]
  # be.ind2 <- unlist(strsplit(i2, " "))[1]
  if(unif.margin){
    x <- X[,1]
    y <- X[,2]
  }else{
    x <- uni(X[,1])
    y <- uni(X[,2])
  }


  # par(mgp = c(1.8, 0.5, 0),mar=c(3,3,3,1))
  plot(c(0,1), c(0,1), xlab=expression(U[x]),ylab=expression(U[y]),type = "n")
  points(x,y,mgp = c(1.8, 0.5, 0),xlim=c(0,1),ylim=c(0,1),cex=cex,col=2, pch=16)
  plot_bid(dep, be.ind1, be.ind2)
}

MaxBET <- function(X, dep, unif.margin = FALSE, asymptotic = TRUE, plot = FALSE, index = list(c(1:ncol(X)))){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)
  if (p == 1){
    for (i in 1:n){
      if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
    }
  }

  # independent index
  # mutual.idx = list()
  # for(i in 1:p){
  #   mutual.idx[length(mutual.idx) + 1] = c(i)
  # }
  if(identical(index, list(c(1:p)))){
    # c(1:p):uniformity
    unif.margin = TRUE
    if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
    test.uniformity = TRUE
    test.independence = FALSE
  }else{
    test.uniformity = FALSE
    test.independence = TRUE
    # test index cover all 1:p for only 1 time
    v = c()              
    for (i in 1:length(index)) {
      v = c(v, index[[i]])
    }
    if(length(v) != p){
      stop("index out of range of 1:p")
    }else if(!all.equal(sort(v), c(1:p))){
      stop("index should be a list of disjoint subsets of 1:p")
    }
  }

  if (plot && (p == 2))
    bet.plot(X, dep, unif.margin, index = index)
  if (plot && (p != 2)) warning("plot not available: X does not have two columns.")

  BETCpp(X, dep, unif.margin, asymptotic, test.uniformity, test.independence, index)
}

symm <- function(X, dep, unif.margin = FALSE, print.sample.size = TRUE){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)
  if (p == 1){
    for (i in 1:n){
      if (X[i][1] > 1 || X[i][1] < 0) stop("Data out of range [0, 1]")
    }
  }
  res = symmCpp(X, dep, unif.margin)[-1,]
  res = res[order(res$BinaryIndex),]
  rownames(res) = 1:nrow(res)

  if(print.sample.size){
    cat("Sample size: ", n, "\n")
  }

  return(res)
}

get.signs <- function(X, dep, unif.margin = FALSE){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)
  if (p == 1){
    for (i in 1:n){
      if (X[i][1] > 1 || X[i][1] < 0) stop("Data out of range [0, 1]")
    }
  }

  res = colorCpp(X, dep, unif.margin)
  res = res[,order(colnames(res))]

  return(res)
}

cell.counts <- function(X, dep, unif.margin = FALSE){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)
  if (p == 1){
    for (i in 1:n){
      if (X[i][1] > 1 || X[i][1] < 0) stop("Data out of range [0, 1]")
    }
  }

  res = cellCpp(X, dep, unif.margin)

  return(res)
}

MaxBETs <- function(X, d.max=4, unif.margin = FALSE, asymptotic = TRUE, plot = FALSE, index = list(c(1:ncol(X)))){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)

  # independent index
  # mutual.idx = list()
  # for(i in 1:p){
  #   mutual.idx[length(mutual.idx) + 1] = c(i)
  # }

  if(identical(index, list(c(1:p)))){
    # c(1:p):uniformity
    unif.margin = TRUE
    if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
    test.uniformity = TRUE
    test.independence = FALSE
  }else{
    test.uniformity = FALSE
    test.independence = TRUE
    # test index cover all 1:p for only 1 time
    v = c()
    for (i in 1:length(index)) {
      v = c(v, index[[i]])
    }
    if(length(v) != p){
      stop("index out of range of 1:p")
    }else if(!all.equal(sort(v), c(1:p))){
      stop("index should be a list of disjoint subsets of 1:p")
    }
  }

  temp <- MaxBET(X, 1, unif.margin, asymptotic, FALSE, index) #BET
  bet.adj.pvalues <- rep(NA,d.max)
  bet.extreme.asymmetry <- rep(NA,d.max)

  max.abs.count.interaction <- abs(temp$Extreme.Asymmetry)
  bet.extreme.asymmetry[1] <- temp$Extreme.Asymmetry
  # table22 <- matrix(c(max.abs.count.interaction/2+n/4, -max.abs.count.interaction/2+n/4, -max.abs.count.interaction/2+n/4, max.abs.count.interaction/2+n/4), 2, 2)
  # FE22 <- fisher.test(table22,conf.int=FALSE)$p.value- dhyper(table22[1,1],n/2,n/2,n/2)/2
  FE.pvalue0 <- min(temp$p.value.bonf, 1)

  bet.adj.pvalues[1] <- FE.pvalue0
  bet.s.interaction <- temp$Interaction

  if (d.max==1){
    return(list(bet.s.pvalue=temp$p.value.bonf,bet.s.extreme.asymmetry=temp$Extreme.Asymmetry, bet.s.index=temp$Interaction, bet.s.zstatistic=temp$z.statistic))
  }else{
    for (id in 2:d.max){
      tempa <- MaxBET(X, id, unif.margin, asymptotic, FALSE, index) #BET

      max.abs.count.interaction <- abs(tempa$Extreme.Asymmetry)
      bet.extreme.asymmetry[id] <- tempa$Extreme.Asymmetry

      if (p == 1){
        FE.pvalue <- min(tempa$p.value.bonf/(2^id-1) * ((2^id-1) - (2^(id-1)-1)), 1)
      }else{
        FE.pvalue <- min((tempa$p.value.bonf/(2^(p*id)-p*(2^id-1)-1)) * ((2^(p*id)-p*(2^id-1)-1) - (2^(p*(id-1))-p*(2^(id-1)-1)-1)), 1)
      }

      bet.adj.pvalues[id] <- FE.pvalue
      if (FE.pvalue < FE.pvalue0){
        bet.s.interaction <- tempa$Interaction
        FE.pvalue0 <- FE.pvalue
      }
    }
    bet.s.pvalue <- min(min(bet.adj.pvalues)*d.max,1)
    dp = which(bet.adj.pvalues==min(bet.adj.pvalues),arr.ind=TRUE)[1]
    if (plot && p == 2)
      bet.plot(X, dp, unif.margin, index = index)
    if (plot && p != 2) warning('plot not available: X does not have two columns.')
    bet.s.extreme.asymmetry <- bet.extreme.asymmetry[which(bet.adj.pvalues==min(bet.adj.pvalues))]
    bet.s.zstat <- abs(bet.s.extreme.asymmetry)/sqrt(n)
    return(list(bet.s.pvalue.bonf=bet.s.pvalue, bet.s.extreme.asymmetry=bet.s.extreme.asymmetry, bet.s.index=bet.s.interaction, bet.s.zstatistic=bet.s.zstat))
  }
}

BEAST <- function(X, dep, subsample.percent = 1/2, B = 100, unif.margin = FALSE, lambda = NULL, index = list(c(1:ncol(X))), method = "p", num = NULL){
  if(is.vector(X)){
    X = as.matrix(X, ncol = 1)
  }
  n <- nrow(X)
  p <- ncol(X)
  if (p == 1){
    if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
  }

  if(is.null(lambda)){
    lambda <- sqrt(log(2^(p * dep)) / (8*n))
  }

  # independent index
  # mutual.idx = list()
  # for(i in 1:p){
  #   mutual.idx[length(mutual.idx) + 1] = c(i)
  # }

  if(identical(index, list(c(1:p)))){
    # c(1:p):uniformity
    unif.margin = TRUE
    if(sum(X > 1 | X < 0) > 0) stop("Data out of range [0, 1]")
    test.uniformity = TRUE
    test.independence = FALSE
  }else{
    test.uniformity = FALSE
    test.independence = TRUE
    # test index cover all 1:p for only 1 time
    v = c()
    for (i in 1:length(index)) {
      v = c(v, index[[i]])
    }
    if(length(v) != p){
      stop("index out of range of 1:p")
    }else if(!all.equal(sort(v), c(1:p))){
      stop("index should be a list of disjoint subsets of 1:p")
    }
  }

  if(is.null(num)){
    if(!method %in% c("p", "s")){
      method <- "NA"
      num <- 1
    }else if(method == "p"){
      num <- 100
    }
  }
  


  m <- n * subsample.percent

  L = BeastCpp(X, dep, m, B, unif.margin, lambda, test.uniformity, test.independence, index, method, num)
  L$Interaction = matrix(as.numeric(unlist(strsplit((unlist(strsplit(L$Interaction, "-"))), ""))), nrow = p, byrow = TRUE)
  return(L)


}

# BEAST.null.simu <- function(n, p, dep, subsample.percent = 1/2, B = 100, lambda = NULL, index = list(c(1:p)), method = "p", num = NULL){
#   if(is.null(lambda)){
#     lambda <- sqrt(log(2^(p * dep)) / (8*n))
#   }
# 
#   # independent index
#   # mutual.idx = list()
#   # for(i in 1:p){
#   #   mutual.idx[length(mutual.idx) + 1] = c(i)
#   # }
# 
#   if(identical(index, list(c(1:p)))){
#     # c(1:p):uniformity
#     test.uniformity = TRUE
#     test.independence = FALSE
#   }else{
#     test.uniformity = FALSE
#     test.independence = TRUE
#     # test index cover all 1:p for only 1 time
#     v = c()
#     for (i in 1:length(index)) {
#       v = c(v, index[[i]])
#     }
#     if(length(v) != p){
#       stop("index out of range of 1:p")
#     }else if(!all.equal(sort(v), c(1:p))){
#       stop("index should be a list of disjoint subsets of 1:p")
#     }
#   }
# 
#   if(!method %in% c("p", "s")){
#     stop("Select a method from permutation or simulation to generate a null distribution.")
#   }
# 
#   if(is.null(num)){
#     if(method == "p"){
#       num = 100
#     }else if(method == "s"){
#       num = 1000
#     }
#   }
# 
# 
#   m <- n * subsample.percent
# 
#   nullCpp(n, p, dep, m, B, lambda, test.uniformity, test.independence, index, method, num)
# 
# }
