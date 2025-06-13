stRoc <- function(data, t, H, bw, adj, tcr, meth, ...){
 UseMethod("stRoc")
}

stRoc.default <- function(data, t, H, bw, adj, tcr, meth, ...){
 if (missing(tcr)){
  tcr <- "C"
  warning("C/D ROC curve estimation will be computed", call. = FALSE)}
  else{
   if (!(tcr == "B" | tcr == "C" | tcr == "I")){
    stop(paste(tcr, "-", "Invalid selection for time-dependent ROC curve estimation."), call. = FALSE)
   }
   else{
    if (!(tcr == "I")){
     if (missing(meth)){
      meth <- 1
      warning("C/D ROC curve estimation will be done with the smooth method", call. = FALSE)
     }
     else{
      if (!(meth == "1" | meth == "2")) {
       stop(paste(tcr, "-", "Invalid selection for C/D ROC curve estimation method."), call. = FALSE)
      }
     }
    }
    else{
     meth <- 1
     warning("I/D ROC curve estimation is always computed with the smooth method", call. = FALSE)
    }
   }
  }

 le <- length(t)
 pto <- NULL
 val <- NULL
 for (j in 1:le){
  val <- c(val, ifelse( sum(data[,1] >= t[j]) > 1 & sum(data[,1] * data[,2] < t[j]) > 1, t[j], min(t)-1))
 }
 if (sum(val >= min(t)) == 0){
  stop(message ("Not enough points"))
 }
 pto     <- val[which(val >= min(t))]
 M       <- NULL
 P       <- NULL
 density <- funcen(data, H, bw, adj, ...)
 x       <- density$eval.points[[1]]
 tx      <- density$eval.points[[2]]
 l       <- length(tx)
 p       <- seq(0, 1, 1 / (l - 1))
 for (j in 1:length(pto)){
  TP  <- NULL
  FP  <- NULL
  gr  <- NULL
  auc <- NULL
  R   <- NULL
  pos <- max(which(tx <= pto[j]))
  if (!(tcr == "I")){
   if (meth == 1){
    if (pos == 1 | pos == l - 1){
     pt <- as.character(pto[j])
     warning(paste("Not valid point:", pt), call. = FALSE)
     rm(pos)
    }
    else{
     X   <- pos * rowMeans(density$estimate[,1:pos])
     Y   <- (l -pos) * rowMeans(density$estimate[,(pos + 1):l])
     TP  <- 1 - (cumsum(X) / sum(X))
     FP  <- 1 - (cumsum(Y) / sum(Y))
    }
   }
   else{
    if (meth == 2){
     pr <- NULL
     I0 <- which(data[,1] > pto[j])
     I1 <- which(data[,1] <= pto[j] & data[,2]==1)
     Ip <- which(data[,1] <= pto[j] & data[,2]==0)
     gr[I0]<- 0
     gr[I1]<- 1
     for(i in 1:length(Ip)){
      posX  <- ifelse(sum( x <= data[Ip[i],1]) > 0 , max(which( x <= data[Ip[i],3])),  min(x))
      posti <- ifelse(sum(tx <= data[Ip[i],1]) > 0 , max(which(tx <= data[Ip[i],1])), min(tx))
      pr[i] <- sum(density$estimate[posX,posti:pos])/sum(density$estimate[posX, posti:l])
     }
     gr[Ip] <- pr
      for (i in 1:l){
       TP[i] <- sum(gr[which(data[,3] >= x[i])])/sum(gr)
       FP[i] <- sum(1-gr[which(data[,3] >= x[i])])/sum(1-gr)
      }
    }
    else{
     stop(message("Not valid method"))
    }
   }

   if (!(is.null(TP) & is.null(FP))){
    R   <- approxfun(c(0, FP, 1), c(0, TP, 1))(p)
    auc <- (p[2] - p[1]) * sum(R)
    M   <- rbind(M, cbind(rep(pto[j],l), x, FP, TP, p, R, rep("C", l), rep(auc, l)))
    if (!is.null(gr)){
     P  <- rbind(P, cbind(rep(t[j],length(data[,1])), data[,1],1 - gr))
    }
   }
 }
  if (!(tcr == "C")){
    X   <- density$estimate[,pos] / sum(density$estimate[,pos])
    Y   <- (l - pos) * rowMeans(density$estimate[,(pos + 1):l])
    TP  <- 1- cumsum(X)
    FP  <- 1 - (cumsum(Y) / sum(Y))
    R   <- approxfun(c(0, FP,1),c(0, TP, 1))(p)
    auc <- (p[2] - p[1]) * sum(R)
    M   <- rbind(M, cbind(rep(t[j],l), x, FP, TP, p, R, rep("I",l), rep(auc, l)))
  }
 }
 if (!is.null(P)){
   colnames(P) <- c("time", "obvt", "p")
 }
 if (!is.null(M)){
  ret = list(th = M[,2], FP = M[,3], TP = M[,4], p = M[,5], ROC = M[,6], t = M[,1], auc = M[,8], tcr = M[,7],
             Pi = P)
  class(ret) <- "sROCt"
  return(ret)
  }
  else{
    stop(message("Non results to be shown"))
  }
}
