funcen <- function(data, H, bw, adj, ...){
 UseMethod("funcen")
}

funcen.default <- function(data, H, bw, adj, ...){
 data <- na.omit(data)
 N    <- dim(data)[1]
 vec  <- sum(sapply (data[,2], function(i){identical(i,0) | identical(i,1)}))
 if (vec != length(data[,2])){
  stop(message("Non valid data values"))
 }
 if (sum(data[,2]) < 2){
  stop(message("There should be at least two uncensored data"))
 }
 data <- data[order(data[,1]),]
 if (missing(H)){
  if (missing(bw)){
   bw  <- "naive.pdf"
  }
  if (missing(adj)){
   adj <- 1
  }
  if (bw == "naive.pdf"){
   H <- (adj * diag( N ^ -(1/5), 2)) ^ 2
  }
  else{
   if (bw == "naive.cdf"){
    H <- (adj * diag( N ^ -(1/3), 2)) ^ 2
   }
   else{
    if (bw == "Hpi"){
     H <- adj ^ 2 * Hpi(data[,c(3,1)])
    }
    else{
     if (bw == "Hpi.diag"){
      H<- adj ^ 2 * Hpi.diag(data[,c(3,1)])
     }
     else{
      if (bw == "Hlscv"){
       H <- adj ^ 2 * Hlscv(data[,c(3,1)])
      }
      else{
       if (bw == "Hlscv.diag"){
         H <- adj ^ 2 * Hlscv.diag(data[,c(3,1)])
       }
       else{
        if (bw == "Hbcv"){
         H <- adj ^ 2 * Hbcv(data[,c(3,1)])
        }
        else{
         if (bw == "Hbcv.diag"){
          H <- adj ^ 2 * Hbcv.diag(data[,c(3,1)])
         }
         else{
          if (bw == "Hscv"){
           H <- adj ^ 2 * Hscv(data[,c(3,1)])
          }
          else{
           if (bw == "Hscv.diag"){
            H <- adj ^ 2 * Hscv.diag(data[,c(3,1)])
           }
           else{
            if (bw == "Hucv"){
             H<- adj^2*Hucv(data[,c(3,1)])
            }
            else{
             if (bw == "Hucv.diag"){
              H <- adj ^ 2 * Hucv.diag(data[,c(3,1)])
             }
             else{
              stop(message("Non valid method"))
             }
            }
           }
          }
         }
        }
       }
      }
     }
    }
   }
  }
 }
 W <- diag(1, N, N)
 for (i in 1:(N-1)){
  if (data[i,2] == 0){
   W[(i+1):N, i] <- W[(i+1):N, i] + rep(sum(W[i,]) / (N-i), (N-i))
   W[i,] <- rep(0,N)
  }
 }
 pdata <- NULL
 pb    <- txtProgressBar(title = "Preparing data", label = "0 %", min = 0, max = 100, initial = 50)
 for (i in 1:N){
  info <- sprintf("%d%% Done", round(((i/N) * 100)))
  setTxtProgressBar(pb, ((i/N) * 100), label = info)
  for (j in 1:N){
   if (W[i,j] != 0){
    pdata <- rbind(pdata, c(data[i,1], data[j,3], W[i,j]))
   }
  }
 }
 close(pb)
 l = dim(pdata)[1]
 funcen <- kde(x = cbind(pdata[,2], pdata[,1]), H = H, w = (pdata[,3] * l/N), ...)
}

