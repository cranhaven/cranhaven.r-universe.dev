
# noise = noise[environment == idxObs,]

# meetSNR3 <- function(noise, A, SNR){
#   p <- ncol(A)
#   inv <- solve(diag(p) - A)
#   data <- noise%*%inv
#   apply(data,2,var)
#   apply(noise, 2, var)
#   apply(data,2,var)/apply(noise, 2, var)
#   signal <- data%*%A
#   mean(apply(signal, 2, function(c) if(sum(c)!=0) var(c) else 0)/apply(noise,2,var))
#   
#   
#   # lossFct <- function(datatmp, kappa){
#   #   (mean(diag(var(datatmp))) - kappa)^2
#   # }
#   
#   lossFct <- function(datatmp, D, kappa){
#     signal <- datatmp%*%(D%*%A)
#     snrstmp <- apply(signal, 2, function(c) if(sum(c)!=0) var(c) else 0)/apply(noise,2,var)
#     (mean(snrstmp) - kappa)^2
#     # sum((log(snrstmp) - rep(log(kappa), ncol(datatmp)))^2)
#     # sum((log(diag(var(datatmp))) - rep(log(kappa), ncol(datatmp)))^2)
#   }
#   
#   getLoss <- function(D, kappa){
#     invp <- solve(diag(p) - D%*%A)
#     datap <- noise%*%invp
#     lossFct(datap, D, kappa)
#   }
#   
#   continue <- TRUE
#   range <- seq(from = 0, to = 2, by = 0.001) #exp(seq(-5, log(10), length.out = 20))
#   range <- range[range != 0] 
#   if(all(range != 1)) range <- c(1, range)
#   grid <- replicate(p, range, simplify=TRUE)
#   # gridO <- cbind(replicate(p-1, rep(1, length(range))), range)
#   # grid <- NULL
#   #   #expand.grid(replicate(p, range, simplify=FALSE))
#   # 
#   # for(col in 1:p){
#   #   reorder <- 1:(p-1)
#   #   grid <- rbind(grid, gridO[,append(reorder, p, col-1)])
#   # }
#   # 
#   lossOld <- getLoss(diag(rep(1, p)), SNR)
#   
#   while(continue){
#     lossVals <- apply(grid, 1, function(row) getLoss(diag(row), SNR))
#     Dprime <- diag(grid[which.min(lossVals),])
#     lossNew <- getLoss(Dprime, SNR)
#     print(lossNew)
#     print(A)
#     if(lossNew < lossOld){
#       A <- Dprime%*%A
#     }
#     print(A)
#     print(Dprime)
#     # loss with new A
#     loss <- getLoss(diag(rep(1, p)), SNR)
#     if(loss < 0.001 | (lossOld - lossNew) < 0.0001){
#       continue <- FALSE
#     }
#     lossOld <- loss
#     print(loss)
#   }
# 
#   
#   
# }
# 
# 
# 
# 
# 
# # simulate
# simResult <- simulateInterventions(n, p, A, G, intervMultiplier = 3,
#              noiseMult = 1, nonGauss = TRUE, hiddenVars = TRUE,
#              knownInterventions = FALSE, fracVarInt = NULL, simulateObs = TRUE,
#              seed = myseed)
# 
# 
# meetSNR2 <- function(noise, A, SNR){
#   p <- ncol(A)
#   inv <- solve(diag(p) - A)
#   data <- noise%*%inv
#   apply(data,2,var)
#   apply(noise, 2, var)
#   apply(data,2,var)/apply(noise, 2, var)
#   signal <- data%*%A
#   apply(signal, 2, function(c) if(sum(c)!=0) var(c) else 0)/apply(noise,2,var)
#   
#   lossFct <- function(datatmp, D, kappa){
#     signal <- datatmp%*%(D%*%A)
#     snrstmp <- apply(signal, 2, function(c) if(sum(c)!=0) var(c) else 0)/apply(noise,2,var)
#     (mean(snrstmp) - kappa)^2
#    # sum((log(snrstmp) - rep(log(kappa), ncol(datatmp)))^2)
#     # sum((log(diag(var(datatmp))) - rep(log(kappa), ncol(datatmp)))^2)
#   }
#   
#   getLoss <- function(D, kappa){
#     invp <- solve(diag(p) - D%*%A)
#     datap <- noise%*%invp
#     lossFct(datap, D, kappa)
#   }
#   
#   continue <- TRUE
#   range <- exp(seq(-5, log(10), length.out = 20))
#   range <- range[range != 0] 
#   
#   gridO <- cbind(replicate(p-1, rep(1, length(range))), range)
#   grid <- NULL
#   #expand.grid(replicate(p, range, simplify=FALSE))
#   
#   for(col in 1:p){
#     reorder <- 1:(p-1)
#     grid <- rbind(grid, gridO[,append(reorder, p, col-1)])
#   }
#   
#   lossOld <- getLoss(diag(rep(1, p)), SNR)
#   
#   while(continue){
#     lossVals <- apply(grid, 1, function(row) getLoss(diag(row), SNR))
#     Dprime <- diag(grid[which.min(lossVals),])
#     lossNew <- getLoss(Dprime, SNR)
#     print(lossNew)
#     print(A)
#     if(lossNew < lossOld){
#       A <- Dprime%*%A
#     }
#     print(A)
#     print(Dprime)
#     # loss with new A
#     loss <- getLoss(diag(rep(1, p)), SNR)
#     if(loss < 0.001 | (lossOld - lossNew) < 0.0001){
#       continue <- FALSE
#     }
#     lossOld <- loss
#     print(loss)
#   }
#   
#   
#   
# }