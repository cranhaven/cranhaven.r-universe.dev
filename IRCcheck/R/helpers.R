Gamma_SS <- function(adj, k, cl){
  diag(adj ) <- 1
  
  G1 <- which(adj == 1, arr.ind = F)
  G2 <- which(adj == 1, arr.ind = F)
  
  grid <- expand.grid(G1, G2)
  
  Gamma_SS <- matrix(
    parSapply(X = 1:nrow(grid), cl = cl, FUN = function(x){
      k[grid[x,1], grid[x,2]]
    }), nrow = length(G1), ncol = length(G2), byrow = F)
  
  return(Gamma_SS)
}

Gamma_ScS <- function(adj, k, cl){
  diag(adj) <- 1
  
  G1 <- which(adj == 0, arr.ind = F)
  G2 <- which(adj == 1, arr.ind = F)
  
  grid <- expand.grid(G1, G2)
  
  Gamma_ScS <- matrix(
    parSapply(X = 1:nrow(grid), cl = cl, FUN = function(x){
      k[grid[x,1], grid[x,2]]
    }), nrow = length(G1), ncol = length(G2), byrow = F)
  
  return(Gamma_ScS)
}

symm_mat <- function (x) {
  x[lower.tri(x)] <- t(x)[lower.tri(x)]
  x
}

compare <- function (Estimate, True) {
  True <- as.matrix(True)
  Estimate <- as.matrix(Estimate)
  TN <- ifelse(True[upper.tri(True)] == 0 & Estimate[upper.tri(Estimate)] == 
                 0, 1, 0)
  TN <- sum(TN)
  FP <- ifelse(True[upper.tri(True)] == 0 & Estimate[upper.tri(Estimate)] != 
                 0, 1, 0)
  FP <- sum(FP)
  TP <- ifelse(True[upper.tri(True)] != 0 & Estimate[upper.tri(Estimate)] != 
                 0, 1, 0)
  TP <- sum(TP)
  FN <- ifelse(True[upper.tri(True)] != 0 & Estimate[upper.tri(Estimate)] == 
                 0, 1, 0)
  FN <- sum(FN)
  Specificity <- TN/(TN + FP)
  Sensitivity <- TP/(TP + FN)
  Precision <- TP/(TP + FP)
  Recall <- TP/(TP + FN)
  F1_score <- 2 * ((Precision * Recall)/(Precision + Recall))
  MCC <- (TP * TN - FP * FN)/sqrt((TP + FP) * (TP + FN) * (TN + 
                                                             FP) * (TN + FN))
  results <- c(Specificity, Sensitivity, Precision, Recall, 
               F1_score, MCC)
  results_name <- c("Specificity", "Sensitivity", 
                    "Precision", "Recall", "F1_score", 
                    "MCC")
  results <- cbind.data.frame(measure = results_name, score = results)
  return(results)
}
