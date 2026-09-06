setwd("~/Documents/MkvDiscretizedImplementation/")

library(phytools)
library(phangorn)
library(geiger)

phy <- ape::rtree(10)

for (SIM in 1:250){
  
  phylo <- read.tree(paste("data/sim_trees/sim_tree_", SIM, ".tre", sep=""))
  
  
  NCAT = 4
  
  
  ## this is taken directly from phangorn. 
  
  disc.gamma <- function(alpha, k) {
    if (k == 1) return(1)
    # Compute quantiles to divide the gamma distribution into k categories
    quants <- qgamma((1:(k - 1)) / k, shape = alpha, rate = alpha)
    diff(c(0, pgamma(quants * alpha, alpha + 1), 1)) * k
  }
  
  gammaCats <- disc.gamma(alpha=1, k=NCAT)
  
  
  NCHAR=10
  
  char <- list()
  rate <- rep(NA, NCHAR)
  
  for (i in 1:NCHAR) {
    
    rate[i] <- gammaCats[sample(1:NCAT, 1)]
    
    Q <- matrix (c( -rate[i], rate[i], rate[i], -rate[i]), 2, 2, byrow = T, dimnames=list(0:1,0:1))
    
    char[[i]] <- sim.Mk(tree=phylo, Q=Q, nsim=1)
    
    while (length(levels(char[[i]])) == 1) {
      rate[i] <- gammaCats[sample(1:NCAT, 1)]
      
      Q <- matrix (c( -rate[i], rate[i], rate[i], -rate[i]), 2, 2, byrow = T, dimnames=list(0:1,0:1))
      
      char[[i]] <- sim.Mk(tree=phyl, Q=Q, nsim=1)
    }
    
  }
  
  hist(rate)
  mean(rate)
  
  rate_p2 <- rate
  
  v <- list()
  
  for (i in 1:NCHAR) {
    v[[i]] <- as.vector(char[[i]])
    names(v[[i]]) <- names(char[[i]])
  }
  
  dataMatrix <- as.data.frame(do.call(rbind, v))
  
  write.nexus.data(dataMatrix, file=paste("data/Sim1000Chars_Tree", SIM, "_mMkv_DiscretizedGamma.nex", sep=""), format="standard", interleaved = F)
  write.csv(rate, file=paste("data/Sim1000Chars_Tree", SIM, "_mMkv_DiscretizedGamma.rates.txt", sep=""), quote = F)
  
}
