
variogram <- function(data, phylo, weights = "IID", complete = FALSE, time.units = "Ma", trait.units = NULL,  progress = TRUE, algorithm = "GMM"){
  
  #For testing
  # data("moid_traits")
  # data("musteloids")
  # data <- moid_traits$SSD
  # phylo <- musteloids
  # complete <-  FALSE
  # algorithm <- "kmeans"
  # progress <- TRUE
  # weights = "IID"
  
  SPECIES <- phylo$tip.label
  names(data) <- SPECIES
  
  #Get all pairwise phylogenetic distances
  DISTS <- as.data.frame(as.table(ape::cophenetic.phylo(phylo)))
  DISTS <- DISTS[order(DISTS$Freq),]
  
  #Lag BINS
  if(complete){
    TAU <- unique(DISTS$Freq)
  } else {
    LAGS <- DISTS$Freq
    
    if(algorithm == "kmeans"){
      N <- round(sqrt(length(LAGS))+1)

      INIT <- matrix(seq(0,
                         max(LAGS),
                         length.out = N))
      
      CENTERS <- ClusterR::KMeans_rcpp(matrix(LAGS),
                                       CENTROIDS = INIT,
                                       clusters = N)$centroids
      
      CENTERS <- na.omit(CENTERS)[,1]
      # Remove redundant means and sort
      TAU <- sort(unique(CENTERS))
    }
    
    if(algorithm == "GMM"){
      
      #Gaussian Mixture model clustering
      CENTERS <- ClusterR::GMM(matrix(LAGS),
                             gaussian_comps = sqrt(length(LAGS))+1)$centroids
      
      # Remove redundant means and sort
      TAU <- sort(unique(CENTERS))
    }
  }
  
  
  # Calculate gamma 
  GAMMA <- vector("numeric", length(TAU))
  CI_min <- vector("numeric", length(TAU))
  CI_max <- vector("numeric", length(TAU))
  VAR <- vector("numeric", length(TAU))
  DOF <- vector("numeric", length(TAU))
  
  if(progress){
    pb <- txtProgressBar(min = 0, max = length(TAU)-1, style = 3)
  }
  
  for(k in 2:length(TAU)){
    
    #Species with a common ancestor separated by lag tau
    if(complete){
      SUB <- DISTS[DISTS$Freq == TAU[k],]
    } else{
      SUB <- DISTS[DISTS$Freq <= TAU[k] & DISTS$Freq > TAU[k-1],]
    }
    
    #Check for lags without data. Happens when complete = FALSE
    if(nrow(SUB) >0){
      
      
      #######################################################
      ##########        Calculate the weights       #########
      #######################################################
      
      #uniform weights (not statistically appropriate, but I've included them in case they're needed for testing)
      if(weights == "uniform"){
        PAIRS <- expand.grid(SUB$Var1,SUB$Var2)
        COV <- diag(nrow(PAIRS))
        ONE <- rep(1, nrow(COV))
        W <- PDsolve(COV) %*% ONE
        W <- W/sum(W)
      }
      
      
      #IID weights
      if(weights == "IID"){
        PAIRS <- combn(unique(as.character(SUB[,1])),2)
        
        #IID weights
        N <- length(unique(SUB[,1]))
        COV <- array(0,c(N,N,N,N)) # where N <- length(SUBSET)
        # then set the off-diagonals in a loop (which also incorrectly sets the diagonals to 1/2, but that's fine if we do it first)
        for(i in 1:N){
          COV[i,,i,] <- 1/4
          COV[i,,,i] <- 1/4
          COV[,i,i,] <- 1/4
          COV[,i,,i] <- 1/4
        }
        
        # then flatten 
        dim(COV) <- c(N^2,N^2)
        
        # Drop any that aren't needed
        KEEPERS <- match(do.call("paste", SUB[,1:2]), do.call("paste", expand.grid(unique(SUB[,1]),unique(SUB[,1]))))
        COV <- COV[KEEPERS,KEEPERS]
        
        # then set the diagonal to 1
        diag(COV) <- 1
        
        # Vector of 1s
        ONE <- rep(1, nrow(COV))
        
        W <- solve(COV) %*% ONE # can switch to ctmm's PDsolve if the behaviour of solve is not ideal
        W <- W/sum(W)#} else {
      }
      
      
      
      #BM weights
      if(weights == "BM"){
        
        PAIRS <- combn(unique(as.character(SUB[,1])),2)
        
        #Species at time lag tau
        phylo_sub <- ape::keep.tip(phylo, as.character(SUB$Var1))
        
        VCV <- ape::vcv(phylo_sub)
        
        N <- nrow(unique(SUB))
        COV <- array(0,c(N,N)) # where N <- length(SUBSET)
        
        for(i in 1:nrow(SUB)){
          for(j in 1:nrow(SUB)){
            IJ <- VCV[as.character(SUB[,1][i]),as.character(SUB[,2][i])]
            KL <- VCV[as.character(SUB[,1][j]),as.character(SUB[,2][j])]
            COV[i,j] <- (IJ + KL)/TAU[k]
          } #closes loop over j
        } #closes loop over i
        
        # then set the diagonal
        diag(COV) <- 1
        
        #Squared correlation matrix
        COV <- COV^2
        
        #Vector of 1s
        ONE <- rep(1, nrow(COV))
        
        #Calculate the weights
        W <- PDsolve(COV) %*% ONE
        W <- W/sum(W)
        
      }
      
      
      # Weighted semi-variance
      DIFFS <- vector("numeric", nrow(SUB))
      for(l in 1:length(DIFFS)){
        DIFFS[l] <- W[l]*0.5*abs((data[SUB$Var1[l]] - data[SUB$Var2[l]]))^2
      }
      GAMMA[k] <- sum(DIFFS)
      
      
      #Variance
      VAR[k] <- 2 * (t(W) %*% COV %*% W) * (GAMMA[k])^2
      
      DOF[k] <- 2*GAMMA[k]^2/VAR[k]
      
      
      #Update progress bar is turned on
      if(progress){setTxtProgressBar(pb, k)}
    } #Closes the check for any data in the lag
  }
  
  #Convert TAU for correct plotting
  UNITS <- NULL
  LAG <- tryCatch({LAG <- TAU %#% time.units
    
    UNITS <- "time"
    
    LAG
    
    }, error=function(err){TAU} )
  
  if(is.null(UNITS)){UNITS <- "unknown"}

  
  #Store outputs asata frame
  SVF <- data.frame(SVF=GAMMA,
                    DOF=DOF,
                    lag=LAG)
  
  #Convert to variogram class
  SVF <- new.vg(SVF)
  
  #Set the units of the trait
  if(is.null(trait.units)){SVF@info$axes <- "x"} else {SVF@info$axes <- trait.units}
  
  attr(SVF,"info")$lags <- UNITS
  
  return(SVF)
}
