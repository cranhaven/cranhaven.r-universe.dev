#' @name .ffdsingleVoxelFFBS
#' @title .ffdsingleVoxelFFBS
#' @description
#' this is an internal function
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' this is an internal function
#' @param posi.ffd  the position of the voxel in the brain image.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param ffdc  a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images that are meant to be analyzed. (i,j,k) define the position of the observed voxel at time t.
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at t=0 (m=0 is the default value when no prior information is available). For the case of available prior information, m0 can be defined as a pXr matrix, where p is the number of columns in the covariates object and r is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at t=0 (Cova=100 is the default value when no prior information is available). For the case of available prior information, Cova0 can be defined as a pXp matrix, where p is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure among voxels within every cluster at t=0. S0=1 is the default value when no prior information is available and defines an rXr identity matrix. For the case of available prior information, S0 can be defined as an rXr matrix, where r is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting.N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos1 a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param Min.vol helps to define a threshold for the voxels considered in
#' the analysis. For example, Min.vol = 0.10 means that all the voxels with values
#' below to max(ffdc)*Min.vol can be considered irrelevant and discarded from the analysis.
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @keywords internal
.ffdsingleVoxelFFBS <- function(posi.ffd, covariates, ffdc, m0, Cova, delta, S0, n0, N1, Nsimu1, Cutpos1, Min.vol, r1){
  
  
  if(r1 == 0){
    
    posi <- .distanceNeighbors (posi.refer = as.vector(posi.ffd), r1)
    
    #BOLD RESPONSE SERIES IN THE CLUSTER RELATED TO posi
    series.def <- sapply(1:(dim(posi)[1]), function(k){ffdc[posi[k,1], posi[k,2], posi[k,3], ]})
    #CHEKING THE THRESHOLD Min.vol FOR THE MAIN TS: JUST TEMPORAL SERIES ABOVE THE THRESHOLD, DISCARD TEMPORAL SERIES WITH NON-SIGNIFICANT SIGNAL
    if(min(series.def[,1]) < Min.vol){
      return(list(EvidenceJoint = rep(NA, dim(covariates)[2]), EvidenceMargin = rep(NA, dim(covariates)[2]), EvidenLTT = rep(NA, dim(covariates)[2])))}else{
      series.def <- matrix((series.def - mean(series.def))/sd(series.def), ncol=1)
      #PRIOR HYPERPARAMETERS FOR q1=1
      m01   <- matrix(rep(m0, dim(covariates)[2]*dim(series.def)[2]), ncol=1)
      Cova1 <- diag(rep(Cova, dim(covariates)[2]))
      S01 <- diag(rep(S0,dim(series.def)[2]))
      #DISCOUNT FACTORS MATRIX
      delta1 <- sqrt(delta)
      Beta1 <-  diag(1/c(rep(delta1, dim(covariates)[2])))
      
      res <- .Individual_Backwards_Sampling(ffd1 = as.matrix(series.def), Cova = as.matrix(covariates), m0In = m01, c0In = Cova1, 
                                            S0In = S01, beta0In = Beta1, nt0In = n0, NIn = N1, Nsimu = Nsimu1, CUTpos = Cutpos1)
      
      return(list(EvidenceJoint = as.vector(res$Eviden_joint), EvidenceMargin = as.vector(res$Eviden_margin), EvidenLTT=as.vector(res$eviden_lt)))
    
    }
  }else{
    #THIS LINE RETURN THE POSITIONS OF EACH VOXEL INSIDE THE CLUSTER GIVEN THE DISTANCE r1
    posi1 <- .distanceNeighbors (posi.refer = as.vector(posi.ffd), r1)
    
    
    aux.pos <- dim(ffdc)[1:3]
    #GOING THROUGH EACH ROW AND CHECKING IF ANY POSITION IS OUTSIDE THE BOUNDS
    row_sub1 <- apply(posi1, 1, function(row, x1){0 < row & row<=x1}, x1=aux.pos)
    posi <- posi1[apply(t(row_sub1), 1, sum)==3, ]
    #BOLD RESPONSE SERIES FOR THE CLUSTER RELATED TO posi
    series <- sapply(1:(dim(posi)[1]), function(k){ffdc[posi[k,1], posi[k,2], posi[k,3], ]})
    #CHEKING THE THRESHOLD Min.vol FOR THE MAIN TS: JUST TEMPORAL SERIES ABOVE THE THRESHOLD, DISCARD TEMPORAL SERIES WITH NON-SIGNIFICANT SIGNAL
    if(min(series[,1]) < Min.vol){return(list(EvidenceJoint = rep(NA, dim(covariates)[2]), EvidenceMargin = rep(NA, dim(covariates)[2]), EvidenLTT = rep(NA, dim(covariates)[2])))}else{
      # IDENTIFYING AND REMOVING TEMPORAL SERIES INSIDE THE CLUSTER WITH ZERO VALUES
      zero.series <- unique(which(series==0, arr.ind = TRUE)[,2]) 
      if(length(zero.series)==0){series.def <- series}else{series.def <- series[,-(zero.series)]}
      #CHECKING THE SIZE OF THE CLUSTER: q=1 or q>1
      #is.vector(series.def)==TRUE THEN q=1 OTHERWISE q>1
      if(is.vector(series.def)){ 
        series.def <- matrix((series.def - mean(series.def))/sd(series.def), ncol=1)
        #PRIOR HYPERPARAMETERS FOR q1=1
        m01   <- matrix(rep(m0, dim(covariates)[2]*dim(series.def)[2]), ncol=1)
        Cova1 <- diag(rep(Cova, dim(covariates)[2]))
        S01 <- diag(rep(S0,dim(series.def)[2]))
        #DISCOUNT FACTORS MATRIX
        delta1 <- sqrt(delta)
        Beta1 <-  diag(1/c(rep(delta1, dim(covariates)[2])))}else{
          series.def <- apply(series.def, 2, function(x){(x-mean(x))/sd(x)})
          #PRIOR HYPERPARAMETERS FOR q1>1
          m01   <- matrix(rep(m0, dim(covariates)[2]*dim(series.def)[2]), ncol=dim(series.def)[2])
          Cova1 <- diag(rep(Cova, dim(covariates)[2]))
          S01 <- diag(rep(S0,dim(series.def)[2]))
          delta1 <- sqrt(delta)
          #DISCOUNT FACTORS MATRIX
          Beta1 <-  diag(1/c(rep(delta1, dim(covariates)[2])))
          
        }
      
      
      
      res <- .Individual_Backwards_Sampling(ffd1 = as.matrix(series.def), Cova = as.matrix(covariates), m0In = m01, c0In = Cova1, 
                                            S0In = S01, beta0In = Beta1, nt0In = n0, NIn = N1, Nsimu = Nsimu1, CUTpos = Cutpos1)
      
      #EVIDENCE OF ACTIVATION FOR A SINGLE VOXEL TAKING INTO ACCOUNT THE INFORMATION OF THE ENTIRE CLUSTER OF SIZE q
      return(list(EvidenceJoint = as.vector(res$Eviden_joint), EvidenceMargin = as.vector(res$Eviden_margin), EvidenLTT=as.vector(res$eviden_lt)))
      
      
      
      
      
    }
    
  }
  
  
  
}
#END FUNCTION

