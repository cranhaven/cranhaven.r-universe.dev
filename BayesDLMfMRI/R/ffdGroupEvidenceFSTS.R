#' @import pbapply

#' @name ffdGroupEvidenceFSTS
#' @title ffdGroupEvidenceFSTS
#' @description
#' \loadmathjax
#' This function can be used to build activation maps for group task-based fMRI data.
#' @details
#' A multivariate dynamic linear model is fitted in the same fashion as at the individual level for every subject in the sample. 
#' However, at this stage, the posterior distributions from all the subjects are combined to build a single one, 
#' which is then employed to compute the activation evidence maps for the group using the Forward State Trajectories Sampler (FSTS) algorithm To deeply understand the method implemented in this package, a reading of \insertCite{CARDONAJIMENEZ2021107297}{BayesDLMfMRI} and \insertCite{cardona2021bayesdlmfmri}{BayesDLMfMRI} is mandatory.
#' @param ffdGroup list of N elements, each being a 4D array (\code{ffdc[i,j,k,t]}) that contains the sequence of MRI images related to each of the N subjects in the sample.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at \code{t=0} (\code{m=0} is the default value when no prior information is available). For the case of available prior information, \code{m0} can be defined as a \mjseqn{p\times q} matrix, where \mjseqn{p} is the number of columns in the covariates object and \mjseqn{q} is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at \code{t=0} (\code{Cova=100} is the default value when no prior information is available). For the case of available prior information, \code{Cova} can be defined as a \mjseqn{p\times p} matrix, where \mjseqn{p} is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between \code{0.85<delta<1}. \code{delta=1} will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure between pair of voxels within every cluster at \code{t=0}. \code{S0=1} is the default value when no prior information is available and defines an \mjseqn{q\times q} identity matrix. For the case of available prior information, \code{S0} can be defined as an \mjseqn{q \times q} matrix, where \mjseqn{q} is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix \code{S0} at \code{t=0} (\code{n1=1} is the default value when no prior information is available). For the case of available prior information, \code{n0} can be set as \code{n0=np}, where \code{np} is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (\code{2<N1<T}) from the \code{ffdc} array employed in the model fitting. \code{N1=NULL} (or equivalently \code{N1=T}) is its default value, taking all the images in the \code{ffdc} array for the fitting process.
#' @param Nsimu1  is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (\code{30<Cutpos1<T}).  
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different \code{r1} values: \mjseqn{r1 = 0, 1, 2, 3, 4}, which leads to \mjseqn{q = 1, 7, 19, 27, 33}, where \mjseqn{q} is the size of the cluster.
#' @param mask 3D array that works as a brain of reference (MNI atlas) for the group analysis.
#' @param Ncores a postive integer indicating the number of threads or cores to be used in the computation of the activation maps.
#' @return It returns a list of the form \code{[[k]][p,x,y,z]}, where \code{k} defines the type of test
#'  (\code{k = 1} for Marginal effect, \code{k = 2} for Joint effect, and \code{k = 3} for Average cluster effect), \code{p} represents the column 
#'  position in the covariates matrix and \code{x,y,z} represent the voxel position in the brain image.
#' @examples
#'\dontrun{
#' # This example can take a long time to run.
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' res <- ffdGroupEvidenceFSTS(ffdGroup = DatabaseGroup, covariates = Covariates, 
#'                             m0 = 0, Cova = 100, delta = 0.95, S0 = 1, 
#'                             n0 = 1, N1 = FALSE, Nsimu1 = 100, Cutpos=30, 
#'                             r1 = 1, mask = MASK, Ncores = 7)
#' str(res)
#' }
#' @export
ffdGroupEvidenceFSTS <- function(ffdGroup, covariates, m0=0, Cova=100,
                                 delta = 0.95, S0 = 1, n0 = 1, 
                                 N1 = FALSE, Nsimu1=100, Cutpos=30, r1, 
                                 mask, Ncores = NULL){
  
  if(is.logical(N1)) {
    if(N1==FALSE){N1 = dim(covariates)[1]}
  }

  # validation
  Ncores  <- .get_n_cores(Ncores)

  .validate_input(
    ffdGroup=ffdGroup,
    covariates=covariates,
    delta=delta,
    n0=n0,
    N1=N1,
    Nsimu1=Nsimu1,
    Cutpos1=Cutpos,
    r1=r1
  )
  
  covariates <- as.matrix(covariates)

  #TAKING THE POSITIONS FROM THE 4D IMAGE WITH NON-NULL VALUES 
  posiffd <- which(mask[,,] != 0, arr.ind = TRUE)
  
  
  

    ffd.out <- pbapply::pbapply(posiffd, 1, .ffdGroupVoxelFSTS, ffdGroup, covariates, m0, Cova,
                                delta, S0, n0, N1, Nsimu1, r1, Cutpos, cl = Ncores)
    
    #number of tests from the output of .ffdsingleVoxelFFBS  (Joint, marginal and LTT)
    Ntest <- 3
    vol.evidence <- list()
    
    
    for(k in 1:(Ntest)){
      vol.evidence[[k]] <- array(0, c(dim(covariates)[2], dim(mask)[1:3]))
    }
    
    
    for(ii in 1:dim(posiffd)[1]){
      vol.evidence[[1]][ ,posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceJoint
      vol.evidence[[2]][ ,posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenceMargin
      vol.evidence[[3]][ ,posiffd[ii,1], posiffd[ii,2], posiffd[ii,3]] <- ffd.out[[ii]]$EvidenLTT
    }

    attr(vol.evidence, "class") <- "fMRI_group_evidence"
    
    return(vol.evidence)
    
    

}











