#' @name GroupSingleVoxelFFBS
#' @title GroupSingleVoxelFFBS
#' @description
#' \loadmathjax
#' This function is used to perform a group activation analysis for single voxels based on the FFBS algorithm.
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' This function allows the performance of a group activation analysis for single voxels. A multivariate dynamic linear model is fitted to a cluster of voxels, with its center at location (i,j,k), in the way it is presented in \insertCite{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}.
#' @param posi.ffd  the position of the voxel in the brain image.
#' @param DatabaseGroup  list of N elements, each being a 4D array (\code{ffdc[i,j,k,t]}) that contains the sequence of MRI images related to each of the N subjects in the sample.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param m0 the constant prior mean value for the covariates parameters and common to all voxels within every neighborhood at \code{t=0} (\code{m0=0} is the default value when no prior information is available). For the case of available prior information, \code{m0} can be defined as a \mjseqn{p\times q} matrix, where \mjseqn{p} is the number of columns in the covariates object and \mjseqn{q} is the cluster size.
#' @param Cova a positive constant that defines the prior variances for the covariates parameters at \code{t=0} (\code{Cova=100} is the default value when no prior information is available). For the case of available prior information, \code{Cova} can be defined as a \mjseqn{p \times p} matrix, where \code{p} is the number of columns in the covariates object.
#' @param delta a discount factor related to the evolution variances. Recommended values between \code{0.85<delta<1}. \code{delta=1} will yield results similar to the classical general linear model.
#' @param S0 prior covariance structure between pair of voxels within every cluster at \code{t=0}, \code{S0=1} is the default value when no prior information is available and defines an \mjseqn{q\times q} identity matrix. For the case of available prior information, \code{S0} can be defined as an \mjseqn{q \times q} matrix, where \mjseqn{q} is the common number of voxels in every cluster.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix \code{S0} at \code{t=0} (\code{n1=1} is the default value when no prior information is available). For the case of available prior information, \code{n0} can be set as \code{n0=np}, where \code{np} is the number of MRI images in the pilot sample.
#' @param N1 is the number of images (\code{2<N1<T}) from the \code{ffdc} array employed in the model fitting. \code{N1=NULL} (or equivalently \code{N1=T}) is its default value, taking all the images in the \code{ffdc} array for the fitting process.
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param Cutpos a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (\code{30<Cutpos1<T}).  
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different \code{r1} values: \mjseqn{r1 = 0, 1, 2, 3, 4}, which leads to \mjseqn{q = 1, 7, 19, 27, 33}, where \mjseqn{q} is the size of the cluster.
#' @return a list containing a vector (\code{Evidence}) with the evidence measure of 
#' activation for each of the \code{p} covariates considered in the model, the simulated 
#' online trajectories related to the state parameter, the simulated BOLD responses,
#'  and a measure to examine the goodness of fit of the model \mjseqn{(100 \ast |Y[i,j,k]_t - \hat{Y}[i,j,k]_t |/ \hat{Y}[i,j,k]_t )} for that particular voxel (\code{FitnessV}).
#' @examples
#'\dontrun{
#' # This example can take a long time to run.
#' DatabaseGroup <- get_example_fMRI_data_group()
#' data("covariates", package="BayesDLMfMRI")
#' resSingle <- GroupSingleVoxelFFBS(posi.ffd = c(14, 56, 40), DatabaseGroup,
#'                                   covariates = Covariates, m0 = 0, Cova = 100, 
#'                                   delta = 0.95, S0 = 1, n0 = 1, N1 = FALSE, 
#'                                   Nsimu1 = 100, r1 = 1, Cutpos = 30)
#' }
#' @export
GroupSingleVoxelFFBS <- function(posi.ffd, DatabaseGroup, covariates, m0, Cova, delta, S0, n0, N1, Nsimu1, r1, Cutpos){


  
  if(is.logical(N1)) {
    if(N1==FALSE){N1 = dim(covariates)[1]}
  }

  .validate_input(
    covariates=covariates,
    delta=delta,
    n0=n0,
    N1=N1,
    Nsimu1=Nsimu1,
    r1=r1,
    Cutpos1=Cutpos
  )

  covariates  <- as.matrix(covariates)

  
  if(r1 == 0){
    
    posi <- .distanceNeighbors(posi.refer = posi.ffd, r1)
    Ngroup <- length(DatabaseGroup)
    #BOLD RESPONSE SERIES FOR A SPECIFIC CLUSTER
    series.group = NULL 
    #system.time(
    for(i in 1:Ngroup){
      ffd.c <- DatabaseGroup[[i]]
      series <- sapply(1:dim(posi)[1], function(ii){ffd.c[posi[ii,1], posi[ii,2], posi[ii,3], ]})
      series.group <- rbind(series.group, series)
    }
    
    #case for single voxels
    if(any(series.group==0)){return( NA )}else{
                                            
                                            
                                            Cova1 <- diag(rep(Cova, dim(covariates)[2]))
                                            delta1<- sqrt(delta)
                                            Beta1 <- diag(1/c(rep(delta1, dim(covariates)[2])))
                                            res   <- .Group_Functional_Backwards_Sampling(ffd1 = series.group, Cova = covariates, m0In = m0, c0In = Cova1, S0In = S0, 
                                                                                         beta0In = Beta1, nt0In = n0, flag1 = 0, NIn = N1, NS = Ngroup, Nsimu = Nsimu1,
                                                                                         CUTpos = Cutpos)
                                            attr(res, "class") <- "fMRI_group_single_voxel"
                                            return(res)
                                            
                                            
                                            
                                          }
  }else{
    
    
    
    Ngroup <- length(DatabaseGroup)
    #THIS LINE RETURN THE POSITIONS OF EACH VOXEL INSIDE THE CLUSTER GIVEN THE DISTANCE r1
    posi1 <- .distanceNeighbors(posi.refer = posi.ffd, r1)
    aux.pos <- dim(DatabaseGroup[[1]])[1:3]
    row_sub1 <- apply(posi1, 1, function(row, x1){0 < row & row<=x1}, x1=aux.pos)
    
    posi <- posi1[apply(t(row_sub1), 1, sum)==3, ]
    
    
    
    #BOLD RESPONSE SERIES FOR A SPECIFIC CLUSTER
    series.group = NULL 
    for(i in 1:Ngroup){
      ffd.c <- DatabaseGroup[[i]]
      series <- sapply(1:dim(posi)[1], function(ii){ffd.c[posi[ii,1], posi[ii,2], posi[ii,3], ]})
      series.group <- rbind(series.group, series)
    }
    
    
    if(any(series.group[,1]==0)){return( NA )}else{
                                                
                                                flag <- any(series.group==0)
                                                Cova1 <- diag(rep(Cova, dim(covariates)[2]))
                                                delta1 <- sqrt(delta)
                                                Beta1 <- diag(1/c(rep(delta1, dim(covariates)[2])))
                                                
                                                res   <- .Group_Functional_Equation(ffd1 = series.group, Cova = covariates, m0In = m0, c0In = Cova1, S0In = S0, 
                                                                                   beta0In = Beta1, nt0In = n0, flag1 = flag, NIn = N1, NS = Ngroup, Nsimu = Nsimu1,
                                                                                   CUTpos = Cutpos)
                                                attr(res, "class") <- "fMRI_group_single_voxel"
                                                return( res )
                                                
                                                
                                                
                                              }  
    
  }
  
  
  
  
}
