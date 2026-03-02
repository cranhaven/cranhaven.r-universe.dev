#' @name .get_n_cores
#' @title .get_n_cores
#' @description
#' return the number of cores.
#' @param Ncores numer of cores
#' @keywords internal
.get_n_cores  <- function(Ncores) {

    # check ncores
    if(!is.null(Ncores)) {
        
        if(is.na(Ncores)) {
            Ncores  <-  1
        }

        if(!is.numeric(Ncores)) {
            stop("The number of cores must be numeric")
        }

        Ncores  <- as.integer(Ncores)
        
        if(Ncores < 1) {
            stop("The number of cores must be greater than zero")
        }
    }

    return(Ncores)

}

#' @name .check_ffdgroup
#' @title .check_ffdgroup
#' @description
#' validate ffdGroup
#' @param ffdGroup group
#' @keywords internal
.check_ffdgroup  <- function(ffdGroup) {

    if(!is.list(ffdGroup)) {
        stop("ffdGroup must be a list.")
    }

    for(i in 1:length(ffdGroup)) {

        ffdc  <- ffdGroup[[i]]

        # Check ffdc
        if(!is.null(ffdc)) {

            if(!is.array(ffdc)) {
                stop("all elements of ffdGroup must be an array.")
            }

            if(length(dim(ffdc)) != 4 ) {
            
                stop("all elements of ffdGroup must be an 4D array.")
            }

        }



    }

}

#' @name .validate_input
#' @title .validate_input
#' @description
#' validate input
#' @param N1 is the number of images (2<N1<T) from the ffdc array employed in the model fitting.N1=NULL (or equivalently N1=T) is its default value, taking all the images in the ffdc array for the fitting process.
#' @param Test test type either "LTT" (Average cluster effect) or "JointTest" (Joint effect).
#' @param Nsimu1 is the number of simulated on-line trajectories related to the state parameters. These simulated curves are later employed to compute the posterior probability of voxel activation.
#' @param ffdc a 4D array (ffdc[i,j,k,t]) that contains the sequence of MRI images that are meant to be analyzed. (i,j,k) define the position of the observed voxel at time t.
#' @param covariates a data frame or matrix whose columns contain the covariates related to the expected BOLD response obtained from the experimental setup.
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @param delta a discount factor related to the evolution variances. Recommended values between 0.85<delta<1. delta=1 will yield results similar to the classical general linear model.
#' @param perVol helps to define a threshold for the voxels considered in the analysis. For example, Min.vol = 0.10 means that all the voxels with values
#' below to max(ffdc)*perVol can be considered irrelevant and discarded from the analysis.
#' @param Min.vol helps to define a threshold for the voxels considered in
#' the analysis. For example, Min.vol = 0.10 means that all the voxels with values
#' below to max(ffdc)*Min.vol can be considered irrelevant and discarded from the analysis.
#' @param n0 a positive hyperparameter of the prior distribution for the covariance matrix S0 at t=0 (n=1 is the default value when no prior information is available). For the case of available prior information, n0 can be set as n0=np, where np is the number of MRI images in the pilot sample.
#' @param Cutpos1 a cutpoint time from where the on-line trajectories begin. This parameter value is related to an approximation from a t-student distribution to a normal distribution. Values equal to or greater than 30 are recommended (30<Cutpos1<T).  
#' @param ffdGroup group
#' @keywords internal
.validate_input  <- function(N1=NULL, Test=NULL, Nsimu1=NULL,
                            ffdc=NULL, covariates=NULL,
                            r1 = NULL, delta=NULL, perVol=NULL,Min.vol=NULL,
                            n0 = NULL, Cutpos1=NULL, ffdGroup=NULL) {


  # .check_ffdgroup
  if(!is.null(ffdGroup)) {
    .check_ffdgroup(ffdGroup)
  }

  # check perVol
  if(!is.null(n0)) {

    if(n0 < 0 ) {
        stop("n0 must be non-negative" )
    }

  }
  
  # check delta
  if(!is.null(delta)) {

    if( (delta >= 1)  | (delta <= 0) ) {
        stop("The discount factor, delta must between 0 and 1" )
    }

  }

  # check perVol
  if(!is.null(perVol)) {

    if(perVol < 0 ) {
        stop("The threshold for the voxels, perVol must be non-negative" )
    }

  }

  # check Min.vol
  if(!is.null(Min.vol)) {

    if(Min.vol < 0 ) {
        stop("The threshold for the voxels, Min.vol must be non-negative" )
    }

  }

  # check r1
  if(!is.null(r1)) {

    if(r1 < 0 ) {
        stop("r1 must be non-negative" )
    }

  }

  

  # check Cutpos1
  if(!is.null(Cutpos1)) {

    if(Cutpos1 < 0 ) {
        stop("the cutpoint, Cutpos1 must be non-negative" )
    }

  }

  # check N1
  if(!is.null(N1)) {

    
    if( !(N1 > 2) ) {
        stop("The number of images must be grater than 2")
    }

  }

   # check Nsimu1
  if(!is.null(Nsimu1)) {

    
    if( !(Nsimu1 > 2) ) {
        stop("The  numbersimulations must be grater than 2")
    }

  }

  # check test type
  if(!is.null(Test)) {

    
    if(!(Test %in% c("LTT", "JointTest"))) {
        stop("Test must be LTT or JointTest")
    }

  }

  
  
  # Check ffdc
  if(!is.null(ffdc)) {

    if(!is.array(ffdc)) {
        stop("ffdc must be an array.")
    }

    if(length(dim(ffdc)) != 4 ) {
    
        stop("ffdc must be a 4D array.")
    }

  }

  # check covariates
  if(!is.null(covariates)) {

    if(!is.data.frame(covariates)) {
        stop("covariates must be a dataframe.")
    }

    if(length(dim(covariates)) != 2) {
        stop("covariates must be a 2D array.")
    }

  }


  

}