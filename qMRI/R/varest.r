#
#
#      estimate variance parameter in a multicoil system
#
#
awslsigmc <- function(y,                 # data 
                      steps,             # number of iteration steps for PS
                      mask = NULL,              # data mask, where to do estimation
                      ncoils = 1,        # number of coils for parallel MR image acquisition
                      vext = c( 1, 1),   # voxel extensions
                      lambda = 5,       # adaptation parameter for PS
                      minni = 2,         # minimum sum of weights for estimating local sigma
                      hsig = 5,          # bandwidth for median smoothing local sigma estimates
                      sigma = NULL,
                      family = c("NCchi"),
                      verbose = FALSE,
                      trace = FALSE,
                      u=NULL#,
                      #bc=FALSE # bias correction ...
) {
  if(is.null(mask)){
     mask <- array(TRUE, dim(y))
     img <- y
  } else if(length(y) == sum(mask)) {
     img <- array(0, img)
     img[mask] <- y
  }
  ## Code has been moved to aws package
  ## function from aws is called for compatibility reasons
  aws::awsLocalSigma(img, steps, mask, ncoils, vext,
                     lambda, minni, hsig, sigma, family, verbose, trace, u)
}


#
#
#      estimate variance parameter in a multicoil system
#
#
awssigmc <- function(y,                 # data
                     steps,             # number of iteration steps for PS
                     mask = NULL,       # data mask, where to do estimation
                     ncoils = 1,        # number of coils for parallel MR image acquisition
                     vext = c( 1, 1),   # voxel extensions
                     lambda = 20,       # adaptation parameter for PS
                     h0 = 2,            # initial bandwidth for first step in PS
                     verbose = FALSE,
                     sequence = FALSE,  # return estimated sigma for intermediate steps of PS?
                     hadj = 1,          # adjust parameter for density() call for mode estimation
                     q = .25,  # for IQR
                     qni = .8,
                     method=c("VAR","MAD")  # for variance, alternative "MAD" for mean absolute deviation
) {
  if(is.null(mask)){
    mask <- array(TRUE, dim(y))
    img <- y
  } else if(length(y) == sum(mask)) {
    img <- array(0, img)
    img[mask] <- y
  }
  method <- switch(method,"VAR"="awsVar","MAD"="awsMAD")
  aws::estGlobalSigma(img, mask, ncoils, steps, vext, lambda, h0,
                      hadj, q, qni, sequence=sequence, method=method)
}
