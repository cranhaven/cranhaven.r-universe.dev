#######################################################################################
###############
############### AntMAN Package : Tests and Examples
###############
###############
#######################################################################################

quit() ## Skip this test - too long
pdf(file.path(tempdir(), "segmentation.pdf"))
##############################################
### Load packages
##############################################
library("AntMAN")


set.seed(123)

##############################################
### Functions
##############################################

### Load image and generate dataset
LoadAndGenerate <- function(img_path) {
  library(jpeg)
  # Import the image in R
  img <- readJPEG(img_path) # Read the image
  
  ## We can plot the image with the command rasterImage
  plot(1:2, type='n')
  rasterImage(img, 1.01, 1.01, 1.99, 1.99)
  
  # Obtain the dimension
  imgDm <- dim(img)
  
  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    x = rep(1:imgDm[2], each = imgDm[1]),
    y = rep(imgDm[1]:1, imgDm[2]),
    R = as.vector(img[,,1]),
    G = as.vector(img[,,2]),
    B = as.vector(img[,,3])
  )
  
  x <- imgRGB[,c(3,4,5)]
  
  return (list(pic = x, dim = imgDm))
}
DrawMat <- function(pic,imgDm) {
  img_seg <- array(dim=imgDm)
  img_seg[,,1] <- matrix(pic[,1],nrow=imgDm[1],ncol=imgDm[2])
  img_seg[,,2] <- matrix(pic[,2],nrow=imgDm[1],ncol=imgDm[2])
  img_seg[,,3] <- matrix(pic[,3],nrow=imgDm[1],ncol=imgDm[2])
  plot(1:2, type='n')
  rasterImage(img_seg, 1.01, 1.01, 1.99, 1.99)
}

DrawResult <- function(mat,imgDm,clusters) {
  clusters = clusters / max(clusters)
  img_seg <- array(dim=imgDm)

  img_seg[,,1] <- matrix(clusters,nrow=imgDm[1],ncol=imgDm[2])
  img_seg[,,2] <- matrix(clusters,nrow=imgDm[1],ncol=imgDm[2])
  img_seg[,,3] <- matrix(clusters,nrow=imgDm[1],ncol=imgDm[2])
  plot(1:2, type='n')
  rasterImage(img_seg, 1.01, 1.01, 1.99, 1.99)
  
  
  
  
}

data(brain)
imgDm <- brain$dim
x = brain$pic
bn <- imgDm[1] * imgDm[2]
mat <- matrix(0,bn,3)
mat[,1 ] <-x$R
mat[,2 ] <-x$G
mat[,3 ] <-x$B
### scatter3D(x=mat[,1],y=mat[,2],z=mat[,3])

DrawMat(mat,imgDm)


mixture_mvn_params = AM_mix_hyperparams_multinorm   (mu0=c(0,0,0),ka0=.1,nu0=5,Lam0=0.1*diag(3))

mcmc_params        = AM_mcmc_parameters(niter=10000, burnin=5000, thin=5, verbose=0, output = c("CI","K","M"))
components_prior   = AM_mix_components_prior_pois (init=5,a=10,b=2) 
weights_prior      = AM_mix_weights_prior_gamma(init=2, a=1, b=1)


set.seed(523)
fit <- AM_mcmc_fit(
  init_K = 1,
  y = mat, 
  mix_kernel_hyperparams = mixture_mvn_params,
  mix_components_prior =components_prior,
  mix_weight_prior = weights_prior,
  mcmc_parameters = mcmc_params)

summary (fit)

##### These take too long, Raffa you are right (as always), 
##### we need to implement these in C++/OpenMP, or even better to just precompute the stuff
### plot (fit)
### AM_clustering_estimation_laugreen(fit)

