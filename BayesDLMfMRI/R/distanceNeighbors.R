#' @name .distanceNeighbors 
#' @title .distanceNeighbors 
#' @description
#' this is an internal function
#' @references
#' \insertRef{CARDONAJIMENEZ2021107297}{BayesDLMfMRI}
#' 
#' \insertRef{cardona2021bayesdlmfmri}{BayesDLMfMRI}
#' @details
#' this is an internal function
#' @param posi.refer  the position of the voxel in the brain image.
#' @param r1 a positive integer number that defines the distance from every voxel with its most distant neighbor. This value determines the size of the cluster. The users can set a range of different r values: r = 0, 1, 2, 3, 4, which leads to q = 1, 7, 19, 27, 33, where q is the size of the cluster.
#' @keywords internal
.distanceNeighbors  <- function(posi.refer, r1){
  
  if(r1==0){return(rbind(posi.refer))}
  
  if(r1==1){
   res1 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(1, nrow = 3)                                                                
   res2 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(-1, nrow = 3) 
   return(rbind(posi.refer, res1, res2))
  }
  
  if(r1==2){
   res1 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(1, nrow = 3)                                                                
   res2 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(-1, nrow = 3)
   res3 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(1, 4), c(1, 0, -1, 0), c(0, 1, 0, -1))
   res4 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + cbind(c(-1, 0, 0), rep(1, 3), c(0, 1, -1))
   res5 <-  matrix(rep(posi.refer,5), byrow = TRUE, ncol=3) + cbind(c(-1, -1, -1, 0, 0), c(0, -1, 0, -1, -1), c(1, 0, -1, 1, -1))
   return(rbind(posi.refer, res1, res2, res3, res4, res5))
  }
  
  if(r1==3){
    res1 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(1, nrow = 3)                                                                
    res2 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(-1, nrow = 3)
    res3 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(1, 4), c(1, 0, -1, 0), c(0, 1, 0, -1))
    res4 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + cbind(c(-1, 0, 0), rep(1, 3), c(0, 1, -1))
    res5 <-  matrix(rep(posi.refer,5), byrow = TRUE, ncol=3) + cbind(c(-1, -1, -1, 0, 0), c(0, -1, 0, -1, -1), c(1, 0, -1, 1, -1))
    res6 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(1, 4), c(1, 1, -1, -1), c(1, -1, 1, -1))
    res7 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(-1, 4), c(1, 1, -1, -1), c(1, -1, 1, -1))
    return(rbind(posi.refer, res1, res2, res3, res4, res5, res6, res7))
  }
  
  if(r1==4){
    res1 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(1, nrow = 3)                                                                
    res2 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + diag(-1, nrow = 3)
    res3 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(1, 4), c(1, 0, -1, 0), c(0, 1, 0, -1))
    res4 <-  matrix(rep(posi.refer,3), byrow = TRUE, ncol=3) + cbind(c(-1, 0, 0), rep(1, 3), c(0, 1, -1))
    res5 <-  matrix(rep(posi.refer,5), byrow = TRUE, ncol=3) + cbind(c(-1, -1, -1, 0, 0), c(0, -1, 0, -1, -1), c(1, 0, -1, 1, -1))
    res6 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(1, 4), c(1, 1, -1, -1), c(1, -1, 1, -1))
    res7 <-  matrix(rep(posi.refer,4), byrow = TRUE, ncol=3) + cbind(rep(-1, 4), c(1, 1, -1, -1), c(1, -1, 1, -1))
    res8 <-  matrix(rep(posi.refer,6), byrow = TRUE, ncol=3) + rbind(diag(2, 3), diag(-2, 3))
    return(rbind(posi.refer, res1, res2, res3, res4, res5, res6, res7, res8))
  }
  
}


