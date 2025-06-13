#' @title 3D Volume Hessian
#' @description This function returns the eigenvalues of the hessian matrices for a 3D array or NIfTI volume.
#' @param image a 3D array or image of class \code{\link{nifti}}
#' @param mask an array or \code{\link{nifti}} mask of voxels for which vesselness will be calculated,
#' with more selective masking improving speed significantly.
#' Note that mask should be in the same space as the image volume
#' @param radius an integer specifying radius of the neighborhood (in voxels) for which the hessian should be calculated
#' @param parallel is a logical value that indicates whether the user's computer
#' is Linux or Unix (i.e. macOS), and should run the code in parallel
#' @param cores if parallel = TRUE, cores is an integer value that indicates how many cores
#' the function should be run on
#'
#' @return A list of three eigenvalue volumes.
#' @examples \dontrun{
#' library(neurobase)
#' epi <- readnii('path/to/epi')
#' mask <- epi!=0
#' hesseigs <- hessian3D(image = epi, mask = mask) }
#' @export
#' @importFrom pbmcapply pbmclapply
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores
hessian3D=function(image, mask, radius = 1, parallel = FALSE, cores = 2){

  print("Getting derivatives")
  grads=gradient3D(image,which="all",radius=radius)
  gx=grads$Dx
  gy=grads$Dy
  gz=grads$Dz
  rm(grads)

  gradsx=gradient3D(gx,which="all",radius=radius)
  gxx=gradsx$Dx
  gxy=gradsx$Dy
  gxz=gradsx$Dz
  rm(gx,gradsx)

  gradsy=gradient3D(gy,which="all",radius=radius)
  gyx=gradsy$Dx
  gyy=gradsy$Dy
  gyz=gradsy$Dz
  rm(gy,gradsy)

  gradsz=gradient3D(gz,which="all",radius=radius)
  gzx=gradsz$Dx
  gzy=gradsz$Dy
  gzz=gradsz$Dz
  rm(gz,gradsz)

  print("Creating hessian matrices")
  bigmat=cbind(as.vector(gxx[mask==1]),as.vector(gxy[mask==1]),as.vector(gxz[mask==1]),
               as.vector(gyx[mask==1]),as.vector(gyy[mask==1]),as.vector(gyz[mask==1]),
               as.vector(gzx[mask==1]),as.vector(gzy[mask==1]),as.vector(gzz[mask==1]))

  rm(gxx,gxy,gxz,gyx,gyy,gyz,gzx,gzy,gzz)

  biglist=split(bigmat,row(bigmat))
  biglist=lapply(biglist,matrix,nrow=3,byrow=T)

  rm(bigmat)

  getevals=function(matrix){
    thiseig=eigen(matrix)$values
    sort=order(abs(thiseig))
    return(thiseig[sort])
  }

  print("Calculating eigenvalues")
  if(parallel==TRUE){
    result=matrix(unlist(pbmclapply(biglist,getevals,mc.cores=cores)),
                  ncol=3,byrow=T)
  }else if(parallel==FALSE){
    result=matrix(unlist(pblapply(biglist,getevals)),ncol=3,byrow=T)
  }
  e1=mask
  e1[mask==1]<-result[,1]
  e2=mask
  e2[mask==1]<-result[,2]
  e3=mask
  e3[mask==1]<-result[,3]

  return(list(eigval1=e1,eigval2=e2,eigval3=e3))
}
