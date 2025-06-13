#' @title 3D Volume Gradient
#' @description This function returns the gradient images for a 3D array or NIfTI volume.
#' @param image a 3D array or image of class \code{\link{nifti}}
#' @param mask an array or \code{\link{nifti}} mask of voxels for which the gradient will be calculated,
#' if \code{NULL} the gradient will be run for the full array.
#' Note that mask should be in the same space as the image volume
#' @param which a string specifying the gradient direction that should be returned; either "all" for a list of x, y, and z gradient volumes, or "x", "y", or "z" for a single volume with the given gradient
#' @param radius an integer specifying radius of the neighborhood (in voxels) for which the gradient should be calculated
#'
#' @return Either a list of three gradient volumes or a single gradient volume, in either array or NIfTI format based on what was input.
#' @examples \dontrun{
#' library(neurobase)
#' epi <- readnii('path/to/epi')
#' gradients <- gradient3D(image = epi, which = "all") }
#' @export
#' @importFrom oro.nifti is.nifti
gradient3D=function(image, mask = NULL, which = "all", radius = 1){
  if(radius>=min(dim(image))){stop("Radius larger than smallest image dimension")}
  if(is.nifti(image)){
    if(which=="all"){
      dx=image
      dx@.Data[1:radius,,]=0
      dx@.Data[dim(image)[1]-radius+1,,]=0
      dx@.Data[((1+radius):(dim(image)[1]-radius)),,]=
        (image@.Data[((1+2*radius):dim(image)[1]),,]-
           image@.Data[(1:(dim(image)[1]-2*radius)),,])/(2*radius)
      dx@.Data[mask@.Data==0]<-0

      dy=image
      dy@.Data[,1:radius,]=0
      dy@.Data[,dim(image)[2]-radius+1,]=0
      dy@.Data[,((1+radius):(dim(image)[2]-radius)),]=
        (image@.Data[,((1+2*radius):dim(image)[2]),]-
           image@.Data[,(1:(dim(image)[2]-2*radius)),])/(2*radius)
      dy@.Data[mask@.Data==0]<-0

      dz=image
      dz@.Data[,,1:radius]=0
      dz@.Data[,,dim(image)[3]-radius+1]=0
      dz@.Data[,,((1+radius):(dim(image)[3]-radius))]=
        (image@.Data[,,((1+2*radius):dim(image)[3])]-
           image@.Data[,,(1:(dim(image)[3]-2*radius))])/(2*radius)
      dz@.Data[mask@.Data==0]<-0

      return(list(Dx=dx,Dy=dy,Dz=dz))
    }else if(which=="x"){
      dx=image
      dx@.Data[1:radius,,]=0
      dx@.Data[dim(image)[1]-radius+1,,]=0
      dx@.Data[((1+radius):(dim(image)[1]-radius)),,]=
        (image@.Data[((1+2*radius):dim(image)[1]),,]-
           image@.Data[(1:(dim(image)[1]-2*radius)),,])/(2*radius)
      dx@.Data[mask@.Data==0]<-0
      return(dx)
    }else if(which=="y"){
      dy=image
      dy@.Data[,1:radius,]=0
      dy@.Data[,dim(image)[2]-radius+1,]=0
      dy@.Data[,((1+radius):(dim(image)[2]-radius)),]=
        (image@.Data[,((1+2*radius):dim(image)[2]),]-
           image@.Data[,(1:(dim(image)[2]-2*radius)),])/(2*radius)
      dy@.Data[mask@.Data==0]<-0
      return(dy)
    }else if(which=="z"){
      dz=image
      dz@.Data[,,1:radius]=0
      dz@.Data[,,dim(image)[3]-radius+1]=0
      dz@.Data[,,((1+radius):(dim(image)[3]-radius))]=
        (image@.Data[,,((1+2*radius):dim(image)[3])]-
           image@.Data[,,(1:(dim(image)[3]-2*radius))])/(2*radius)
      dz@.Data[mask@.Data==0]<-0
      return(dz)
    }
  }else if(is.array(image)){
    if(which=="all"){
      dx=image
      dx[1:radius,,]=0
      dx[dim(image)[1]-radius+1,,]=0
      dx[((1+radius):(dim(image)[1]-radius)),,]=
        (image[((1+2*radius):dim(image)[1]),,]-
           image[(1:(dim(image)[1]-2*radius)),,])/(2*radius)
      dx[mask==0]<-0

      dy=image
      dy[,1:radius,]=0
      dy[,dim(image)[2]-radius+1,]=0
      dy[,((1+radius):(dim(image)[2]-radius)),]=
        (image[,((1+2*radius):dim(image)[2]),]-
           image[,(1:(dim(image)[2]-2*radius)),])/(2*radius)
      dy[mask==0]<-0

      dz=image
      dz[,,1:radius]=0
      dz[,,dim(image)[3]-radius+1]=0
      dz[,,((1+radius):(dim(image)[3]-radius))]=
        (image[,,((1+2*radius):dim(image)[3])]-
           image[,,(1:(dim(image)[3]-2*radius))])/(2*radius)
      dz[mask==0]<-0

      return(list(Dx=dx,Dy=dy,Dz=dz))
    }else if(which=="x"){
      dx=image
      dx[1:radius,,]=0
      dx[dim(image)[1]-radius+1,,]=0
      dx[((1+radius):(dim(image)[1]-radius)),,]=
        (image[((1+2*radius):dim(image)[1]),,]-
           image[(1:(dim(image)[1]-2*radius)),,])/(2*radius)
      dx[mask==0]<-0
      return(dx)
    }else if(which=="y"){
      dy=image
      dy[,1:radius,]=0
      dy[,dim(image)[2]-radius+1,]=0
      dy[,((1+radius):(dim(image)[2]-radius)),]=
        (image[,((1+2*radius):dim(image)[2]),]-
           image[,(1:(dim(image)[2]-2*radius)),])/(2*radius)
      dy[mask==0]<-0
      return(dy)
    }else if(which=="z"){
      dz=image
      dz[,,1:radius]=0
      dz[,,dim(image)[3]-radius+1]=0
      dz[,,((1+radius):(dim(image)[3]-radius))]=
        (image[,,((1+2*radius):dim(image)[3])]-
           image[,,(1:(dim(image)[3]-2*radius))])/(2*radius)
      dz[mask==0]<-0
      return(dz)
    }
  }else{
    print("Image must be array or NifTI")
  }
}
