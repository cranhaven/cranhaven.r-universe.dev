#'Contorno da imagem segmentada (Outline of the segmented image.)
#'@description Esta funcao retorna o contorno da imagem segmentada
#'(This function returns the outline of the segmented image).
#'
#'@usage contour_image(img,plot=TRUE)
#'@param img Este objeto deve conter uma imagem contida em uma matriz binaria.
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default)).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna o contorno de objetos em uma imagem binaria (Returns the outline of objects in a binary image ).
#'@seealso  \code{\link{edit_image}} ,  \code{\link{edit_imageGUI}}

#'@export
#' @examples
#' end=example_image(3)
#' im=read_image(end,TRUE)
#' b=gray_scale(im = im,method = "g",plot = TRUE)
#' m=segmentation(b,threshold = "otsu",selectHigher = FALSE,fillHull = TRUE,plot=TRUE)
#' contour_image(m)
#'
#'
#' @exportS3Method print contour_image
# @import EBImage
# @importFrom EBImage bwlabel combine dilate display erode is.Image readImage


contour_image=function(img,plot=TRUE){
Contorno2=function(img2,imagem=TRUE){
  t=img2
  c=ncol(t)
  t2=(t[,-1]+t[,-c])/2
  t3=(t2!=0) & (t2!=1)
  t[,-1]=t3
  return((t))
}
img=as.matrix(img)
m2=Contorno2(img2 = img)
m3=Contorno2(img2 = t(img))

m4=m2+t(m3)
m5=1*(m4!=0)
m5=EBImage::as.Image(m5)
if(plot==TRUE) {plot(m5)}
m5
}




print.contour_image=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}


