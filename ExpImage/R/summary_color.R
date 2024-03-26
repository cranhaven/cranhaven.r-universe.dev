#' Summarize the colors present in the segmented area of an image
#' (Sumarizar as cores presentes na area segmentada de uma imagem).
#'
#' @description Summarize the colors present in the segmented area of an image
#' (Sumarizar as cores presentes na area segmentada de uma imagem).
#'
#' @usage summary_color(im,
#'                      segmentation,
#'                      return=c("image","summary","histogram1","histogram2","histogram3"))
#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#'@param segmentation Binary matrix obtained of a segmentation(matrix binaria obtida por uma segmentacao).
#' @param return    : Indicates the result you want to obtain from the segmented area in the image
#'   ("image","summary","histogram1","histogram2","histogram3")
#'   (Indica o resultado que se deseja obter a partir da area segmentada na imagem).
#'
#'
#' @return Returns an image with the colors present in the segmented area, measures that summarize these colors or histograms
#' (Retorna uma imagem com as cores presentes na área segmentada, medidas que resumem essas cores ou histogramas).
#' @seealso  \code{\link{segmentation} , \link{histogram_image} }
#' @export
#' @importFrom stats sd

#' @examples
#' im=read_image(example_image(3),plot=TRUE)
#' r=gray_scale(im,method = "r")
#' seg=segmentation(img.band = r,threshold = 0.3,selectHigher = FALSE,fillHull = FALSE,plot = TRUE)
#'
#' summary_color(im,seg,return = "image")
#' summary_color(im,seg,return = "summary")
#' summary_color(im,seg,return = "histogram1")
#' summary_color(im,seg,return = "histogram2")
#' summary_color(im,seg,return = "histogram3")

summary_color=function(im,segmentation,return=c("image","summary","histogram1","histogram2","histogram3")){
  Return=return[1]
  seg=segmentation
  R=im@.Data[,,1][seg==1]
  G=im@.Data[,,2][seg==1]
  B=im@.Data[,,3][seg==1]

  n=floor(sqrt(length(R)))
  id=sample(size = n*n,x = 1:(n*n),replace = F)

  RR=matrix(R[id],n,n)
  GG=matrix(G[id],n,n)
  BB=matrix(B[id],n,n)

  im2=im
  im2@.Data=array(NA,dim=c(n,n,3))
  im2@.Data[,,1]=RR
  im2@.Data[,,2]=GG
  im2@.Data[,,3]=BB

  mat=cbind(R,G,B)

  SummaryRGB=rbind(
    min=apply(mat,2,min),
    max=apply(mat,2,max),
    mean=apply(mat,2,mean),
    sd=apply(mat,2,sd),
    cv=100*apply(mat,2,mean)/apply(mat,2,sd))


  Conv=convert_color(data.frame(mat))
  mat=Conv$LAB
  SummaryLAB=rbind(
    min=apply(mat,2,min),
    max=apply(mat,2,max),
    mean=apply(mat,2,mean),
    sd=apply(mat,2,sd),
    cv=100*apply(mat,2,mean)/apply(mat,2,sd))

  mat=Conv$XYZ
  SummaryXTZ=rbind(
    min=apply(mat,2,min),
    max=apply(mat,2,max),
    mean=apply(mat,2,mean),
    sd=apply(mat,2,sd),
    cv=100*apply(mat,2,mean)/apply(mat,2,sd))

  Summary=list(SummaryRGB=SummaryRGB,SummaryLAB=SummaryLAB,SummaryXTZ=SummaryXTZ)

  if(Return=="image"){

   (plot_image(im2))
    return(im2)
  }

  if(Return=="histogram1"){ print(histogram_image(im2,layout = 1)) }
  if(Return=="histogram2"){ print(histogram_image(im2,layout = 2)) }
  if(Return=="histogram3"){ print(histogram_image(im2,layout = 3)) }

  if(Return=="summary"){ return(Summary)}

}
