#' Segmentacao a partir de modelo logit (Segmentation from the logit model)
#'@description Esta funcao permite a predicao para a segmentacao considerando o
#'modelo ajustado pela funcao 'segmentation_logit' (This function allows the
#' prediction for segmentation considering the model adjusted by the
#' 'segmentation_logit' function).
#' @usage predict_logit(im,modelo,fillHull=TRUE,
#'TargetPixels="all",
#'plot=TRUE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format).
#'@param modelo modelo exportado pela funcao 'segmentation_logit' (model
#'  exported by the 'segmentation_logit' function ).
#'@param fillHull    :Este argumento deve receber a palavra TRUE quando se
#'  pretende desconsiderar valores vazios dentro do foreground, caso contrario
#'  FALSE.
#'@param TargetPixels    :Quando se pretende segmentar todos os pixeis da imagem
#'  deve considerar a palavra "all" (Default). Se a segmentacao deva ser feita
#'  apenas para um conjunto de pixels, estes devem ser apresentados em uma
#'  matriz contendo o valor 1 para os pixels de interesse e 0 para os demais.
#'@param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada.
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Apresenta a imagem contida no objeto im.

#'@seealso  \code{\link{segmentation_logit}}
#'@export
#' @examples
#'
#'#Abrir imagem das folhas
#'im=read_image(example_image(3),plot=TRUE)
#'#Abrir paleta de cores do fundo
#'fundo=read_image(example_image(4),plot=TRUE)
#'#Abrir paleta de cores das folhas
#'folhas=read_image(example_image(5),plot=TRUE)
#'#Abrir paleta de cores referencia
#'ref=read_image(example_image(6),plot=TRUE)
#'
#'#Gerando modelo logit
#'#################################################################
#'Modelo=segmentation_logit(im,foreground=folhas,
#'                          background=list(fundo,ref),
#'                          return="model",plot=FALSE)
#'summary(Modelo)
#'
#'#################################################################
#'# Predicao a partir do modelo ajustado
#'
#'im2=predict_logit(im,Modelo,plot=TRUE)

#' @exportS3Method print predict_logit




predict_logit=function(im,modelo,fillHull=TRUE,
                       TargetPixels="all",
                       plot=TRUE){
  modelo1=modelo
  if(isFALSE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1]),G=c(im@.Data[,,2]),B=c(im@.Data[,,3]))
    pred1 <- round(
      suppressWarnings(predict(modelo1, newdata = imagem, type = "response"))
      , 0)
    ImagemSeg <- matrix(pred1, ncol = ncol(im@.Data[,,1]))
  }

  if(isTRUE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1][TargetPixels]),G=c(im@.Data[,,2][TargetPixels]),B=c(im@.Data[,,3][TargetPixels]))
    pred1 <- round(predict(modelo1, newdata = imagem, type = "response"), 0)
    ImagemSeg=TargetPixels*0
    ImagemSeg[TargetPixels==1]=pred1

  }


  if(fillHull==TRUE){ImagemSeg=EBImage::bwlabel(ImagemSeg);ImagemSeg=EBImage::fillHull(ImagemSeg)}


  ImagemSeg=ImagemSegf=EBImage::as.Image((ImagemSeg>0)*1)
  if(plot==T){
    mm=mask_pixels(im,ImagemSeg,Contour = T,col.TargetPixels = "red",plot = F)
    ImagemSeg=IM3(ImagemSeg)
    join_image(ImagemSeg,mm)}


  return(ImagemSegf)}




print.predict_logit=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}




