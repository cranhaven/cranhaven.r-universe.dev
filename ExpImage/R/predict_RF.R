#'Pixel classification by Random Forest methodology (Classificacao de pixels pela metodologia Random Forest)
#'
#' @description This function classifies pixels in images from a random forest model
#' (Esta funcao classifica pixels em imagens a partir de um modelo de random forest).
#' @usage predict_RF(im,model,col="rand",plot=TRUE)

#' @param im    : Image that will be segmented (Imagem que sera segmentada).
#' @param model   : Model adjusted by the "segmentation RF" function (Modelo ajustado pela funcao "segmentation_RF").
#' @param col    : Vector with the desired colors in the segmentation. If it's "rand" it will be random colors
#' (Vetor com as cores desejadas na segmentacao. Se for "rand" serao cores aleatorias).
#' @param plot    : Logical value, if TRUE, the image will be displayed
#' (Valor logico, se for TRUE a imagem sera apresentada).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the segmented image (Retorna a imagem segmentada).
#' @seealso  \code{\link{segmentation_RF}} , \code{\link{segmentation_logit}}
#'@export
#' @examples
#'
#' \donttest{
#' #Carregando imagens de exemplo
#' im=read_image(example_image(3))
#' Fundo=read_image(example_image(4))
#' Folha=read_image(example_image(5))
#' Ref=read_image(example_image(6))
#' paleta=list(Fundo=Fundo,Folha=Folha,Referencia=Ref)
#' col=c("black","green","red")
#'
#' #### Criando imagem
#' Image=segmentation_RF(im=im,
#'                       palette=paleta,
#'                       return="image",
#'                       NumMax=1000,
#'                       col=col,
#'                       seed=NULL,
#'                       norma=1,
#'                       plot=TRUE)
#'
#'
#' #Criando um modelo
#' model=segmentation_RF(im=im,
#' palette=paleta,
#' return="model",
#' NumMax=1000,
#' col=col,
#' seed=NULL,
#' norma=1,
#' plot=FALSE)
#'
#'
#' image=predict_RF(im,model,col="rand",plot=TRUE)
#'
#'}


predict_RF=function(im,model,col="rand",plot=TRUE){

  ima = linearize_image(im)[, -c(1, 2)]
  RF=predict(model,newdata=ima)
  RF2=RF1=as.character(RF)
  classes=unique(RF2)
  ncluster=length(classes)

  if (col[1] == "rand") {
    COL = grDevices::rgb(red = runif(n = ncluster, min = 0,
                                     max = 1), green = runif(n = ncluster, min = 0,
                                                             max = 1), blue = runif(n = ncluster, min = 0,
                                                                                    max = 1))}
  if (col[1] != "rand") { COL = col}




  for(i in 1:ncluster){
    RF2[RF1==classes[i]]=COL[i]
  }

  RF3=grDevices::col2rgb(c(RF2))
  r=matrix(RF3[1,],ncol=ncol(im@.Data[,,1]))
  g=matrix(RF3[2,],ncol=ncol(im@.Data[,,2]))
  b=matrix(RF3[3,],ncol=ncol(im@.Data[,,3]))

  im2=im
  im2@.Data[,,1]=r
  im2@.Data[,,2]=g
  im2@.Data[,,3]=b

  if(mean(im2@.Data)>1){im2@.Data=im2@.Data/255}

  if (plot == TRUE) {
    plot_image(im2)
  }
  return(im)
}






