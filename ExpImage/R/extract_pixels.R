#'Function to exclude pixels from the background or foreground in an image
#'(Funcao para excluir em uma imagem os pixels correspondente ao background ou
#'foreground)
#'
#'@description Function to exclude pixels from the background or foreground in
#'  an image(Esta funcao permite criar uma imagem excluindo os pixels
#'  correspondente ao background ou foreground).
#'@usage extract_pixels(im,target,valueTarget=TRUE,
#'  valueSelect=c(r=1,g=1,b=1),plot=FALSE)
#'@param im    :This object must contain an image in EBImage format (Este objeto
#'  deve conter uma imagem no formato do EBImage).
#'@param target    : This object must be a binary array, containing the values 0
#'  (background pixels) or 1 (foreground pixels) (Este objeto deve ser
#'  obrigatoriamente uma matriz binaria, contendo os valores 0 (pixels do
#'  background) ou 1 (pixels do foreground)).

#' @param valueTarget    :Must receive the value 0 or 1 depending on what will
#'   be extracted from the image (background or foreground) (Deve receber o
#'   valor 0 ou 1 a depender do que sera extraido da imagem (background ou
#'   foreground)).
#' @param valueSelect    :It must be a vector with three values ranging from 0
#'   to 1. These values respectively indicate the values of r, g and b that will
#'   replace the unwanted pixels in the image (Deve ser um vetor com tres
#'   valores variando entre 0 a 1. Estes valores indicam reespectivamente os
#'   valores de r, g e b que substituirao os pixels indesejados na imagem).
#' @param plot    :Indicates whether the image will be displayed (TRUE) or not
#'   (FALSE) (default) (Indica se sera apresentada (TRUE) ou nao (FALSE)
#'   (default) a imagem segmentada).

#' @return Returns an image with the color indicated in the valueSelect variable
#'   over the unwanted pixels (Retorna uma imagem com a cor indicada na variavel
#'   valueSelect sobre os pixels indesejaveis).
#' @seealso  \code{\link{segmentation_logit}}


#' @examples
#' \donttest{
#'###########################################################################
#'#Estimar a area atacada por doenca no tomateiro
#'###########################################################################
#'
#'   im=read_image(example_image(ex=7),plot=TRUE)
#'
#'
#'   #Selecionando o melhor indice para a segmentacao da folha
#'   r=gray_scale(im,method = "r",plot=TRUE)
#'   g=gray_scale(im,method = "g",plot=TRUE)
#'   b=gray_scale(im,method = "b",plot=TRUE)
#'
#'   #O limiar pode ser um valor escolhido aleatoriamente
#'   MatrizSegentada=segmentation(b,threshold = 0.5,fillHull = FALSE,plot=TRUE)
#'

#'   #O limiar tambem pode ser estabelecido pelo metodo de otsu
#'   MatrizSegentada2=segmentation(b,threshold = "otsu",fillHull = TRUE,selectHigher
#'   = FALSE,plot=TRUE)
#'
#'   #Selecionar na imagem apenas os pixeis desejaveis (Folha)
#'   im2=extract_pixels(im,target=MatrizSegentada2,valueTarget=TRUE,
#'   valueSelect=c(r=1,g=1,b=1),plot=TRUE)
#'
#'   #####################################################################
#'   #####################################################################
#'   #Selecionando o melhor indice para a segmentacao da doenca
#'   r=gray_scale(im2,method = "r",plot=TRUE)
#'    g=gray_scale(im2,method = "g",plot=TRUE)
#'   b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'   MatrizSegmentada3=segmentation(g,threshold = 0.3,selectHigher = FALSE,
#'   fillHull =TRUE,plot=TRUE)
#'
#'
#'   #Como pode-se obsevar, a segmentacao por limiar nao e possivel. Entao vamos
#'   #usar paletas de cores
#'   folha=read_image(example_image(ex=8))
#'   doenca=read_image(example_image(ex=9))
#'
#'   DoencaSeg=segmentation_logit(im,foreground = doenca,background =
#'   folha,sample = 2000,fillHull = TRUE,TargetPixels =MatrizSegentada2==1
#'   ,plot=TRUE)
#'
#'   im3=mask_pixels(im=im2,TargetPixels=DoencaSeg==1,col="red",plot=TRUE)
#'
#'   ii=join_image(im,im3,plot=TRUE)
#'
#'
#'   #Porcentagem da area lesionada.
#'
#'   100*(sum(DoencaSeg)/sum(MatrizSegentada2))
#' }
#'@export
#' @exportS3Method print extract_pixels



extract_pixels=function(im,target,valueTarget=TRUE,valueSelect=c(r=1,g=1,b=1),plot=FALSE){
  id=target!=valueTarget

  r=im@.Data[,,1]
  g=im@.Data[,,2]
  b=im@.Data[,,3]

  r[id]=valueSelect[1]
  g[id]=valueSelect[2]
  b[id]=valueSelect[3]

  im@.Data[,,1]=r
  im@.Data[,,2]=g
  im@.Data[,,3]=b

  if(plot==T){plot_image(im)}
  return(im)
}


print.extract_pixels=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}
