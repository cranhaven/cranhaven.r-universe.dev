#'Funcao para acrescentar bordas nos objetos.
#'@description Esta funcao permite acrescentar as bordas em objetos nas imagens.
#'@usage dilate_image(im,n=1,plot=TRUE)
#'@param im Este objeto deve conter uma imagem binaria em uma matriz.
#'@param n Numero de vezes que deseja-se a aumentar as bordas.
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada.
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem acrescentando-se bordas.
#'@seealso  \code{\link{edit_image}} ,  \code{\link{erode_image}}


#'@export
#' @examples
#' #Abrindo uma imagem de exemplo
#' im=read_image(example_image(10),plot=TRUE)
#' b=gray_scale(im,method = "b",plot=TRUE)
#' raiz=segmentation(b,threshold = 0.7,selectHigher = TRUE,plot=TRUE)
#' raiz2=dilate_image(raiz,n = 4,plot = TRUE)
#' raiz3=dilate_image(raiz,n = 8,plot = TRUE)
#'
#'
#'@export
#'
#'@exportS3Method print dilate_image
#'
dilate_image=function(im,n=1,plot=TRUE){
  m2=im

  for(i in 1:n){
    m2=EBImage::dilate(m2)
  }

  if(isTRUE(plot)){plot_image(m2)}
  return(m2)
}



print.dilate_image=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}
