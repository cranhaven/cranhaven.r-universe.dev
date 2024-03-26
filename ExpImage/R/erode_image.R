#'Funcao para eliminar as bordas dos objetos.
#'@description Esta funcao permite excluir as bordas de objetos nas imagens.
#'@usage erode_image(im,n=1,plot=TRUE)
#'@param im Este objeto deve conter uma imagem binaria em uma matriz.
#'@param n Numero de vezes que deseja-se a exclusao das bordas.
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada.
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem excluindo-se suas bordas.
#'@seealso  \code{\link{edit_image}} ,  \code{\link{dilate_image}}


#'@export
#' @examples
#' #Abrindo uma imagem de exemplo
#' im=read_image(example_image(10),plot=TRUE)
#' b=gray_scale(im,method = "b",plot=TRUE)
#' raiz=segmentation(b,threshold = 0.62,selectHigher = TRUE,plot=TRUE)
#' raiz2=erode_image(raiz,n = 1,plot = TRUE)
#'
#'@export
#'
#' @exportS3Method print erode_image
erode_image=function(im,n=1,plot=TRUE){
  m2=im

  for(i in 1:n){
    m2=EBImage::erode(m2)
  }

  if(isTRUE(plot)){plot_image(m2,col=0)}
  return(m2)
}


print.erode_image=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}

