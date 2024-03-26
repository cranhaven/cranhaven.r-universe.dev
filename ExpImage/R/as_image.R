#' Transform matrix into image (Transformar matriz em imagem).
#'
#' @description This function transform matrix into image  (Esta funcao transforma matriz em imagem).
#' @usage as_image(img)

#' @param img    :Object image  (Objeto com uma imagem).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns an object of type image (Retorna um objeto do tipo imagem).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#'m=matrix(sort(runif(10000)),ncol=100,nrow=100)
#'m
#'info_image(m)
#'im=as_image(m)
#'im
#'info_image(im)
#'plot_image(m)

#'@export


as_image=function(img){
  EBImage::as.Image(img)
}
