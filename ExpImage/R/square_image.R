#' Transform the square image (Transforme a imagem quadrada).
#'
#' @description Transforms the image square by placing borders at the edges
#' (Transforma a imagem quadrada colocando bordas nas margens).

#' @usage square_image(im,r=1,g=1,b=1,plot=FALSE)

#' @param im    :Image in ExpImage format (Imagem no formato do ExpImage).
#' @param r    :Value from 0 to 1 corresponding to the Red band (Valor de 0 a 1 correspondente a banda de Vermelho).
#' @param g    :Value from 0 to 1 corresponding to the Green band (Valor de 0 a 1 correspondente a banda de Verde).
#' @param b    :Value from 0 to 1 corresponding to the Blue band (Valor de 0 a 1 correspondente a banda de Azul).


#' @param plot    :TRUE if you want to see the image resized (TRUE se desejar ver a imagem redimensionada).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the resolution in DPI of an image. (Retorna a resolução em uma imagem).
#' @seealso  \code{\link{pick_color}} , \code{\link{pick_length}}

#' @examples
#\donttest{
#'im=read_image(example_image(10),plot=TRUE)
#'square_image(im,r=0,g=0,b=1,plot=TRUE)
#}
#'
#'@export
#'
square_image=function(im,r=1,g=1,b=1,plot=FALSE){

  len=info_image(im)$Length
  n=max(len[1],len[2])
  mat=array(data = NA,dim = c(n,n,len[3]))
  mat[,,1]=r ; mat[,,2]=g ; mat[,,3]=b

  if(len[1]>len[2]){
    ni=ceiling((n-len[2])/2)
    mat[,ni:(ni+len[2]-1),]=im@.Data
  }

  if(len[2]>len[1]){
    ni=ceiling((n-len[1])/2)
    mat[ni:(ni+len[1]-1),,]=im@.Data
  }

  im@.Data=mat

  if(plot==TRUE){plot_image(im)}
return(im)
}

