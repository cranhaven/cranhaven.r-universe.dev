#' Image information  (Informacoes sobre as imagens).
#'
#' @description This function returns information about the image
#' (Esta funcao retorna informacoes sobre a imagem).
#' @usage info_image(im)

#' @param im    :Object containing an image (Objeto contendo a uma imagem).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Importa uma imagem.
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'im=read_image(example_image(1),plot=TRUE)
#'info_image(im)
#}
#'@export
#' @exportS3Method print info_image

info_image=function(im){
Class=c(class(im))
Length=dim(im)
Pixels=prod(Length)
MegaPixels=Pixels/1000000
Mode=mode(im)
Size=round(c(utils::object.size(im)/1024^2),5)
names(Size)="MB"
#print("----------------------------------------------")
res=list(Class=Class,Length=Length,MegaPixels=MegaPixels,Mode=Mode,SizeMemory=Size)
class(res)="info_image"
return(res)
}

print.info_image=function(x, ...){
  res=x
  RES=data.frame(unlist(res),nrow=length(unlist(res)))
  RES[,2]=NULL
  colnames(RES)=NULL
  print(RES)
}

