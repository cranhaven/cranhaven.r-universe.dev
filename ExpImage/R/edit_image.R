#' This function changes the brightness, contrast and makes the gamma adjustment
#' in the image (Esta funcao permite mudar o brilho, contraste e fazer um ajuste
#' gamma na imagem).
#'
#' @description This function changes the brightness, contrast and makes the
#'   gamma adjustment in the image (Esta funcao permite mudar o brilho,
#'   contraste e fazer um ajuste gamma na imagem).
#' @usage edit_image(im,brightness=0,contrast=1,gamma =1,plot=T)

#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param brightness    : numerical value between -1 and 1 corresponding to the
#'   desired change in brightness (valor numerico entre -1 e 1 correspondente a
#'   alteracao desejada para o brilho).
#' @param contrast    : numerical value corresponding to the desired change in
#'   contrast (valor numerico  correspondente a alteracao desejada para o
#'   contraste).
#' @param gamma    :numerical value between -1 and 1 corresponding to the
#'   desired change for the gamma adjustment  (valor numerico entre -1 e 1
#'   correspondente a alteracao desejada para o ajuste gamma).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)


#' @param plot    :Indicates whether the image will be displayed (TRUE) or not
#'   (FALSE) (default) (Indica se sera apresentada (TRUE) ou nao (FALSE)
#'   (default) a imagem editada).

#' @return Returns a cropped image with selected pixels(Retorna uma imagem cortada, apresentando apenas os  pixels
#'   selecionados).
#' @seealso  \code{\link{edit_imageGUI}}

#' @examples
#'\donttest{
#'
#'#Carregar imagem de exemplo
#'im=read_image(example_image(1))
#'##mostrar imagem
#'plot_image(im)
#'
#'
#'##Diminuir a resolucao (tamanho da imagem)
#'im2=resize_image(im,w=1000,plot=TRUE)
#'
#'##Cortar Imagem
#'im3=crop_image(im2,w =200:750,h=100:650,plot = TRUE)
#'
#'##Aumentar brilho
#'im4=edit_image(im3,brightness = 0.1)
#'
#'#Aumentar contraste
#'im5=edit_image(im4,contrast = 1.2)
#'
#'#Aumentar gamma
#'im6=edit_image(im5,gamma  = 1.1)
#'
#'
#'#Alterando brilho, contraste e gamma
#'imb=edit_image(im3,brightness = 0.1,contrast = 1.7,gamma  = 1.2)
#'
#'#Mostrando ambas as imagens simultaneamente.
#'im4=join_image(im3,imb)
#'}
#'
#'

#' @export
#' @exportS3Method print edit_image



edit_image=function(im,brightness=0,contrast=1,gamma =1,plot=T){
  if( EBImage::is.Image(im)){
    im@.Data=im@.Data+brightness
    im@.Data=im@.Data*contrast
    im@.Data=im@.Data^gamma
    im@.Data[im@.Data>1]=1
    im@.Data[im@.Data<0]=0
    #im=EBImage::normalize(im,ft=c(0,1))
    if(plot==T){plot_image(im)}
  }

  if(is.matrix(im)){
    im=im+brightness
    im=im*contrast
    im=im^gamma
im[im>1]=1
im[im<0]=0
   # im=EBImage::normalize(im,ft=c(0,1))
    if(plot==T){plot_image( EBImage::as.Image(im))}
  }

  return(im)
}



print.edit_image=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}
