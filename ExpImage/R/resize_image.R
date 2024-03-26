#' Function to change image size (Funcao para mudar o tamanho da imagem)
#'
#' @description Esta funcao permite mudar o tamanho de uma imagem, modificando o
#'   peso ocupado na memoria e em sua resolucao.
#' @usage resize_image(im, w=NULL, h=NULL,percentage=NULL,plot=FALSE)
#' @param im    :Este objeto deve conter uma imagem no formato do EBImage ou na
#'   forma de uma matriz.
#' @param w    : e o numero de linhas que pretende-se obter na imagem de saida.
#'   Se houver o valor de h este valor e dispensavel, caso queira manter a
#'   proporcionalidade.
#' @param h    : e o numero de colunas que pretende-se obter na imagem de
#'   saida.Se houver o valor de w este valor e dispensavel, caso queira manter a
#'   proporcionalidade.
#'
#' @param percentage : Desired percentage for image reduction (Porcentagem desejada para a reducao da imagem).
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem editada
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @return Retorna uma imagem com o tamanho desejado.
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}
#'@export
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
#' im2b=resize_image(im,percentage = 10,plot=TRUE)
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
#'join_image(im3,imb)
#'}




resize_image=function(im, w=NULL, h=NULL,percentage=NULL,plot=FALSE){

  if(im@colormode>0){
    cm=im@colormode
    im@colormode=as.integer(2)
  }

  if(!is.null(percentage)){w=round(info_image(im)$Length[1]*percentage/100,0)
  im2=EBImage::resize(im,w =w)}


  if(is.null(percentage)){
    if(!is.null(w)|!is.null(h)){
    if(is.null(w)){im2=EBImage::resize(im,h=h)}
    if(is.null(h)){im2=EBImage::resize(im,w=w)}
    }

    if(!is.null(w)&!is.null(h)){im2=EBImage::resize(im,w =w,h =h)}
  }

  if(plot==T){plot_image(im2)}

  if(im2@colormode>0){  im2@colormode=as.integer(2)}
  return(im2)
}
