#' Function to import an image (Funcao para importa uma imagem).
#'
#' @description Esta funcao importa uma imagem.
#' @usage read_image(file,plot=FALSE,norm=1)

#' @param file    :Nome do arquivo ou endereco da imagem.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem
#' @param norm : Indica o valor a partir do qual a matriz sera normatizada para
#' variar entre 0 e 1. Ex.: Se os valores das matrizes variarem entre 0 e 255,
#' a norma seria 255. Se for TRUE o menor valor da matriz sera convertido a zero,
#' o maior para 1 e os demais serao intermediarios. Default = 1. Este parametro
#' e util apenas quando as imagens carregadas sao da extensao tif.

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Importa uma imagem.
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}

#' @examples
# \donttest{
#'#Carregar imagem de exemplo
#'End=example_image(1)
#'im=read_image(End,plot=TRUE)
#'
#}
#'@export


read_image=function(file,plot=FALSE,norm=1){
  ebimage()
  raster=FALSE
  n=unlist(strsplit(file, "[.]"))
  if((n[length(n)]=="tif")|(n[length(n)]=="TIF")){raster=TRUE}
  if(raster==FALSE){
norma=norm
  n=unlist(strsplit(file, "[.]"))
#   if((n[length(n)]=="tif")|(n[length(n)]=="TIF")){
#     im2=raster::stack(file)
# print(im2)
#
#
#     if(length(im2@layers)==1){
#       #a=raster::raster(im2)
#
# #arr=extract_band(im2,1,1)
#       im2=raster::stack(file)
#
#       im2=unstack(im2)
#
#       # aa=matrix(im2b[[1]],ncol=im2@ncols,byrow = T)
#       # if(is.numeric(norma)){
#       # aa=aa/norma
#       # }
#       #
#       # if(isTRUE(norma)){aa=Normatiza(aa,Metodo=2)}
#       #
#       #
#       # im3=EBImage::as.Image(aa)
#       #
#       # im3@colormode=as.integer(0)
#     }
#
#     if(length(im2@layers)>1){
#       arr=array(NA,c(im2@ncols,im2@nrows, length(im2@layers)))
#       im2a=raster::stack(file)
#
#       im2b=unstack(im2a)
#
#
#
#      # im2=EBImage::readImage(file)
#     for(i in 1:length(im2@layers)){
#       if(is.numeric(norma)){
#       arr[,,i]=matrix(im2b[[i]],ncol=im2@ncols,byrow = F)/norma
# }
#
#       if(isTRUE(norma)){
#         arr[,,i]=Normatiza(matrix(im2b[[i]],ncol=im2@ncols,byrow = F),Metodo=2)
#       }
#
#
#     }
#       im3=EBImage::as.Image(arr)
#       im3@colormode=as.integer(2)
#     }
#
# im2
#
#   }

  if((n[length(n)]!="tif")&(n[length(n)]!="TIF")){
    im3=EBImage::readImage(file)
  }


  if(plot==T){
    plot_image(im3)
    }


  return(im3)
}

if(raster==TRUE){
  im=raster::stack(file)

  if(plot==TRUE){
    raster::plot(im,col=colorRampPalette(c("black","white"))(100))
  }
  return(im)
}
}

