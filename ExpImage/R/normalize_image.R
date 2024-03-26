#' Create a histogram from image bands
#' (Criar um histrograma a partir das bandas das imagens)
#'
#' @description This function allows you to create histograms from the bands of the images (Esta funcao permite criar histogramas a partir das bandas das imagens).
#' @usage normalize_image(im,inf=0,max=1)

#' @param im    :Este objeto deve conter uma imagem (This object must contain an image ).
#' @param inf    : Numero em que o pixel de menor valor devera ter
#' (Number in which the lowest value pixel should have).
#' @param max    : Numero em que o pixel de maior valor devera ter
#' (Number in which the pixel with the highest value should have).

#' @return Retorna histogramas a partir das bandas (Return histograms from the bands of the images).
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
#' \donttest{
#' end=example_image(6)
#' im=read_image(end,plot=TRUE)
#' histogram_image(im,layout = 1)
#' histogram_image(im,layout = 2)
#' histogram_image(im,layout = 3)
#'
#' im2=normalize_image(im)
#' plot_image(im2)
#' histogram_image(im2,layout = 3)
#' }

#' @export



normalize_image=function(im,inf=0,max=1)
{
  info=info_image(im)
  bandas=c(info$Length,1)[3]

  if(bandas>1){
  for(i in 1:bandas){
    im@.Data[,,i]=Normatiza(im@.Data[,,i],Metodo = 2)

  }
  }


  if(bandas==1){
    im@.Data=Normatiza(im@.Data,Metodo = 2)
  }
  im
  }
