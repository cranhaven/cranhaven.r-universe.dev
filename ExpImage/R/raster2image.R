#' Convert a image in raster format to ExpImage format (Converte uma imagem no formato raster para o formato ExpImage)
#'
#' @description This function converts a image in raster format to ExpImage format (Esta funcao converte uma imagem no formato raster para o formato ExpImage)
#' @usage raster2image(im,Bands=NULL,norma=1)
#' @param im    : Image in a raster format (imagem do formato raster)
#' @param Bands    : Vector with the values of desired layers (Vetor contendo os numeros correspondentes as camadas desejadas)
#' @param norma    : Value used as the denominator to have values pixels whith values between 0 and 1 (Valor utilizado como divisor para ter pixels com valores entre 0 e 1 )
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns an raster image in ExpImage format (Retorna um imagem no formato do ExpImage).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}



#
#'@export
#'


raster2image=function(im,Bands=NULL,norma=1){
  im2b=raster::values(im)
  ncols=im@ncols
  nrows=im@nrows
  bands=ncol(im2b)
  aa=array(NA,dim=c(ncols,nrows,bands))
  for(i in 1:bands){
    aa[,,i]=matrix(im2b[,i],ncol=ncols)
  }


  if(!is.null(norma)) aa=aa/norma
  if(is.null(Bands)){bb=aa}
  if(!is.null(Bands)){bb=aa[,,Bands]}
  im3=EBImage::as.Image(bb)
  EBImage::colorMode(im3)=2
  return(im3)
}


