#' Image information in tif format (Informacoes das imagens no formato tif).
#'
#' @description Function to get information from images in tif format
#' (Funcao para obter informacoes das imagens no formato tif).
#' @usage tif_info(img=NULL,file=NULL)
#' @param img Este objeto deve conter uma imagem no formato do EBImage ou na forma
#'de uma matriz, no caso de imagem em escala de cinza (This
#'  object must contain an image in EBImage format).
#' @param file    :File name or image address (Nome do arquivo ou endereco da imagem).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Function to get information from images in tif format
#' (Funcao para obter informacoes das imagens no formato tif).
#' @seealso  \code{\link{crop_image}} , \code{\link{edit_image}}
#' @examples
#' ########################################################
#' ###' Abrindo o endereco de bandas de imagens de satelite
#' ########################################################
#' end1=example_image(14) #Banda de azul
#'
#' ########################################################
#' ###' Abrindo bandas de imagens de satelite
#' B1=read_image(end1,plot=TRUE)
#' ########################################################
#' ###' Informacao das bandas de imagens de satelite
#'  tif_info(B1)

#'
#'@export

tif_info=function(img=NULL,file=NULL){
  if((is.null(img))&(!is.null(file))){

  n=unlist(strsplit(file, "[.]"))
  if((n[length(n)]=="tif")|(n[length(n)]=="TIF")){
    img=raster::stack(file)
   Info= info_image(img)

Extent=raster::extent(img)[1:4]
names(Extent)=c("xmin","xmax","ymin","ymax")


Res=list(
End=file,
Crs=raster::crs(img)@projargs,
Res=raster::res(img),
Extent=Extent,
Info=Info
)


  }

  if((n[length(n)]!="tif")&(n[length(n)]!="TIF")){

    stop("The file must be of tif type (O arquivo deve ser do tipo tif)")
  }

  return(Res)
}

  if((!is.null(img))&(is.null(file))){
    Info= info_image(img)

    Extent=raster::extent(img)[1:4]
    names(Extent)=c("xmin","xmax","ymin","ymax")


    Res=list(
      End=file,
      Crs=raster::crs(img)@projargs,
      Res=raster::res(img),
      Extent=Extent,
      Info=Info
    )

    Res}
}
