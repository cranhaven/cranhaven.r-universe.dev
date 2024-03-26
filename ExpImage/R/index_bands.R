#' Estimation of vegetation indices (Estimacao de indices de vegetacao).
#'
#' @description This function create vegetation indices
#' (Esta funcao cria indices de vegetacao).
#' @usage index_bands(index="NDVI",red=NULL,green=NULL,blue=NULL,nir=NULL,
#'       swir=NULL,normalize=FALSE)

#' @param index   :Vegetation index to be estimated, default="NDVI" (Indice de vegetacao a ser estimado, default="NDVI").
#' @param red    :Matrix with the red band (Matriz com a banda de vermelho).
#' @param green    :Matrix with the green band (Matriz com a banda de verde).
#' @param blue    :Matrix with the blue band (Matriz com a banda de azul).
#' @param nir    :Matrix with the nir band (Matriz com a banda nir).
#' @param swir    :Matrix with the swir band (Matriz com a banda swir).

#' @param normalize    :Logical value, if TRUE, the result will be normalized to vary between 0 and 1
#'  (Valor logico, se for TRUE o resultado sera normatizado para variar entre 0 e 1).
#'
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{gray_scale}} , \code{\link{read_image}}
#' @examples
#'\donttest{
#' #' ########################################################
#' ###' Abrindo o endereco de bandas de imagens de satelite
#' ########################################################
#' end1=example_image(14) #Banda de azul
#' end2=example_image(15) #Banda de verde
#' end3=example_image(16) #Banda de vermelho
#' end4=example_image(17) #Banda de IR
#' end5=example_image(18) #Banda de SWIR
#'
#' ########################################################
#' ###' Abrindo bandas de imagens de satelite
#' ########################################################
#' B1=read_image(end1,plot=TRUE)
#' B2=read_image(end2,plot=TRUE)
#' B3=read_image(end3,plot=TRUE)
#' B4=read_image(end4,plot=TRUE)
#' B5=read_image(end5,plot=TRUE)
#'
#' ########################################################
#' ###' Juntando as bandas em uma imagem
#' ########################################################
#' im=join_bands(imgs = list(B1,B2,B3,B4,B5))
#' tif_info(im)
#'
#' ##########################################################
#' #### Mostrando a imagem colorida
#' ##########################################################
#' plot_image(im,r=3,g=2,b=1,rasterRGB = TRUE)
#'

#' ########################################################
#' ###' Calculando o NDVI
#' ########################################################
#' NDVI=index_bands(index="NDVI",red =im[[3]],nir = im[[4]])
#' plot_image(im=NDVI,col=8,axis=TRUE,title="NDVI")
#'
#' ########################################################
#' ###' Calculando o NDWI
#' ########################################################
#' NDWI=index_bands(index="NDWI",green =im[[2]],swir = im[[5]])
#' plot_image(im=NDWI,col=8,axis=TRUE,title="NDWI")
#'}
#' @export
index_bands=function(index="NDVI",red=NULL,green=NULL,blue=NULL,nir=NULL,swir=NULL,normalize=FALSE){
raster=FALSE
 if(!is.null(red)) t=red
 if(!is.null(green)) t=green
 if(!is.null(blue)) t=blue
 if(!is.null(nir)) t=nir
 if(!is.null(swir)) t=swir

 if((base::class(t)=="RasterStack")|(base::class(t)=="RasterLayer")) raster=TRUE

if(raster==FALSE){
if(index=="NDVI"){
if(is.null(nir)|is.null(red)){
  stop("nir and/or red is NULL")
}
Index=as.matrix((nir-red)/(nir+red))
}
  if(index=="NDWI"){
    if(is.null(green)|is.null(swir)){
      stop("green and/or swir is NULL")
    }
    green=green/(255*255)
    swir=swir/(255*255)
    Index=as.matrix((green-swir)/(green+swir))
  }
  if(isTRUE(normalize)){Index=Normatiza(Index,Metodo = 2)}
}

  if(raster==TRUE){
    if(index=="NDVI"){
      if(is.null(nir)|is.null(red)){
        stop("nir and/or red is NULL")
      }
      Index=((nir-red)/(nir+red))
    }
    if(index=="NDWI"){
      if(is.null(green)|is.null(swir)){
        stop("green and/or swir is NULL")
      }
      Index=((green-swir)/(green+swir))
    }
    if(isTRUE(normalize)){Index=Normatiza(Index,Metodo = 2)}
  }



return(Index)

}
