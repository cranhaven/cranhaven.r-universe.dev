#'Function to compare image sizes  (Funcao para comparar os tamanhos das imagens).
#'
#' @description This function compares the size of images  (Esta funcao compara o tamanho das imagens).
#' @usage join_bands(imgs=NULL,filesnames=NULL,path = NULL)

#' @param imgs    :List object containing the images  (Objeto do tipo lista contendo as imagens).
#' @param filesnames    :Images names (Nomes das imagens).
#' @param path    :Path files  (Endereco das pastas).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Return images size (Retorna o tamanho das imagens).
#' @seealso  \code{\link{info_image}} , \code{\link{read_image}}

#' @examples
#' ###############################################
#' #Carregando imagens de exemplo
#' im1=read_image(example_image(2),plot=TRUE)
#' r=gray_scale(im = im1,method = "r",normalize=FALSE)
#' g=gray_scale(im = im1,method = "g",normalize=FALSE)
#' b=gray_scale(im = im1,method = "b",normalize=FALSE)
#'
#' im2=join_bands(imgs = list(r,g,b))
#' plot_image(im2)
#'
#'\donttest{
#'
#'#########################################################
#'#########################################################
#'#' #' ########################################################
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

#' plot_image(im,r=3,g=2,b=1,rasterRGB=TRUE)
#' }
#'@export
#'

join_bands=function(imgs=NULL,filesnames=NULL,path = NULL){
n=1*(!is.null(imgs))+1*(!is.null(filesnames))+1*(!is.null(path))
  if(n==0){stop("The 'imgs', 'filenames' or 'path' arguments must have information (Os argumentos 'imgs', 'filenames' ou 'path'  devem ter informacoes).")}
  if(n>1){stop("Only one of the 'imgs', 'filenames' or 'path' arguments must have information (Apenas um dos argumentos 'imgs', 'filenames' ou 'path'  devem ter informacoes).")}
raster=FALSE


if(!is.null(imgs)){a=imgs[[1]] ; if((base::class(a)[1]=="RasterStack")|(base::class(a)[1]=="RasterLayer")){raster=TRUE}}

if(!is.null(path)){ filenames=list.files(path =path )
  filesnames=paste(path,filesnames ,sep="/")}

if(!is.null(filesnames)){
  n=unlist(strsplit(as.character(filesnames[[1]]), "[.]"))
  if((n[length(n)]=="tif")|(n[length(n)]=="TIF")){raster=TRUE}
}

  if(raster==FALSE){
  if(!is.null(path)){
    filenames=list.files(path =path )
    filesnames=paste(path,filesnames ,sep="/")}

  if(is.null(imgs)&!is.null(filesnames)){
  imgs=list()

  pb <- progress::progress_bar$new(total = length(filesnames))
    for(i in 1:length(filesnames)){
      pb$tick()
      imgs[[i]]=read_image(filesnames[i])
    }

  }


RES=NULL

a=imgs[[1]]
if(!EBImage::is.Image(a)) {a=as_image(a)}
arr=array(NA,c(dim(imgs[[1]]),length(imgs)))
arr[,,1]=a@.Data

#pb1 <- progress::progress_bar$new(total = length(filesnames))
for(i in 2:length(imgs)){
 # pb1$tick()

  arr[,,i]=imgs[[i]]@.Data
  }

a@.Data=arr
a@colormode=length(imgs)

#class(a)="RasterStack"
return(a)
  }


  if(raster==TRUE){

    if(!is.null(path)){ filesnames=paste(path,filesnames ,sep="/")}

    if(is.null(imgs)&(!is.null(filesnames))){
      imgs=list()

      pb <- progress::progress_bar$new(total = length(filesnames))
      for(i in 1:length(filesnames)){
        pb$tick()
        imgs[[i]]=read_image(filesnames[[i]])
      }

    }



    im2=raster::stack(x = imgs)
    #im2=raster::raster(imgs)

    return(im2)
  }
}
