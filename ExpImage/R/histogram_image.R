#' Create a histogram from image bands
#' (Criar um histrograma a partir das bandas das imagens)
#'
#' @description This function allows you to create histograms from the bands of the images
#' (Esta funcao permite criar histogramas a partir das bandas das imagens).
#' @usage histogram_image(im,layout=2,lim=1000,BandNames=NULL)

#' @param im    :Este objeto deve conter uma imagem (This object must contain an image ).
#' @param layout    : Valor numerico variando entre 1 e 3 para se ter diferentes layouts
#' (Numeric value ranging between 1 and 3 to have different layouts).
#' @param lim    : Refere-se ao numero maximo de pixels que se deseja considerar para
#' obter o histograma. Se for NULL todos os pixels serao considerados (refers to the maximum number
#'  of pixels considered to obtain the histogram. If NULL all pixels will be considered).
#' @param BandNames  :Refere-se ao nome das bandas (Refers to the bands names)
#' @return Retorna histogramas a partir das bandas (Return histograms from the bands of the images).
#' @seealso  \code{\link{segmentation_logit}}

#' @examples
#' \donttest{
#' end=example_image(6)
#' im=read_image(end,plot=TRUE)
#' histogram_image(im,layout = 1)
#' histogram_image(im,layout = 2)
#' histogram_image(im,layout = 3)
#' histogram_image(im,BandNames = c("Azul","Verde",
#' "Vermelho","IR","SWIR"))
#'
#'
#'

#' ########################################################
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
#' histogram_image(im)
#'}
#' @export

histogram_image=function(im,layout=2,lim=1000,BandNames=NULL){

  if((c(class(im))=="RasterStack")|(c(class(im))=="RasterLayer")|(c(class(im))=="RasterBrick")){
    im=raster2image(im)

  }

  Bands=Rows=Cols=Value=0

if(is.matrix(im)){
  im=EBImage::as.Image(im)
}


  if(is.numeric(lim)){
    n=info_image(im)$Length[1]
    if(n>lim){im=resize_image(im,w=lim)
    }
  }


  if(length(dim(im))==2){
    arr=array(NA,dim = c(dim(im),1))
    arr[,,1]=im@.Data
  }

if(length(dim(im))==3){
  arr=im@.Data
}

if(length(dim(im))>3){
  stop("The file must be a image or matrix type")
}

  DIM=dim(arr)

  if (is.null(BandNames)){
    BandNames=paste("Band",1:DIM[3])
  }


  arr2=NULL
  for(i in 1:DIM[3]){
    arr2=rbind(arr2,cbind(BandNames[i],linearize_image(arr[,,i])))
  }
  arr2=arr2[!is.na(arr2[,4]),]
  colnames(arr2)=c("Bands","Row","Col","Value")
  arr2$Band=as.factor( arr2$Band)

  if(layout==1){
  PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,color=Bands))+
    ggplot2::geom_histogram(alpha=0.2,position="identity",bins = 30)+
    ggplot2::theme_classic()
  PLOT
  }

  if(layout==2){
    PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,fill=Bands))+
      ggplot2::geom_histogram(color="black",bins = 30)+
      ggplot2::facet_grid(Bands~.)+
      ggplot2::theme_classic()
    PLOT
  }

  if(layout==3){
    PLOT=ggplot2::ggplot(arr2,ggplot2::aes(x=Value,fill=Bands))+
      ggplot2::geom_histogram(colour="black",bins = 30)+
      ggplot2::facet_wrap(Bands~.)+
      ggplot2::theme_classic()
    PLOT
  }
return(PLOT)
}
