#' Creates a mask over the background or foreground (Cria uma mascara sobre o
#' background ou foreground)
#'
#' @description This function superimposes a mask over the pixels corresponding to
#' the background or foreground.(Esta funcao permite criar mascara sobre os pixels
#'   correspondentes ao background ou foreground)
#' @usage mask_pixels(im,TargetPixels,TargetPixels2=NULL,col.TargetPixels="rand",
#' Contour=FALSE, r = 1, g = 2, b = 3, norma = 1,plot=FALSE)

#' @param im    :This object must contain an image in EBImage format (Este objeto deve conter uma imagem no formato do EBImage).
#' @param TargetPixels    : (Este objeto deve ser  uma matriz
#'   binaria ou uma lista com varias matrizes. Em cada matriz deve conter os valores 0 (pixels do background) ou 1 (pixels do
#'   foreground)).
#' @param TargetPixels2    : Este objeto deve ser  uma matriz
#'   binaria ou uma lista com varias matrizes. Em cada matriz deve conter os valores 0 (pixels do background) ou 1 (pixels do
#'   foreground)).
#' @param col.TargetPixels : Se for a palavra "rand" serao valores escolhidos para cada matriz.
#' Pode tambem ser um vetor contendo os nomes das cores. Neste caso, o tamanho de vetor deve ser igual
#' ao numero de matrizes.
#' @param Contour : Valor logico. Se for FALSE (default) sera a parte de interesse sera preenchida.
#' Se for TRUE a area de interesse sera contornada.
#' @param r : Camada correspondente a banda de vermelho.
#' @param g : Camada correspondente a banda de verde
#' @param b = Camada correspondente a banda de azul
#' @param norma = Valor a ser dividido em cada pixel para se ter valor entre 0 e 1.
#' @param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'   imagem segmentada.

#' @return Retorna uma imagem com uma mascara sobre os  pixels selecionados.
#'  @seealso  \code{\link{segmentation_logit}}

#' @examples
#' \donttest{
#'###########################################################################
#'#Estimar a area atacada por doenca no tomateiro
#'###########################################################################
#'
#'   im=read_image(example_image(ex=7))
#'   plot_image(im)
#'
#'   #Selecionando o melhor indice para a segmentacao da folha
#'   r=gray_scale(im,method = "r",plot=TRUE)
#'   g=gray_scale(im,method = "g",plot=TRUE)
#'   b=gray_scale(im,method = "b",plot=TRUE)
#'
#'   #O limiar pode ser um valor escolhido aleatoriamente
#'   MatrizSegentada=segmentation(b,threshold = 0.5,fillHull = FALSE,plot=TRUE)
#'

#'   #O limiar tambem pode ser estabelecido pelo metodo de otsu
#'   MatrizSegentada2=segmentation(b,threshold = "otsu",fillHull = TRUE
#'   ,selectHigher= FALSE, plot=TRUE)
#'
#'
#'   #Selecionar na imagem apenas os pixeis desejaveis (Folha)
#'   im2=extract_pixels(im,target=MatrizSegentada2,valueTarget=TRUE,
#'   valueSelect=c(r=1,g=1,b=1),plot=TRUE)
#'
#'   ################################################################
#'   #Selecionando o melhor indice para a segmentacao da doenca
#'   r=gray_scale(im2,method = "r",plot=TRUE)
#'   g=gray_scale(im2,method = "g",plot=TRUE)
#'   b=gray_scale(im2,method = "b",plot=TRUE)
#'
#'   MatrizSegmentada3=segmentation(g,threshold = 0.3,selectHigher = FALSE,
#'   fillHull =TRUE,plot=TRUE)
#'
#'
#'   #Como pode-se obsevar, a segmentacao por limiar nao e possivel. Entao vamos
#'   #usar paletas de cores
#'   folha=read_image(example_image(ex=8))
#'   doenca=read_image(example_image(ex=9))
#'
#'   DoencaSeg=segmentation_logit(im,foreground = doenca,background =
#'   folha,sample = 2000,fillHull = TRUE,TargetPixels =MatrizSegentada2==1
#'   ,plot=TRUE)
#'
#'   im3=mask_pixels(im2,TargetPixels=DoencaSeg==1)
#'    plot_image(im3)
#'
#'   ii=join_image(im,im3,plot=TRUE)
#'
#'
#'   #Porcentagem da area lesionada.
#'
#'   100*(sum(DoencaSeg)/sum(MatrizSegentada2))
#'}
#' @export
#' @exportS3Method print mask_pixels

mask_pixels=function(im,TargetPixels,TargetPixels2=NULL,
                     col.TargetPixels="rand",Contour=FALSE,
                     r=1,g=2,b=3,norma=1,plot=FALSE){
 # TargetPixels==1

  classe=class(im)
  raster=FALSE
  if(length(classe)>1) classe=classe[1]

  if((classe=="RasterStack")|(classe=="RasterBrick")|(classe=="RasterLayer")){ raster=TRUE

  }


  color=col.TargetPixels

  if(raster==FALSE){

  if(!is.list(TargetPixels)){TargetPixels=list(TargetPixels)}
  if(!is.list(TargetPixels2)){TargetPixels2=list(TargetPixels2)}

  if(!is.null(TargetPixels2[[1]])) TargetPixels=c(TargetPixels,TargetPixels2)
n=length(TargetPixels)

rr=gg=bb=NULL
  if(col.TargetPixels[1]=="rand"){
    rr=runif(n,0,1)
    gg=runif(n,0,1)
    bb=runif(n,0,1)
  }

  # if(col.TargetPixels!="rand"){
  #   if(length(col.TargetPixels)!=length(TargetPixels)){
  #     warning("O numero de cores no vetor `col.TargetPixels` deve ser igual ao numero de imagens binarias \n The number of colors in the vector `col.TargetPixels` must equal the number of binary images ")
  #     rr=runif(1,0,1)
  #     gg=runif(1,0,1)
  #     bb=runif(1,0,1)
  #   }
  #
  # }


  a=0
  for( i in 1:n) {
    if(!is.null(TargetPixels[[i]][[1]])){
    a=a+1
    if(col.TargetPixels[1]!="rand"){
        cc=c(grDevices::col2rgb(col.TargetPixels[a])/255)
        rr=c(rr,cc[1])
        gg=c(gg,cc[2])
        bb=c(bb,cc[3])

      }


  r=im@.Data[,,1]
  g=im@.Data[,,2]
  b=im@.Data[,,3]
ID=EBImage::as.Image(TargetPixels[[a]])
  ID=ID@.Data==TRUE
  if(isTRUE(Contour)){ID=contour_image(ID==1,plot=F)==1}
  r[ID]=rr[a]
  g[ID]=gg[a]
  b[ID]=bb[a]

  im@.Data[,,1]=r
  im@.Data[,,2]=g
  im@.Data[,,3]=b

  }
  }



if(plot==T){plot_image(im)}
  }


######################
  ################
  ###############
  #

  if(raster==TRUE){
    if(!is.list(TargetPixels)){TargetPixels=list(TargetPixels)}
    if(!is.list(TargetPixels2)){TargetPixels2=list(TargetPixels2)}
    if(!is.null(TargetPixels2[[1]])){
    TargetPixels=c(TargetPixels,TargetPixels2)}
    n=length(TargetPixels)

    if(col.TargetPixels[1]=="rand"){
      rr=runif(n,0,1)
      gg=runif(n,0,1)
      bb=runif(n,0,1)
      color=rgb(rr,gg,bb)
    }



    a=0

    raster::plotRGB(im,r=r,g=g,b=b,axes=FALSE,stretch="lin")
    for( i in 1:n) {
      TP=TargetPixels[[i]]
      raster::plot(TP,col=color[i],add=TRUE,legend=FALSE)

      }
  }







  return(im)
}




print.mask_pixels=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x),"\n")
}
