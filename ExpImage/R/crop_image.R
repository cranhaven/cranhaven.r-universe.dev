#'Esta funcao corta a imagem retirando as laterais nao desejadas. (This function
#' removes unwanted sides from the images.)
#'@description Esta funcao permite cortar a imagem (This function allows you to
#'  crop the image).
#'@usage crop_image(im,w=NULL,h=NULL,segmentation=NULL,plot=TRUE,
#'    extent=NULL,verbose=FALSE)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format).
#'@param w Deve ser um vetor contendo os numeros das colunas que permanecerao na
#'  imagem (It must be a vector containing the column numbers that will remain
#'  in the image).
#'@param h Deve ser um vetor contendo os numeros das linhas que permanecerao na
#'  imagem (It must be a vector containing the numbers of the lines that will
#'  remain in the image ).
#'@param segmentation matrix binaria obtida por uma segmentacao
#'(Binary matrix obtained of a segmentation)
#'@param plot Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default)).
#'@param extent Caso a imagem seja do tipo TIF este objeto devera ter um vetor com quatro valores
#' de coordenadas que delimitam a area a ser cortada. Neste caso o argumento raster deve ser TRUE
#'(If the image is of type TIF, this object must have a vector with four coordinate values that
#' delimit the area to be cut. In this case the raster argument must be TRUE).
#'@param verbose Indica se sera apresentada (TRUE) ou nao (FALSE) (default) os
#'pontos de corte (Indicates whether the segmented image will be
#'  displayed (TRUE) or not (FALSE) (default) the points crop).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Retorna uma imagem cortada, apresentando apenas os  pixels
#'  selecionados (Returns a cropped image showing only selected pixels).
#'@seealso  \code{\link{edit_image}} ,  \code{\link{edit_imageGUI}}
#'@importFrom stats binomial glm predict dist aggregate
#'@importFrom grDevices dev.off  jpeg colorRampPalette rgb
#'@importFrom graphics  lines locator
#'@importFrom utils setTxtProgressBar txtProgressBar unstack
#'@importFrom raster raster
#'@export
#'@examples
#\donttest{
#'#library(ExpImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'
#'##Cortar Imagem
#'im3=crop_image(im,w =286:421,h=242:332,plot = TRUE)
#'
#' #Exemplo utilizando mascara
#' imb=read_image(example_image(2),plot=TRUE)
#' m=gray_scale(imb,"g/rgb",plot=TRUE)
#' mask=segmentation(m,threshold="otsu",plot=TRUE)
#' imc=crop_image(imb,segmentation=mask,plot=TRUE)
#'
#'# intefacie grafica
#'\dontrun{
#'im=read_image(example_image(2),plot=TRUE)
#'im2=crop_image(im)
#'}
#'

#'




crop_image=function(im,w=NULL,h=NULL,segmentation=NULL,plot=TRUE,extent=NULL,verbose=FALSE){
  classe=class(im)
  raster=FALSE
  if(length(classe)>1) classe=classe[1]

  if((classe=="RasterStack")|(classe=="RasterBrick")){ raster=TRUE
 }

  if(raster==FALSE){

  n=1
  nr=dim(im)[1]
  nc=dim(im)[2]
  l=length(dim(im))
  if(is.null(segmentation)){
  if(!is.null(w)|!is.null(h)){
    if(is.null(w)){w=1:nr}
    if(is.null(h)){h=1:nc}

    if((mode(w)!="numeric")|is.null(h)) {stop("Vectors must be numeric.")}
    if((mode(h)!="numeric")|is.null(w)) {stop("Vectors must be numeric.")}
      if(l==3){im@.Data=im@.Data[w,h,]}
      if(l==2){im=im[w,h]}
   }

   if(is.null(w)&is.null(h)){

     im0=im
     im=resize_image(im,w=400)

     print("Clique em dois vertices opostos para cortar (Click on two opposite vertices to crop)")
     if(EBImage::is.Image(im)){plot_image(im)}
     if(is.matrix(im)){plot_image(EBImage::as.Image((im)))}
     c=NULL
     for(i in 1:2){
      c0=locator(type = "p", n = 1, col = "red", pch = 22)
       c=rbind(c,c(c0$x,c0$y))

       if(i>1){
         lines(c[(i-1):i,],col="red")
       }
     }
     print(c)
     c[,1]=ncol(im0@.Data[,,1])*c[,1]/ncol(im@.Data[,,1])
     c[,2]=nrow(im0@.Data[,,1])*c[,2]/nrow(im@.Data[,,1])
     im=im0
       w=round(min(c[,1]),0):round(max(c[,1]),0)
       h=round(min(c[,2]),0):round(max(c[,2]),0)

       if(l==3){im@.Data=im@.Data[w,h,]}
       if(l==2){im=im[w,h]}
     }
}


if(!is.null(segmentation)){
    m=segmentation
    r=(1:nrow(m))[(rowSums(m)!=0)]
    w=(min(r)-n):(max(r)+n)

    c=(1:ncol(m))[(colSums(m)!=0)]
    h=(min(c)-n):(max(c)+n)

    if(l==3){im@.Data=im@.Data[w,h,]}
    if(l==2){im=im[w,h]}
    }








  mm=rbind(
  w=c(round(min(w),0),round(max(w),0)),
  h=c(round(min(h),0),round(max(h),0))
  )

  rownames(mm)=c("min","max")

  if(verbose==TRUE){print(mm)}


  if(plot==T){
    if(EBImage::is.Image(im)){plot_image(im)}
    if(is.matrix(im)){plot_image(EBImage::as.Image((im)))}

  }


  return(im)
}


  if(raster==TRUE){
    imb=raster::crop(im,raster::extent(extent))
   # imb=raster:stack(im)
   if(plot==TRUE){plot_image(imb)}
    return(imb)

  }
}






