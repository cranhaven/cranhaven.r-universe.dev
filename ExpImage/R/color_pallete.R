#'Creates a color palette from an image (Criar uma paleta de cores a partir de uma imagem).
#'
#' @description Creates a color palette from an image (Criar uma paleta de cores a partir de uma imagem)

#' @usage color_pallete(im,number=8,mask=NULL,proportional=FALSE,plot=TRUE)

#' @param im    :Image in ExpImage format (Imagem no formato do ExpImage).
#' @param number    :Number of color scales to be created (Numero de escalas de cores a serem criados)

#' @param mask    : Mask obtained by the segmentation process, default=NULL (Mascara obtida pelo processo de segmentacao).
#' @param proportional    :Logical variable indicating whether the pixel quantity of each color will be proportional to the original image
#' (Variavel logica indicando se a quantidade pixel de cada cor sera proporcional à da imagem original).
#' @param plot    : Logical value, if TRUE, the image will be displayed
#' (Valor logico, se for TRUE a imagem sera apresentada).

#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)

#' @return Returns the segmented image (Retorna a imagem segmentada).
#' @seealso  \code{\link{segmentation}} , \code{\link{calibration_palette}}

#' @examples
#'
#' name=example_image(3)
#' image=read_image(name,plot=TRUE)
#' pallete=color_pallete(im=image,number =5,plot = TRUE)
#' pallete=color_pallete(im=image,number =5,proportional = TRUE,plot = TRUE)
#'\donttest{
#' name=example_image(10)
#' image=read_image(name,plot=TRUE)
#' pallete=color_pallete(im=image,number =5,plot = TRUE)
#' pallete=color_pallete(im=image,number =5,proportional = TRUE,plot = TRUE)
#'
#' m=gray_scale(image,method = "GLI",plot=TRUE)
#' seg=segmentation(m,treshold = 0.25,plot=TRUE)
#' pallete1=color_pallete(im=image,number =10,mask = seg,plot = TRUE)
#' pallete2=color_pallete(im=image,number =10,mask = seg,proportional = TRUE,plot = TRUE)
#'
#'
#' name=example_image(13)
#' image=read_image(name,plot=TRUE)
#' pallete1=color_pallete(im=image,number =10,plot = TRUE)
#' pallete2=color_pallete(im=image,number =10,proportional = TRUE,plot = TRUE)
#'
#' m=gray_scale(image,method = "r",plot=TRUE)
#' seg=segmentation(m,treshold = "otsu",fillHull = TRUE,plot=TRUE)
#' pallete1=color_pallete(im=image,number =10,mask = seg,plot = TRUE)
#' pallete2=color_pallete(im=image,number =10,mask = seg,proportional = TRUE,plot = TRUE)
#'
#' }
#'@export
#'





color_pallete=function(im,number=8,mask=NULL,proportional=FALSE,plot=TRUE){
  if(is.null(mask)){
    id=matrix(TRUE,nrow = nrow(im@.Data[,,1]),ncol = ncol(im@.Data[,,1]))
  }

  if(!is.null(mask)){
    id=(mask*1)!=0
  }

  ck=clustering_Kmeans(im,ncluster=number,mask = mask,plot = F)
  camadas=length(ck)
  ck2=1*ck[[1]]
  for(i in 2:camadas){
    ck2=ck2+i*ck[[i]]
  }
ck=ck2

# if(is.matrix(id)){
#   im@.Data[,,1][id==1]=NA
#   im@.Data[,,2][id==1]=NA
#   im@.Data[,,3][id==1]=NA
# }
  MAT=NULL
  for(i in unique(na.omit(c(ck)))){
    r=mean(im@.Data[,,1][ck==i],na.rm=T)
    g=mean(im@.Data[,,2][ck==i],na.rm=T)
    b=mean(im@.Data[,,3][ck==i],na.rm=T)
    MAT=cbind(MAT,c(r=r,g=g,b=b))
  }


  MATn=NULL
  for(i in unique(na.omit(c(ck)))){
    r=length(na.omit(im@.Data[,,1][ck==i]))

    MATn=cbind(MATn,r=r)
  }





  if(proportional==FALSE){
  n=ncol(MAT)
  ARR=array(NA,dim = c(100,66*n,3))
  c=1
  f=66
  for(i in 1:n){
    ARR[1:100,c:f,1]=MAT[1,i]
    ARR[1:100,c:f,2]=MAT[2,i]
    ARR[1:100,c:f,3]=MAT[3,i]

    c=f+1
    f=f+66
  }
  }


  if(proportional==TRUE){
    n=ncol(MAT)
    ARR=array(NA,dim = c(100,66*n,3))

    nn=round(66*n*(MATn/sum(MATn)),0)
    a=1
    b=nn[1]
    nn=c(nn,0)
    for(i in 1:n){

      ARR[1:100,a:b,1]=MAT[1,i]
      ARR[1:100,a:b,2]=MAT[2,i]
      ARR[1:100,a:b,3]=MAT[3,i]

      a=b+1
      b=b+nn[i+1]
      if(b>(66*n)){b=66*n}
    }



  }


  im2=EBImage::as.Image(ARR)
  EBImage::colorMode(im2)=2
  if(plot==TRUE){plot_image(im2)}
  return(im2)
}
