#' Obter uma paleta de cores com cliques (Get a color palette with clicks ).
#'
#'@description Esta funcao retorna uma paleta de cores com os pixels selecionados.
#' (This function returns a color palette with selected pixels).
#'@usage pick_palette(im,palette.previous=NULL,percent.pick=0.01)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@param palette.previous Este objeto pode conter uma imagem no formato do EBImage
#' com parte da paleta de cores obtida previamente (This
#'  object can contain an image in EBImage format whith a color palette).
#'@param percent.pick E um valor variando entre 0 e 1. Quanto menor for o valor, menor
#'sera a area capturada com os cliques. (And a value ranging between 0 and 1. The smaller
#' the value, the smaller the area captured with the clicks will be).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Esta funcao retorna uma paleta de cores com os pixels selecionados.
#' (This function returns a color palette with selected pixels).
#'@seealso  \code{\link{segmentation_logit}}, \code{\link{pick_color}}

#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'\dontrun{
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'pick_palette(im)
#'}
#' @exportS3Method print pick_color
#'




pick_palette=function(im,palette.previous=NULL,percent.pick=0.01){
  print("Clique sobre a imagem (Click on the image)")
plot_image(im)
a=0
D=NULL

dim=dim(im)
perc= min(dim[1],dim[2])*percent.pick
finish=TRUE
while(finish){

  c0=locator(type = "n", n = 1)
  #print(c0)

  if(is.null(c0)){finish=FALSE}
  if(!is.null(c0)){
  c= cbind(c0$x,c0$y)

    c1=cbind(c0$x-perc,c0$y-perc)
    c2=cbind(c0$x-perc,c0$y+perc)
    c3=cbind(c0$x+perc,c0$y+perc)
    c4=cbind(c0$x+perc,c0$y-perc)


    lines(rbind(c1,c2),col="red")
    lines(rbind(c2,c3),col="red")
    lines(rbind(c3,c4),col="red")
    lines(rbind(c1,c4),col="red")



    im2=crop_image(im,h=c1[2]:c2[2],w=c1[1]:c3[1],plot=F)
    D=rbind(D,cbind(c(im2@.Data[,,1]),c(im2@.Data[,,2]),c(im2@.Data[,,3])))
}
}

if(!is.null(palette.previous)){
  D0=cbind(c(palette.previous@.Data[,,1]),
           c(palette.previous@.Data[,,2]),
           c(palette.previous@.Data[,,3]))
D=rbind(D,D0)
}

seq=sample(nrow(D))
D=D[seq,]


n=nrow(D)

na=round(sqrt(n),0)
nb=floor(n/na)
n=na*nb

imn=array(NA,dim=c(nb,na,3))
imn[,,1]=matrix(D[1:n,1],ncol=nb,nrow=na)
imn[,,2]=matrix(D[1:n,2],ncol=nb,nrow=na)
imn[,,3]=matrix(D[1:n,3],ncol=nb,nrow=na)

imnn=EBImage::as.Image(imn)
EBImage::colorMode(imnn)=2

plot_image(imnn)



return(imnn)
}



print.pick_color=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}

