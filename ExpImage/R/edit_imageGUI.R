#' This function changes the brightness, contrast and makes the gamma adjustment
#' in the image (Esta funcao permite mudar o brilho, contraste e fazer um ajuste
#' gamma na imagem).
#'
#' @description This function changes the brightness, contrast and makes the
#'   gamma adjustment in the image (Esta funcao permite mudar o brilho,
#'   contraste e fazer um ajuste gamma na imagem).
#' @usage edit_imageGUI(im,Verbose=TRUE)

#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param Verbose    :Logical value. Show the configurations  (Valor logico.
#' Mostra as configuracoes).
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @return Returns a edited image (Retorna uma imagem editada).
#' @seealso  \code{\link{edit_image}}

#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'\dontrun{
#'#library(ExpImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'
#'##Editar imagem
#'im2=edit_imageGUI(im)
#'}

#' @exportS3Method print edit_imageGUI
#'




edit_imageGUI=function(im,Verbose=TRUE){
  print("Clique sobre a imagem para editar (Click on the image to edit)")
  #end="inst/images/Edit.png"
  end=system.file("images","Edit.png",package="ExpImage")
  G=G2=read_image(end,plot = T)
desY=722-65
desX=673-37

nc=ncol(im@.Data[,,1])
nr=nrow(im@.Data[,,1])
if(nc>=nr){
 im2= resize_image(im,h=desY)
 a=round(desY/2-nrow(im2@.Data[,,1])/2,0)
 G@.Data[a:(a-1+nrow(im2@.Data)),(66):722,]=im2@.Data
plot_image(G)
 }
if(nc<nr){
  im2= resize_image(im,w=desX)
  a=round(desY/2-ncol(im2@.Data[,,1])/2,0)
  G@.Data[38:673,a:(a-1+ncol(im2@.Data)),]=im2@.Data


  plot_image(G)
  }





# 199:277
# 277:362
# 362:441
# 441:529
# 529:612
# 612:695
#
# 796:831
# 831:871
# 871:919
# 919:962
im3=im2
fim=FALSE; Brilho=0;Contraste=1;rotate=0 ;gamma=1
while(fim==FALSE){
L=locator(type = "p", n = 1, col = "red", pch = 22)
  x=L$x  ; y=L$y



if((x>199)&(x<=277)){xx=1}
if((x>277)&(x<=362)){xx=2}
if((x>362)&(x<=441)){xx=3}
if((x>441)&(x<=529)){xx=4}
if((x>529)&(x<=612)){xx=5}
if((x>612)&(x<=695)){xx=6}
if((x>58)&(x<=374)){xxb=7}
if((x>374)&(x<=680)){xxb=8}


if((y>796)&(y<=831)){yy=1}
if((y>831)&(y<=871)){yy=2}
if((y>871)&(y<=919)){yy=3}
if((y>919)&(y<=962)){yy=4}
if((y>995)&(y<=1037)){yy=5}
#Brilho

if(yy==1){
  if(xx==1){v=-0.2}
  if(xx==2){v=-0.1}
  if(xx==3){v=-0.05}
  if(xx==4){v=0.05}
  if(xx==5){v=0.1}
  if(xx==6){v=0.2}
  Brilho=Brilho+v
}
#Contraste

if(yy==2){
  if(xx==1){v=-0.2}
  if(xx==2){v=-0.1}
  if(xx==3){v=-0.05}
  if(xx==4){v=0.05}
  if(xx==5){v=0.1}
  if(xx==6){v=0.2}
  Contraste=Contraste+v
}

#Gamma

if(yy==3){
  if(xx==1){v=-0.3}
  if(xx==2){v=-0.2}
  if(xx==3){v=-0.01}
  if(xx==4){v=0.01}
  if(xx==5){v=0.2}
  if(xx==6){v=0.3}
  gamma=gamma+v
}
#Rotate

if(yy==4){
  if(xx==1){v=-90}
  if(xx==2){v=-10}
  if(xx==3){v=-1}
  if(xx==4){v=1}
  if(xx==5){v=10}
  if(xx==6){v=90}
  rotate=rotate+v
}

#reset/finish
if(yy==5){
if(xxb==7){im3=im2;Brilho=0;Contraste=1;rotate=0 ;gamma=1}
if(xxb==8){fim=TRUE}
}

im3= edit_image(rotate_image(im2,angle = rotate,plot = F),brightness = Brilho,contrast = Contraste,gamma  = gamma,plot = F)
G=G2
#a=round(desY/2-ncol(im2@.Data[,,1])/2,0)
nc=ncol(im3@.Data[,,1])
nr=nrow(im3@.Data[,,1])
if(nc>=nr){
  im3= resize_image(im3,h=desY)
  a=round(desY/2-nrow(im3@.Data[,,1])/2,0)
  G@.Data[a:(a-1+nrow(im3@.Data)),(66):722,]=im3@.Data

}
if(nc<nr){
  im3= resize_image(im3,w=desX,plot = F)
  a=round(desY/2-ncol(im3@.Data[,,1])/2,0)
  G@.Data[38:673,a:(a-1+ncol(im3@.Data)),]=im3@.Data
}

plot_image(G)

}



im4= rotate_image(edit_image(im,brightness = Brilho,contrast = Contraste,gamma = gamma,plot = F),angle = rotate,plot=F)

if(Verbose==TRUE){
  print(paste0("Brightness = ",Brilho))
  print(paste0("Contrast =", Contraste))
  print(paste0("Gamma =", gamma))
  print(paste0("Rotate =", rotate))
}


return(im4)
}


print.edit_imageGUI=function(x,...){
  if(EBImage::is.Image(x)){cat("Is an image object","\n")}
  if(is.matrix(x)){cat("Is an matrix object","\n")}
  cat("Dimensions of Object:",dim(x@.Data),"\n")
}
