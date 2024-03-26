#' Segmenting objects manually (Segmentação de objetos manualmente)
#'
#' @description With this function it is possible to segment objects by clicking on their boundaries
#' (Com essa funcao e possivel segmentar objetos clicando em suas delimitacoes).

#' @usage pick_segmentation(im,num.points=NULL, col="red")

#' @param im    :This object must contain an image in EBImage format (Este
#'   objeto deve conter uma imagem no formato do EBImage).
#' @param num.points    :Maximum number of points to be used to delimit the object
#' (Número maximo de pontos a ser utilizado para delimitar o objeto).
#' @param col : Color of points and lines that delimit the object
#' (Cor dos pontos e linhas que delimitarão o objeto)
#'
#' @author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#' @return Returns an image with pixel values equal to 1(white) for the foreground and 0 (black)
#'  for the foreground (Retorna uma imagem com valores de pixel igual a 1( branco) para o foreground
#'   e 0 (preto) para o foreground).
#'
#' @seealso  \code{\link{segmentation}} , \code{\link{segmentation_logit}}, \code{\link{segmentation_logitGUI}}

#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'
#'\dontrun{
#'require(ExpImage)
#' im=read_image(example_image(3),plot=TRUE)
#' mask=pick_segmentation(im)
#'im2=extract_pixels(im,mask,plot = TRUE,valueTarget = 1)
#'crop_image(im2,segmentation = mask,plot=TRUE)
#'im3=extract_pixels(im,mask,plot = TRUE,valueTarget = 0)
#'}

pick_segmentation=function(im,num.points=NULL, col="red"){
plot(im)
message("Click circling the object to be segmented (Clique circulando o objeto a ser segmentado)")

stop=FALSE
if(is.null(num.points)) num.points=1000000
coor=NULL
a=0
while(isFALSE(stop)){
  if(a>1){
  if(nrow(coor)>1){
    lines(coor[(nrow(coor)-1):nrow(coor),1],coor[(nrow(coor)-1):nrow(coor),2],col=col)
  }
  }
  x=unlist(locator(type="p",n = 1, col=col,pch=19))
  if(is.null(x)) stop=TRUE
  coor=rbind(coor,x)
  a=a+1

  if(a>=num.points){stop=TRUE}
}
#lines(coor[c(1,nrow(coor)),1],coor[c(1,nrow(coor)),2],col=col)
coor


coor=rbind(coor,coor[1,])
#########################################################################
mat=NULL
for(i in 1:(nrow(coor)-1)){
c1=coor[i,]
c2=coor[i+1,]
  a=c1[2]
  b=(c2[2]-c1[2])/(c2[1]-c1[1])
Xs=round(c1[1],0):round(c2[1],0) -round(c1[1],0)
Ys=round(a+b*Xs,0)
mat=rbind(mat,cbind(Xs+round(c1[1],0),Ys))

lines(Xs+round(c1[1],0),Ys,col=col)
}
n=info_image(im)$Length
imF=matrix(0,n[1],n[2])



id=unique(mat[,1])

for(i in id){
  coorr=mat[mat[,1]==i,]
imF[i,min(coorr[,2],na.rm=T):max(coorr[,2],na.rm=T)]=1

}
imf2=EBImage::fillHull(EBImage::bwlabel(imF))
plot_image(imf2)

return(imf2)

}
