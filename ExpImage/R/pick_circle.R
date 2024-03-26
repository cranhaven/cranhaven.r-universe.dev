#' Segmentation with the shape of a circle by clicking on the image
#' (Segmentacao com o formato de um circulo clicando em na imagem)
#'
#' @description With this function it is possible to make a segmentation with the shape of a
#'  circle by clicking on the image (Com essa funcao e possivel fazer uma segmentacao com o
#'  formato de um circulo clicando em na imagem).

#' @usage pick_circle(im,num.points=NULL, col="red")

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

#' @export
#' @examples
#'
#'\dontrun{
#' im=read_image("https://raw.githubusercontent.com/AlcineiAzevedo/Files_ExpImage/main/PlacaPetri.jpg",
#' plot=TRUE)
#' seg=pick_circle(im)
#' im2=extract_pixels(im,target = seg,plot=TRUE)
#'
#'}

pick_circle=function(im,num.points=NULL, col="red"){
plot_image(im)
message("Click in the center of the circle (Clique no centro do circulo)")
centro=unlist(locator(type="p",n = 1, col=col,pch=19))

message("Click on the end of the circle (Clique na extremidade do circulo)")
ext=unlist(locator(type="p",n = 1, col=col,pch=19))

raio=sqrt(sum((centro-ext)^2))

if(is.null(num.points)) num.points=1000


x1 = seq(-1,1,l=num.points/2)
x2 = x1
y1 = sqrt(1 - x1^2)
y2 = (-1)*y1
x = c(x1,x2)*raio+centro[1]
y = c(y1,y2)*raio+centro[2]

coor=cbind(x,y)

coor


#coor=rbind(coor,coor[1,])
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
