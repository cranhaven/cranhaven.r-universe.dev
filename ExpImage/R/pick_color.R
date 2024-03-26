#'Selecionar os valores de RGB em pixels (Selecting RGB values in pixels).
#'
#'@description Esta funcao retorna o valor de R, G e B no pixel selecionado.
#' (This function returns the value of R, G and B at the selected pixel ).
#'@usage pick_color(im, matrix = NULL)
#'@param im Este objeto deve conter uma imagem no formato do EBImage (This
#'  object must contain an image in EBImage format ).
#'@param matrix Deve ser uma matriz com 3 colunas correspondentes aos canais
#'  R, G e B. Os novos valores selecionados serao adicionados a essa matriz.
#'  (It must be a matrix with 3 columns corresponding to the R, G and B channels.
#'   The new selected values will be added to this matrix).
#'@author Alcinei Mistico Azevedo (Instituto de ciencias agrarias da UFMG)
#'@return Esta funcao retorna o valor de R, G e B no pixel selecionado.
#' (This function returns the value of R, G and B at the selected pixel ).
#'@seealso  \code{\link{segmentation_logit}}

#'@importFrom stats binomial glm predict
#'@importFrom grDevices dev.off  jpeg
#'@export
#' @examples
#'\dontrun{
#'#library(ExpImage)
#'#library(EBImage)
#'#Carregar imagem de exemplo
#'im=read_image(example_image(2),plot=TRUE)
#'pick_color(im)
#'}


#'




pick_color=function(im,matrix=NULL){
  print("Clique sobre a imagem (Click on the image)")
plot_image(im)
a=0
D=matrix
c0=1
nn=0
while(is.null(c0)==FALSE){




  c0=locator(type = "p", n = 2, col = "red", pch = 16)
  #print(c0)

 if(is.null(c0)==FALSE){

  c= cbind(c0$x,c0$y)
if(sum(c[1,]==c[2,])<2){
 c1=c[1,]
    c3=c[2,]
    c2=c(c1[1],c3[2])
    c4=c(c3[1],c1[2])


    lines(rbind(c1,c2),col="red")
    lines(rbind(c2,c3),col="red")
    lines(rbind(c3,c4),col="red")
    lines(rbind(c1,c4),col="red")

    w=round(min(c[,1]),0):round(max(c[,1]),0)
    h=round(min(c[,2]),0):round(max(c[,2]),0)

    im2=crop_image(im,w=w,h=h,plot=F)
    nn=nn+1
    D=rbind(D,cbind(R=c(im2@.Data[,,1]),G=c(im2@.Data[,,2]),B=c(im2@.Data[,,3]),Obj=nn))

}

if(sum(c[1,]==c[2,])==2){
  nn=nn+1
  D=rbind(D,cbind(R=c(im@.Data[c[1,1],c[1,2],1]),G=c(im@.Data[c[1,1],c[1,2],2]),B=c(im@.Data[c[1,1],c[1,2],3]),Obj=nn))

}






 }
}
colnames(D)=c("R","G","B","Obj")
#class(D)="pick_color"

cor=aggregate(D[,-4],by=list(D[,4]),mean)

return(list(Colors=convert_color(cor[,-1]),RGB=D))
}
