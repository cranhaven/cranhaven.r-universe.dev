#' Funcao para a segmentacao de imagens.
#'
#'@description Esta funcao possibilita a segmentacao de imagens por meio de do
#'  ajuste de um modelo linear generalizado com a funcao logit de ligacao.
#'@usage  segmentation_logitGUI(im,return="image",sample=2000,
#'  fillHull=FALSE,TargetPixels="all")

#'@param im    :Este objeto deve ser obrigatoriamente uma imagem colorida (RGB)
#'  no formato do EBImage).
#' @param return Texto indicando o objeto a ser exportado pela funcao. Para
#'  este argumento podemos considerar: \cr
#'    "image" = sera exportada uma matriz referente a imagem segmentada.\cr
#'    "model" = Sera exportado o modelo para a predicao.\cr
#'@param sample    : Deve ser um valor numerico indicando quantos pixels dos
#'  imagens do foreground e do background serao utilizados no ajuste do modelo
#'  logit. O valor a ser escolhido deve ser inferior ou igual ao numero de
#'  pixels contidos nas paletas de cores.
#'@param fillHull    :Este argumento deve receber a palavra TRUE quando se
#'  pretende desconsiderar valores vazios dentro do foreground, caso contrario
#'  FALSE.
#'@param TargetPixels    :Quando se pretende segmentar todos os pixeis da imagem
#'  deve considerar a palavra "all" (Default). Se a segmentacao deva ser feita
#'  apenas para um conjunto de pixels, estes devem ser apresentados em uma
#'  matriz contendo o valor 1 para os pixels de interesse e 0 para os demais.
#@param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#  imagem segmentada.
#'@return Imagem segmentada
#'@seealso  \code{\link{glm}} ,\code{\link{segmentation}}
#'@author Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@export
#' @examples
#'\dontrun{
#'im=read_image(example_image(7),plot=TRUE)
#'segmentation_logitGUI(im)
#'}




segmentation_logitGUI=function(im,return="image",sample=2000,
                               fillHull=FALSE,TargetPixels="all"){
D=NULL
bk=FG="y"
plot(im)
p=recordPlot()

if(is.matrix(TargetPixels)){
  im2=im
im2@.Data[,,1][(TargetPixels==0)|isFALSE(TargetPixels)]=0
im2@.Data[,,2][(TargetPixels==0)|isFALSE(TargetPixels)]=0
im2@.Data[,,3][(TargetPixels==0)|isFALSE(TargetPixels)]=1
im=im2}

aa=0
while((bk=="y")|(FG=="y")){

  if(aa==0){plot_image(im)}
  if(aa==1){print(p)}

  aa=1
  #Select background

if(bk=="y"){
print("Select the background (Selecione o background)")
  c0=locator(type = "p", n = 2, col = "red", pch = 16)

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
      D=rbind(D,cbind(c(im2@.Data[,,1]),c(im2@.Data[,,2]),c(im2@.Data[,,3]),0))

    }

    if(sum(c[1,]==c[2,])==2){

      D=rbind(D,cbind(c(im@.Data[c[1,1],c[1,2],1]),c(im@.Data[c[1,1],c[1,2],2]),c(im@.Data[c[1,1],c[1,2],3]),0))

    }
}

##################################################################
if(FG=="y"){
     print("Select the foreground (Selecione o foreground)")
    c0=locator(type = "p", n = 2, col = "blue", pch = 16)
     c= cbind(c0$x,c0$y)
     if(sum(c[1,]==c[2,])<2){
       c1=c[1,]
       c3=c[2,]
       c2=c(c1[1],c3[2])
       c4=c(c3[1],c1[2])


       lines(rbind(c1,c2),col="blue")
       lines(rbind(c2,c3),col="blue")
       lines(rbind(c3,c4),col="blue")
       lines(rbind(c1,c4),col="blue")

       w=round(min(c[,1]),0):round(max(c[,1]),0)
       h=round(min(c[,2]),0):round(max(c[,2]),0)

       im2=crop_image(im,w=w,h=h,plot=F)
       D=rbind(D,cbind(c(im2@.Data[,,1]),c(im2@.Data[,,2]),c(im2@.Data[,,3]),1))

     }

     if(sum(c[1,]==c[2,])==2){

       D=rbind(D,cbind(c(im@.Data[c[1,1],c[1,2],1]),c(im@.Data[c[1,1],c[1,2],2]),c(im@.Data[c[1,1],c[1,2],3]),1))
     }
}

 n= min(c(tapply(D[,1],D[,4],length),sample))

 D1=D[D[,4]==1,]
 D0=D[D[,4]==0,]

 D1=D1[sample(1:nrow(D1),size=n),]
 D0=D0[sample(1:nrow(D0),size=n),]




 back_fore=data.frame(rbind(D1,D0))

  colnames(back_fore)=c("R","G","B","Y")
  modelo1 <- suppressWarnings(glm(Y ~ R + G + B, family = binomial("logit"),
                                  data = back_fore))


  if(isFALSE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1]),G=c(im@.Data[,,2]),B=c(im@.Data[,,3]))
    pred1 <- round((predict(modelo1, newdata = imagem, type = "response")), 0)
    ImagemSeg <- matrix(pred1, ncol = ncol(im@.Data[,,1]))
  }

  if(isTRUE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1][TargetPixels==1]),G=c(im@.Data[,,2][TargetPixels==1]),B=c(im@.Data[,,3][TargetPixels==1]))
    pred1 <- round(predict(modelo1, newdata = imagem, type = "response"), 0)
    ImagemSeg=TargetPixels*0
    ImagemSeg[TargetPixels==1]=pred1
print(TargetPixels==1)
  }


  if(fillHull==TRUE){ImagemSeg=EBImage::bwlabel(ImagemSeg);ImagemSeg=EBImage::fillHull(ImagemSeg)}


  ImagemSeg=(ImagemSeg>0)*1

  p=recordPlot()
  par(mfrow=c(1,2))
plot_image((im))
fgh=mask_pixels(im,TargetPixels =ImagemSeg==1,col.TargetPixels = "red")
plot_image(fgh)
par(mfrow=c(1,1))
bk=readline(prompt = "Do you want to select more background (Deseja selecionar mais background)? (y/n) ")
if(sum((bk!="y"),(bk!="n"))==2){
  while(sum((bk!="y"),(bk!="n"))==2){
    message("A resposta deve ser 'y' ou 'n'")
    bk=readline(prompt = "Do you want to select more background (Deseja selecionar mais background)? (y/n) ")

  }

}


FG=readline(prompt = "Do you want to select more foreground (Deseja selecionar mais foreground)? (y/n) ")
if(sum((FG!="y"),(FG!="n"))==2){
  while(sum((FG!="y"),(FG!="n"))==2){
    message("The answer must be 'y' or 'n' (A resposta deve ser 'y' ou 'n')")
    FG=readline(prompt = "Do you want to select more foreground (Deseja selecionar mais foreground)? (y/n) ")
  }

}

}

if(return=="image") {return(ImagemSeg)}
if(return=="model") {return(modelo1)}

}
