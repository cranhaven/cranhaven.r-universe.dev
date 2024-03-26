#' Funcao para a segmentacao de imagens.
#'
#'@description Esta funcao possibilita a segmentacao de imagens por meio de do
#'  ajuste de um modelo linear generalizado com a funcao logit de ligacao.
#'@usage  segmentation_logit(im,foreground,background,return="image",
#'    sample=2000, fillHull=TRUE,TargetPixels="all",plot=TRUE)

#'@param im    :Este objeto deve ser obrigatoriamente uma imagem colorida (RGB)
#'  no formato do EBImage).
#'@param foreground    : Deve ser uma imagem correspondende a paleta de cores do
#'  objeto que se pretende segmentar. Caso haja mais de uma paleta de cores,
#'  suas reespectivas imagens devem ser colocadas dentro de um objeto do tipo
#'  list. Cada paleta de cor desve estar no formato de imagens do EBImage.
#'@param background    : Deve ser uma imagem correspondende a paleta de cores
#'  com os tons do fundo. Caso haja mais de uma paleta de cores, suas
#'  reespectivas imagens devem ser colocadas dentro de um objeto do tipo list.
#'  Cada paleta de cor desve estar no formato de imagens do EBImage.
#'
#'@param return Texto indicando o objeto a ser exportado pela funcao. Para
#'  este argumento podemos considerar: \cr
#'    "image" = sera exportada uma matriz referente a imagem segmentada.\cr
#'    "model" = Sera exportado o modelo para a predicao.\cr
#'
#'
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
#'@param plot    :Indica se sera apresentada (TRUE) ou nao (FALSE) (default) a
#'  imagem segmentada.
#'@return Imagem segmentada
#'@seealso  \code{\link{glm}} ,\code{\link{segmentation}}
#'@author Alcinei Mistico Azevedo (Instituto de Ciencias Agrarias da UFMG)
#'@export
#' @examples

#'\donttest{
#'
#' #################################################################
#' #Estimar a area foliar usando um objeto de referencia.
#' ##################################################################
#'   #ativar pacote
#'   #library(ExpImage)
#'   #######################################################
#'   #Abrir imagem das folhas
#'   im=read_image(example_image(3))
#'   plot_image(im)
#'   #Abrir paleta de cores do fundo
#'   fundo=read_image(example_image(4))
#'   plot_image(fundo)
#'   #Abrir paleta de cores das folhas
#'   folhas=read_image(example_image(5))
#'   plot_image(folhas)
#'   #Abrir paleta de cores referencia
#'   ref=read_image(example_image(6))
#'   #Ver a imagem
#'   plot_image(ref)
#'
#'   #################################################################
#'   #Segmentacao para separar as folhas do restante
#'   folhas.seg=segmentation_logit(im,foreground=folhas,
#'   background=list(fundo,ref),sample=2000,fillHull=TRUE,plot=TRUE)
#'
#'   #Segmentacao para separar o objeto de referencia do restante
#'   ref.seg=segmentation_logit(im,foreground=ref,
#'   background=list(fundo,folhas),sample=2000,fillHull=TRUE,plot=TRUE)
#'
#'   #Identificar area de cada folha
#'
#'   medidas=measure_image(folhas.seg,noise = 1000)
#'   #numero de objetos e medias
#'   medidas
#'
#'   #Plotar resultados das areas em pixel e salvar em imagem jpg
#'   plot_meansures(im,medidas$measures[,1],coordy=medidas$measures[,2],
#'   text=round(medidas$measures[,3],1),col="blue",cex = 0.9,pathSave ="none" ,plot=TRUE)
#'   }



segmentation_logit=function(im=NULL,foreground,background,return="image", sample=2000,fillHull=TRUE,
                            TargetPixels="all",plot=TRUE){


  #foreground=list(folhas,ref)
  #background=list(fundo)


  if(sum((class(foreground)=="matrix")|(class(foreground)=="pick_color"))>0){
    foreground=EBImage::as.Image(array(foreground,dim = c(nrow(foreground),1,3)))
    EBImage::colorMode(foreground)=2
  }

  if(sum((class(background)=="matrix")|(class(background)=="pick_color"))>0){
    background=EBImage::as.Image(array(background,dim = c(nrow(background),1,3)))
    EBImage::colorMode(background)=2
  }

  if(isFALSE(is.list(foreground))){foreground=list(foreground)}
  if(isFALSE(is.list(background))){background=list(background)}


  if(is.list(foreground)){
    fore=NULL
    for(i in 1:length(foreground)){
      imm=foreground[[i]]
      if(isFALSE( EBImage::is.Image(imm))){message("All images used must have 3 bands and be in EBImage format (Todas as imagens utilizadas devem ter 3 bandas e estar no formato do EBImage )")}
      n=ncol(imm@.Data)*ncol(imm@.Data)
      if(sample>n){message("One of the foregrounds images has a smaller number of pixels than the established sample (Uma das imagens tem o numero de pixels inferior ao da amostra estabelecida)")
        ;sample2=n}
      if(sample<=n){sample2=sample}

      id=sample(1:n)[1:sample2]
      fore=rbind(fore,cbind(r=c(imm@.Data[,,1])[id],g=c(imm@.Data[,,2])[id],b=c(imm@.Data[,,3])[id]))
    }
  }

  if(is.list(background)){
    back=NULL
    for(i in 1:length(background)){
      imm=background[[i]]
      if(isFALSE( EBImage::is.Image(imm))){
        message("All images used must have 3 bands and be in EBImage format
              (Todas as imagens utilizadas devem ter 3 bandas e estar no formato do EBImage )")}
      n=ncol(imm@.Data)*ncol(imm@.Data)
  if(sample>n){message("One of the backgrounds images has a smaller number of pixels than the established sample
                     (Uma das imagens tem o numero de pixels inferior ao da amostra estabelecida)");sample2=n}
      if(sample<=n){sample2=sample}

      id=sample(1:n)[1:sample2]
      back=rbind(back,cbind(r=c(imm@.Data[,,1])[id],g=c(imm@.Data[,,2])[id],b=c(imm@.Data[,,3])[id]))
    }
  }

  back_fore=data.frame(rbind(cbind(fore,1),cbind(back,0)))

  colnames(back_fore)=c("R","G","B","Y")
  modelo1 <- suppressWarnings(glm(Y ~ R + G + B, family = binomial("logit"),
                                  data = back_fore))
if(!is.null(im)){
#print(modelo1)
  if(isFALSE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1]),G=c(im@.Data[,,2]),B=c(im@.Data[,,3]))
    pred1 <- round(
      suppressWarnings(predict(modelo1, newdata = imagem, type = "response"))
      , 0)
    ImagemSeg <- matrix(pred1, ncol = ncol(im@.Data[,,1]))
  }

  if(!is.null(im)){
  if(isTRUE(is.matrix(TargetPixels))){
    imagem=data.frame(R=c(im@.Data[,,1][TargetPixels]),G=c(im@.Data[,,2][TargetPixels]),B=c(im@.Data[,,3][TargetPixels]))
    pred1 <- round(predict(modelo1, newdata = imagem, type = "response"), 0)
    ImagemSeg=TargetPixels*0
    ImagemSeg[TargetPixels==1]=pred1

  }


  if(fillHull==TRUE){ImagemSeg=EBImage::bwlabel(ImagemSeg);ImagemSeg=EBImage::fillHull(ImagemSeg)}


  ImagemSeg=EBImage::as.Image((ImagemSeg>0)*1)
}


  if((plot==T)&(!is.null(im))){
    iim=mask_pixels(im = im,TargetPixels = ImagemSeg,
                    Contour =T,
                    col.TargetPixels = "red",plot = F)
    m = (ImagemSeg@.Data>0)*1
   # print(m)
    ImagemSeg2=IM3(m)
    a=join_image(list(im,ImagemSeg2),plot=T)
  }
  }
 if(return=="image") {return(ImagemSeg)}
  if(return=="model") {return(modelo1)}
}
