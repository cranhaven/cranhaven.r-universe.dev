## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(EBImage)
library(ExpImage)

## -----------------------------------------------------------------------------
#######################################################
#Abrir imagem das folhas
end1=example_image(3)
im=read_image(end1,plot=TRUE)

#Abrir paleta de cores do fundo
end2=example_image(4)
fundo=read_image(end2,plot=TRUE)


#Abrir paleta de cores das folhas
end3=example_image(5)
folhas=read_image(end3,plot=TRUE)

#Abrir paleta de cores referência
end4=example_image(6)
ref=read_image(end4,plot=TRUE)


## -----------------------------------------------------------------------------
#################################################################
#Segmentacao para separar as folhas do restante
folhas.seg=segmentation_logit(im,foreground=folhas,background=list(fundo,ref),sample=2000,fillHull=TRUE,plot=TRUE)

#Segmentacao para separar o objeto de referencia do restante
ref.seg=segmentation_logit(im,foreground=ref,background=list(fundo,folhas),sample=2000,fillHull=TRUE,plot=TRUE)

## -----------------------------------------------------------------------------
medidas=measure_image(folhas.seg,noise = 1000)
#numero de objetos e medias
medidas

## -----------------------------------------------------------------------------
#Convertendo a area dos objetos para cm2

#Identificando a area do objeto de referência (maior area)
# A area conhecida do objeto de referência tem 8.5 x 5.5 cm 
#e sua areasegmentada esta no objeto ref.seg

medidasref=measure_image(img = folhas.seg,noise =1000,id=ref.seg,length =8.5,width =5.5 )
#numero de objetos e medias
medidasref

## -----------------------------------------------------------------------------
#Plotar resultados das areas em pixel e salvar em imagem jpg
plot_meansures(im,medidasref$measures[,1],coordy=medidasref$measures[,2],text=round(medidasref$measures[,3],1),col="red",
               cex = 0.9 ,plot=TRUE)

