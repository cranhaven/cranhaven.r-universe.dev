## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
 library(EBImage)
library(ExpImage)

## -----------------------------------------------------------------------------
# Obtendo o endereço da imagem de exemplo
end=example_image(2)
im=read_image(end,plot=TRUE)

## -----------------------------------------------------------------------------
##Diminuir a resolucao (tamanho da imagem)
im2=resize_image(im,w=1000,plot=TRUE)


## -----------------------------------------------------------------------------
#Selecionando o melhor indice para a segmentacao
r=gray_scale(im2,method = "r",plot=T)
g=gray_scale(im2,method = "g",plot=T)
b=gray_scale(im2,method = "b",plot=T)

## -----------------------------------------------------------------------------
plot_indexes(im,NumberCores=2)

## -----------------------------------------------------------------------------
MatrizSegmentada=segmentation(b,treshold = 0.20,fillHull = F,selectHigher = T,plot=T)
MatrizSegmentada=segmentation(b,treshold = 0.40,fillHull = F,selectHigher = T,plot=T)
MatrizSegmentada=segmentation(b,treshold = 0.60,fillHull = F,selectHigher = T,plot=T)
MatrizSegmentada=segmentation(b,treshold = 0.80,fillHull = F,selectHigher = T,plot=T)

## -----------------------------------------------------------------------------
MatrizSegmentada=segmentation(r,treshold = "otsu",fillHull = F,selectHigher = T,plot=T)

## -----------------------------------------------------------------------------
plot_image(b,col = 3)
MatrizSegmentada=segmentation(b,treshold = 0.4,fillHull = F,selectHigher = F,plot=T)


## -----------------------------------------------------------------------------
MatrizSegmentada=segmentation(b,treshold = 0.4,fillHull = T,selectHigher = F,plot=T)


## -----------------------------------------------------------------------------
im3=extract_pixels(im2,target =MatrizSegmentada,valueTarget =1,valueSelect = c(0,0,0),plot=T )

## -----------------------------------------------------------------------------
r=gray_scale(im3,method = "r",plot=T)
g=gray_scale(im3,method = "g",plot=T)
b=gray_scale(im3,method = "b",plot=T)

## -----------------------------------------------------------------------------
MatrizSegmentada2=segmentation(b,treshold = 0.50,fillHull = T,selectHigher = T,plot = T)


## -----------------------------------------------------------------------------
Medidas=measure_image(MatrizSegmentada2)
Medidas$ObjectNumber

## -----------------------------------------------------------------------------
im4=extract_pixels(im2,target =MatrizSegmentada2,valueTarget =0,valueSelect = c(1,0,0),plot=T )

## -----------------------------------------------------------------------------
im5=join_image(im2,im4,plot = TRUE)

