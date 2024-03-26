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
end=example_image(11)
im=read_image(end,plot=TRUE)

## -----------------------------------------------------------------------------
plot_indexes(im,NumberCores=2)

## -----------------------------------------------------------------------------
#Criando o objeto com o índice
im2=gray_scale(im,method = "ExG",plot=T)


## -----------------------------------------------------------------------------
max(c(im2@.Data))
im3=segmentation(im2,treshold = "otsu",selectHigher = FALSE,plot=TRUE)


## -----------------------------------------------------------------------------
im4=measure_image(im3)
#Numero de objetos
im4$ObjectNumber

#Tamanho em pixels de cada objeto
im4$measures[,3]

## -----------------------------------------------------------------------------
im4=measure_image(im3,noise=50)
im4$measures[,1:3]

#Numero de objetos
im4$ObjectNumber

## -----------------------------------------------------------------------------
plot_meansures(im,measurements = im4,pch=1,
               cex =2 )

