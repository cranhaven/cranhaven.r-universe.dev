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
end=example_image(1)
im=read_image(end,plot=TRUE)

## -----------------------------------------------------------------------------
##Diminuir a resolucao (tamanho da imagem)
im2=resize_image(im,w=1000,plot=TRUE)

##Cortar Imagem
im3=crop_image(im2,w =200:650,h=100:450,plot = TRUE)

##Aumentar brilho
im4=edit_image(im3,brightness = 0.1,plot = TRUE)

#Aumentar contraste
im5=edit_image(im4,contrast = 1.2,plot = TRUE)

#Aumentar gamma
im6=edit_image(im5,gamma  = 1.1,plot = TRUE)


#Alterando brilho, contraste e gamma
imb=edit_image(im3,brightness = 0.1,contrast = 1.7,gamma  = 1.2,plot = TRUE)
imb

## -----------------------------------------------------------------------------
#Extração da banda
r=gray_scale(imb,method = "r",plot=TRUE)

#Segmentação
seg=segmentation(r,treshold =0.2,selectHigher = TRUE,fillHull = T,plot = TRUE )

#Remoção do background
imc=extract_pixels(imb,target = seg,valueTarget = 1,valueSelect = c(0,1,0),plot = TRUE)

