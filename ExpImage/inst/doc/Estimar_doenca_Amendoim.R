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
im=read_image("https://raw.githubusercontent.com/AlcineiAzevedo/CursosImagem/main/amendoim.jpg",plot=TRUE)

## -----------------------------------------------------------------------------

r=gray_scale(im,method = "r",plot=T)
g=gray_scale(im,method = "g",plot=T)
b=gray_scale(im,method = "b",plot=T)

## -----------------------------------------------------------------------------
MatrizSegmentada0.3=segmentation(g,treshold = 0.30,fillHull = F,selectHigher = F,plot=T)
MatrizSegmentada0.4=segmentation(g,treshold = 0.40,fillHull = F,selectHigher = F,plot=T)
MatrizSegmentada0.6=segmentation(g,treshold = 0.60,fillHull = F,selectHigher = F,plot=T)
MatrizSegmentada0.8=segmentation(g,treshold = 0.80,fillHull = F,selectHigher = F,plot=T)

## -----------------------------------------------------------------------------
100*(sum(MatrizSegmentada0.3)/sum(MatrizSegmentada0.8))

## -----------------------------------------------------------------------------
im3=mask_pixels(im,TargetPixels=MatrizSegmentada0.3==1,col.TargetPixels = "red",plot=F)

im3b=mask_pixels(im,TargetPixels=MatrizSegmentada0.3==1,Contour =TRUE,col.TargetPixels = "red",plot=F)

im4=join_image(im,im3,im3b,plot=T)

