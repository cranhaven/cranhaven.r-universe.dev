## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
 library(EBImage)
library(ExpImage)

## -----------------------------------------------------------------------------

end1=example_image(10)
im=read_image(end1,plot=TRUE)


## -----------------------------------------------------------------------------
#Selecionando o melhor indice para a segmentacao
r=gray_scale(im,method = "r",plot=T)
g=gray_scale(im,method = "g",plot=T)
b=gray_scale(im,method = "b",plot=T)

## -----------------------------------------------------------------------------
plot_indexes(im,NumberCores=2)

## -----------------------------------------------------------------------------
plot_image(b,col = 3)
Seg=segmentation(b,treshold = 0.65,selectHigher = TRUE,fillHull = TRUE,plot = TRUE)

## -----------------------------------------------------------------------------
radicula=thinning_image(Seg,plot = TRUE)

#Obtenção do comprimento da raiz em pixels
sum(radicula)

## -----------------------------------------------------------------------------
im2=mask_pixels(im,TargetPixels=radicula==1,col.TargetPixels = "red",plot=F)
im3=join_image(im,im2,plot=T)

## -----------------------------------------------------------------------------
plot_image(b,col = 3)
Seg2=segmentation(b,treshold = 0.5,selectHigher = FALSE,fillHull = TRUE,plot=TRUE)

## -----------------------------------------------------------------------------
Seg2b=EBImage::erode(Seg2)
EBImage::display(Seg2b)    

## -----------------------------------------------------------------------------
PA=thinning_image(Seg2b,plot = TRUE)

#Obtenção do comprimento da raiz em pixels
sum(PA)

## -----------------------------------------------------------------------------
im4=mask_pixels(im,TargetPixels=PA==1,plot=F,)
im4=join_image(im,im4,plot=T)

