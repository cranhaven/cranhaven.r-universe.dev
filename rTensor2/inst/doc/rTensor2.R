## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rTensor)
library(rTensor2)

## -----------------------------------------------------------------------------
A <- rand_tensor(modes=c(3,4,2))
B <- rand_tensor(modes=c(4,5,2))
tmult(x=A,y=B,"dct")

## -----------------------------------------------------------------------------
A <-  rand_tensor(modes=c(3,3,2))
Ainv = tINV(A,"dct")
tmult(A,Ainv,"dct") # the result is an identity tensor

## -----------------------------------------------------------------------------
A <-  rand_tensor(modes=c(3,5,2))
A
t_tpose(A,"dct")

## -----------------------------------------------------------------------------
A = rand_tensor(modes=c(3,3,2))
result = tEIG(A,"dst")
A - tmult(tmult(result$P,result$D,"dst"),tINV(result$P,"dst"),"dst") # zero tensor

## -----------------------------------------------------------------------------
A <- rand_tensor(modes=c(3,3,2))
result <- tLU(A,"dht")
A - tmult(result$L,result$U,"dht")

## -----------------------------------------------------------------------------
A <- rand_tensor(modes=c(3,3,2))
result <- tQR(A,"fft")
A - tmult(result$Q,result$R,"fft")

## -----------------------------------------------------------------------------
library(raster)
library(grid)
data(raytrace)

tform = "dst"

A = raytrace$boat
wSVD = tSVD(A,tform)

k = 30 # number of singular values kept
U = wSVD$U
V = wSVD$V
S = wSVD$S
tV = t_tpose(V,tform)

Uk = rand_tensor(modes = c(128, k, 128), drop = FALSE)*0
Sk = rand_tensor(modes = c(k, k, 128), drop = FALSE)*0
Vk = rand_tensor(modes = c(k, 128, 128), drop = FALSE)*0

Uk = U[,1:k,]@data

Sk = S[1:k,1:k,]@data

Vk = tV[1:k,,]@data

Uk = as.tensor(Uk)
Vk = as.tensor(Vk)
Sk = as.tensor(Sk)

X = tmult(Uk, Sk,tform)
X = tmult(X, Vk, tform)

# See how close the compressed image is to the original image
fnorm(X-A)

# feature scale
if (tform=="fft"){
  Xnew=Re(X@data)
} else {
  Xnew = X@data
}
Xnew = X@data
newX = (Xnew-min(Xnew))/(max(Xnew)-min(Xnew))

# View Images

# Compressed image
# grid.raster(newX[,45,])
# title(paste0('Compressed'))
# Original Image
# grid.raster(XT[,45,]@data)

