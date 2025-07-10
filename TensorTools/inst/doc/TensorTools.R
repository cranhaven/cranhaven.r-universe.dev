## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TensorTools)

## -----------------------------------------------------------------------------
A <- t_rand(modes=c(3,4,5))
A$modes
A$data

## -----------------------------------------------------------------------------
A <-t_rand(modes=c(3,4,2))
B <- t_rand(modes=c(4,5,2))
tmult(x=A,y=B,"dct")

## -----------------------------------------------------------------------------
A <-  t_rand(modes=c(3,3,2))
Ainv = tINV(A,"dct")
tmult(A,Ainv,"dct") # the result is an identity tensor

## -----------------------------------------------------------------------------
A <-  t_rand(modes=c(3,5,2))
A
t_tpose(A,"dct")

## -----------------------------------------------------------------------------
A = t_rand(modes=c(3,3,2))
result = tEIG(A,"dst")
A$data-tmult(tmult(result$P,result$D,"dst"),tINV(result$P,"dst"),"dst")$data # zero tensor

## -----------------------------------------------------------------------------
A <- t_rand(modes=c(3,3,2))
result <- tLU(A,"dht")
A$data-tmult(result$L,result$U,"dht")$data

## -----------------------------------------------------------------------------
A <- t_rand(modes=c(3,3,2))
result <- tQR(A,"fft")
A$data-tmult(result$Q,result$R,"fft")$data

## -----------------------------------------------------------------------------
library(raster)
library(grid)
data(raytrace)

tform = "dct"

A = raytrace$boat
wSVD = tSVD(A,tform)

k = 30 # number of singular values kept
U = wSVD$U
V = wSVD$V
S = wSVD$S
tV = t_tpose(V,tform)

Uk = t_rand(modes = c(128, k, 128))
Sk = t_rand(modes = c(k, k, 128))
Vk = t_rand(modes = c(k, 128, 128))

Uk = U$data[,1:k,]

Sk = S$data[1:k,1:k,]

Vk = tV$data[1:k,,]

Uk = as.Tensor(Uk)
Vk = as.Tensor(Vk)
Sk = as.Tensor(Sk)

X = tmult(Uk, Sk,tform)
X = tmult(X, Vk, tform)

# See how close the compressed image is to the original image
fnorm(X$data-A$data)

# feature scale
if (tform=="fft"){
  Xnew=Re(X$data)
} else {
  Xnew = X$data
}
Xnew = X$data
newX = (Xnew-min(Xnew))/(max(Xnew)-min(Xnew))

# View Images

# Compressed image

## -----------------------------------------------------------------------------
library(raster)
library(grid)
data(raytrace)

tform = "dct"

A = raytrace$boat
wSVD = tSVD(A,tform)

k = 30 # number of singular values kept
U = wSVD$U
V = wSVD$V
S = wSVD$S
tV = t_tpose(V,tform)

Uk = t_rand(modes = c(128, k, 128))
Sk = t_rand(modes = c(k, k, 128))
Vk = t_rand(modes = c(k, 128, 128))

Uk = U$data[,1:k,]

Sk = S$data[1:k,1:k,]

Vk = tV$data[1:k,,]

Uk = as.Tensor(Uk)
Vk = as.Tensor(Vk)
Sk = as.Tensor(Sk)

X = tmult(Uk, Sk,tform)
X = tmult(X, Vk, tform)

# See how close the compressed image is to the original image
fnorm(X$data-A$data)

# feature scale
if (tform=="fft"){
  Xnew=Re(X$data)
} else {
  Xnew = X$data
}
Xnew = X$data
newX = (Xnew-min(Xnew))/(max(Xnew)-min(Xnew))

# View Images

# Compressed image
grid.raster(newX[,45,])

## -----------------------------------------------------------------------------
# Original Image
grid.raster(raytrace$boat$data[,45,])

## -----------------------------------------------------------------------------
data(Mnist)
T <- Mnist$train$images

## -----------------------------------------------------------------------------
myorder <- order(Mnist$train$labels)
T_sorted <- T$data[,myorder,]

## -----------------------------------------------------------------------------
T <- T_sorted[,c(1:2,1001:1002,2001:2002,3001:3002,4001:4002,5001:5002,6001:6002,7001:7002,8001:8002,9001:9002),]
tLDA(as.Tensor(T),10,2,"dct")

