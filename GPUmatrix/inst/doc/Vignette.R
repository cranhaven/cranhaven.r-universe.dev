## ----eval=F-------------------------------------------------------------------
#  install.packages("torch")
#  library(torch)
#  install_torch() # In some cases is required.

## ----eval=F-------------------------------------------------------------------
#  install.packages("tensorflow")
#  library(tensorflow)
#  install_tensorflow(version = "nightly-gpu")

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("GPUmarix")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github(" ceslobfer/GPUmatrix")

## ----eval=T-------------------------------------------------------------------
library(GPUmatrix)
if (installTorch()) {
  #R matrix initialization
  m <- matrix(c(1:20)+40,10,2)
  #Show CPU matrix
  m
  #GPU matrix initialization
  Gm <- gpu.matrix(c(1:20)+40,10,2)
  #Show GPU matrix
  Gm
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
Gm[c(2,3),1]

Gm[,2]
 
Gm2 <- cbind(Gm[c(1,2),], Gm[c(6,7),])
Gm2

Gm2[1,3] <- 0
Gm2
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
Gm3 <- gpu.matrix(nrow = 2,ncol=3)
Gm3[,2]
Gm3[1,2] <- 1 
Gm3
Gm3[1,3] <- 0
Gm3
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
#GPUmatrix initialization with CPU option   
Gm <- gpu.matrix(c(1:20)+40,10,2,device="cpu")   
#Show CPU matrix from GPUmatrix   
Gm 
}

## ----eval=F-------------------------------------------------------------------
#  # library(GPUmatrix)
#  tensorflowGPUmatrix <- gpu.matrix(c(1:20)+40,10,2, type = "tensorflow") tensorflowGPUmatrix

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(NULL,caption = "Table 1. Cast options from other packages. If back cast is TRUE, then it is possible to convert a gpu.matrix to this object and vice versa. If is FALSE, it is possible to convert these objects to gpu.matrix but not vice versa.")

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
m <- matrix(c(1:10)+40,5,2)
Gm <- gpu.matrix(m)
Gm
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
library(Matrix)
M <- Matrix(c(1:10)+40,5,2)
Gm <- gpu.matrix(M)
Gm
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
library(float)
mfloat32 <- fl(m)
Gm <- gpu.matrix(mfloat32)
Gm
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
Ms <- Matrix(sample(0:1, 10, replace = TRUE), nrow=5, ncol=2, sparse=TRUE)
Ms
 
Gms <- gpu.matrix(Ms)
Gms
}

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
#Creating a float32 matrix
Gm32 <- gpu.matrix(c(1:20)+40,10,2, dtype = "float32")
Gm32

#Creating a non sparse martix with data type float32 from a sparse matrix type float64
Ms <- Matrix(sample(0:1, 20, replace = TRUE), nrow=10, ncol=2, sparse=TRUE)
Gm32 <- gpu.matrix(Ms, dtype = "float32", sparse = F)
Gm32
 
#Convert Gm32 in sparse matrix Gms32
Gms32 <- to_sparse(Gm32)
Gms32

##Convert data type Gms32 in float64
Gms64 <- Gms32
dtype(Gms64) <- "float64"
Gms64
}


## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
(Gm + Gm) == (m + m)

(Gm + M) == (mfloat32 + Gm)

(M + M) == (mfloat32 + Gm)

(M + M) > (Gm + Gm)*2
}

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(NULL,caption = "Table 2. Mathematical operators that accept a gpu.matrix as input")

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(NULL,caption = "Table 3. Complex operators that accept a gpu.matrix with complex type data as input")

## ----eval=T-------------------------------------------------------------------
if (installTorch()) {
m <- matrix(c(1:20)+40,10,2)
Gm <- gpu.matrix(c(1:20)+40,10,2)

head(tcrossprod(m),1)

head(tcrossprod(Gm),1)

Gm <- tail(Gm,3)
rownames(Gm) <- c("a","b","c")
tail(Gm,2)

colMaxs(Gm)
}

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(NULL,caption = "Table 4. Functions that accept one or several gpu.matrix matrices as input")

## ----message=FALSE, warning=FALSE---------------------------------------------
# Standard glm
if (installTorch()) {
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
summary(glm.D93)


# Using speedglm
# library(speedglm)
# sglm.D93 <- speedglm(counts ~ outcome + treatment, family = poisson())
# summary(sglm.D93)

# GPU glm
library(GPUmatrix)
gpu.glm.D93 <- GPUglm(counts ~ outcome + treatment, family = poisson())
summary(gpu.glm.D93)
}


