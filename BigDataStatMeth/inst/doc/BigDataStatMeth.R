## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(BiocStyle)

knitr::opts_chunk$set(collapse = TRUE, comment = "", cache = FALSE, message = FALSE, width = 180, crop = NULL)

## ----cleanup, echo=FALSE, include=FALSE---------------------------------------
if( isTRUE(file.exists('delayed.hdf5'))) {
    file.remove('delayed.hdf5')
}
if( isTRUE(file.exists('robject.hdf5'))){
    file.remove('robject.hdf5')
}
# if( isTRUE(file.exists('rna_file.hdf5'))){
#     file.remove('rna_file.hdf5')
# }

## ----install_required, eval=FALSE---------------------------------------------
# # Install BiocManager (if not previously installed)
# install.packages("BiocManager")
# 
# # Install required packages
# BiocManager::install(c("Matrix", "RcppEigen", "RSpectra",
#                        "HDF5Array", "rhdf5"))

## ----install, eval=FALSE------------------------------------------------------
# # Install devtools and load library (if not previously installed)
# install.packages("devtools")
# library(devtools)
# 
# # Install BigDataStatMeth
# install_github("isglobal-brge/BigDataStatMeth")

## ----load, cache=FALSE--------------------------------------------------------
library(rhdf5)
library(BigDataStatMeth)

## ----hdf5Img, out.width = '100%', fig.align = 'center', fig.cap = "HDF5 hierarchical structure", echo=FALSE----
knitr::include_graphics("imgs/hdf5_squema.jpg")

## ----hdf5Create---------------------------------------------------------------
library(rhdf5)

set.seed(5234)
n <- 500
m <- 600
A <- matrix(rnorm(n*m,mean=0,sd=1), n,m)

# We also can create a dataset from R matrix object
bdCreate_hdf5_matrix(filename = "robject.hdf5", 
                     object = A,
                     group = "INPUT", 
                     dataset = "A",
                     overwriteFile = TRUE)

## ----ls-----------------------------------------------------------------------
list.files(pattern = "*.hdf5")

## ----hdf5AddDataset-----------------------------------------------------------
set.seed(5234)
n <- 500
m <- 1000
A <- matrix(rnorm(n*m,mean=0,sd=1), n, m)

set.seed(5234)
n <- 1000
m <- 12000
B <- matrix(rnorm(n*m,mean=3,sd=0.5), n, m)

# Path to HDF5 file
example_fn <- "delayed.hdf5"

# We create another data file (delayed.hdf5) with a matrix A.
# The group is called INPUT, overwriteFile is set to true to 
# overwrite a file if exists
bdCreate_hdf5_matrix(filename = example_fn, 
                     object = A, 
                     group = "INPUT", 
                     dataset = "A", 
                     overwriteFile = TRUE)

# And them, we add another matrix B to the same group
bdCreate_hdf5_matrix(object = B, 
                filename = example_fn, 
                group = "INPUT", 
                dataset = "B")

## ----hdf5Show-----------------------------------------------------------------
# Examine hierarchy before open file
h5ls(example_fn)

## ----hdf5Open, cache=FALSE----------------------------------------------------
# Open file
h5fdelay <- H5Fopen(example_fn)
# Show hdf5 hierarchy (groups)
h5fdelay

## ----hdf5Dataset--------------------------------------------------------------
bdata <- h5fdelay$INPUT$B
bdata[1:3,1:5]

## ----hdf5DatasetClose---------------------------------------------------------
h5closeAll()

## ----convert_HDF5, cache=FALSE------------------------------------------------
import_hdf5 <- bdImportTextFile_hdf5(filename = "colesterol.csv",
                                     sep=',', 
                                     outputfile = "colesterol_file.hdf5", 
                                     outGroup = "COLESTEROL", 
                                     outDataset = "COLESTEROLDATA", 
                                     header = TRUE,
                                     overwrite = TRUE)


## ----read_data_col_HDF5-------------------------------------------------------

# Show content
h5ls(import_hdf5$fn)

# We can open the file and have access to the data
res_hdf5 <- h5read(import_hdf5$fn, import_hdf5$ds)
colnames_hdf5 <- h5read(import_hdf5$fn, import_hdf5$ds_cols)[,1]

# Show hdf5 content dataset
res_hdf5[1:5, 1:6]

# Show colnames 
head(colnames_hdf5)

# Show data with colnames
colnames(res_hdf5) <- colnames_hdf5
res_hdf5[1:5, 1:6]


## ----blockmult_hdf5_exec------------------------------------------------------

# Perform blockwise matrix multiplication
res <- bdblockmult_hdf5(filename = example_fn, group = "INPUT",
                        A = "A", B = "B", outgroup = "HDF5_RES")

# Show the content of the HDF5 file
h5ls(res$fn)

## ----blockmult_hdf5_res-------------------------------------------------------
# Extract the result from HDF5
result_hdf5 <- h5read(res$fn, res$ds)[1:3, 1:5]
result_hdf5

# Compute the same multiplication in R
result_r <- (A %*% B)[1:3, 1:5]
result_r

# Compare both results
all.equal((A %*% B), h5read(res$fn, res$ds))

## ----crossprod_sing-----------------------------------------------------------

# Create example matrices
set.seed(123)
A <- matrix(rnorm(1000 * 200), nrow = 1000, ncol = 200)
B <- matrix(rnorm(1000 * 150), nrow = 1000, ncol = 150)
C <- matrix(rnorm(800 * 200),  nrow = 800,  ncol = 200) 

# Save matrices to HDF5 file using BigDataStatMeth
example_fn <- "delayed.hdf5"

bdCreate_hdf5_matrix(filename = example_fn, 
                     object = A,
                     group = "INPUT", 
                     dataset =  "A", 
                     overwriteFile = TRUE)

bdCreate_hdf5_matrix(filename = example_fn,
                     object = B,
                     group = "INPUT", 
                     dataset =  "B", 
                     overwriteFile = FALSE)

bdCreate_hdf5_matrix(filename = example_fn,
                     object = C,
                     group = "INPUT", 
                     dataset = "C", 
                     overwriteFile = FALSE)

# Compute t(A) %*% A
res_cross <- bdCrossprod_hdf5(filename = example_fn, 
                              group = "INPUT", 
                              A = "A")

# Show where the result is stored
h5ls(res_cross$fn)

# Compare with R's crossprod
res_hdf5 <- h5read(res_cross$fn, res_cross$ds)
res_r <- crossprod(A)

all.equal(res_r, res_hdf5)

## ----crossprod_dbl------------------------------------------------------------
# Compute t(A) %*% B
res_cross2 <- bdCrossprod_hdf5(filename = example_fn, 
                               group = "INPUT", 
                               A = "A", 
                               B = "B")

# Compare with R
res_hdf5 <- h5read(res_cross2$fn, res_cross2$ds)
res_r <- crossprod(A, B)

all.equal(res_r, res_hdf5)

## ----tcrossprod_sing----------------------------------------------------------
# Compute A %*% t(A)
res_tcross <- bdtCrossprod_hdf5(filename = example_fn, 
                                group = "INPUT", 
                                A = "A")

h5ls(res_tcross$fn)

res_hdf5_t <- h5read(res_tcross$fn, res_tcross$ds)
res_r_t <- tcrossprod(A)

all.equal(res_r_t, res_hdf5_t)

## ----tcrossprod_dbl-----------------------------------------------------------
# Compute A %*% t(B)
res_tcross2 <- bdtCrossprod_hdf5(filename = example_fn, 
                                 group = "INPUT", 
                                 A = "A", 
                                 B = "C")

res_hdf5_t <- h5read(res_tcross2$fn, res_tcross2$ds)
res_r_t <- tcrossprod(A, C)

all.equal(res_r_t, res_hdf5_t)

## ----substract_init-----------------------------------------------------------

# Create two matrices of the same dimensions
set.seed(42)
A_sub <- matrix(rnorm(1000 * 300), nrow = 1000, ncol = 300)
B_sub <- matrix(rnorm(1000 * 300), nrow = 1000, ncol = 300)

# Save them to HDF5
fn_sub <- "subtraction_example.hdf5"

bdCreate_hdf5_matrix(filename = fn_sub, object = A_sub,
                     group = "INPUT", dataset = "A_sub",
                     overwriteFile = TRUE)

bdCreate_hdf5_matrix(filename = fn_sub, object = B_sub,
                     group = "INPUT", dataset = "B_sub",
                     overwriteFile = FALSE)

# Perform subtraction: A - B
res_sub <- bdblockSubstract_hdf5(filename = fn_sub,
                                  group = "INPUT",
                                  A = "A_sub", B = "B_sub")

# Compare a subset with R
result_hdf5 <- h5read(res_sub$fn, res_sub$ds)
result_r <- A_sub - B_sub

all.equal(result_r, result_hdf5)

## ----add----------------------------------------------------------------------
# Create two compatible matrices
set.seed(99)
A_add <- matrix(rnorm(800 * 250), nrow = 800, ncol = 250)
B_add <- matrix(rnorm(800 * 250), nrow = 800, ncol = 250)

# Save them to HDF5
fn_add <- "addition_example.hdf5"

bdCreate_hdf5_matrix(filename = fn_add, object = A_add,
                     group = "INPUT", dataset = "A",
                     overwriteFile = TRUE)

bdCreate_hdf5_matrix(filename = fn_add, object = B_add,
                     group = "INPUT", dataset = "B",
                     overwriteFile = FALSE)

# Perform addition: A + B
res_add <- bdblockSum_hdf5(filename = fn_add,
                            group = "INPUT",
                            A = "A", B = "B")

# Compare result with R
result_hdf5 <- h5read(res_add$fn, res_add$ds)
result_r <- A_add + B_add

all.equal(result_r, result_hdf5)

## ----BSVDImg, out.width = '100%', fig.align = 'center', fig.cap = "Flowchart for a two-level hierarchical Block SVD algorithm", echo=FALSE----
knitr::include_graphics("imgs/blocksvd.png")

## ----BlockSVDNorm-------------------------------------------------------------
# Create dataframe data with 'odata' matrix in delayed hdf5 file at OMIC group
set.seed(5234)
n <- 100
m <- 15000
omicdata <- matrix(rnorm(n*m, mean=0, sd=1), n,m)

bdCreate_hdf5_matrix(filename = example_fn, 
                     object = omicdata, 
                     group = "OMICS", 
                     dataset = "data", 
                     overwriteDataset = TRUE)

# Direct from hdf5 data file
svdh5 <- bdSVD_hdf5( filename = example_fn, 
                     group = "OMICS", 
                     dataset = "data", 
                     overwrite  = TRUE)

# get results svd (d) from hdf5 data file
svd_hdf5_d <- h5read(svdh5$fn, svdh5$ds_d)

# Results in hdf5 file for d
svd_hdf5_d[1:7]

svd <- svd(scale(omicdata))
svd$d[1:7]


## ----BlockSVDNotNorm----------------------------------------------------------
# Direct from hdf5 data file (using only one thread, serial execution)
svdh5 <- bdSVD_hdf5( filename = example_fn, 
                     group = "OMICS", 
                     dataset = "data",
                     bcenter = FALSE, 
                     bscale = FALSE,
                     overwrite  = TRUE)

## ----BlockSVDNotNormResults---------------------------------------------------
# get results svd (d)
svd_hdf5_d <- h5read(svdh5$fn, svdh5$ds_d)[1:7]
# SVD (d) from file - data not normalized
svd_hdf5_d

# with R implementation from data in memory
svd <- svd(omicdata)
svd$d[1:7]

## ----BlockSVDk4---------------------------------------------------------------
# Block decomposition with 1 level and 4 local SVDs at each level using 
# two threads (as maximum)
svdh5 <- bdSVD_hdf5( filename = example_fn, 
                     group = "OMICS", 
                     dataset = "data",
                     q = 1, 
                     k = 4, 
                     threads = 2,
                     overwrite  = TRUE)

# get results svd (d)
svd_hdf5_d <- h5read(svdh5$fn, svdh5$ds_d)[1:7]

# SVD (d) from file - data not normalized
svd_hdf5_d

# with R implementation from data in memory
svd <- svd(scale(omicdata))
svd$d[1:7]

## ----cholDesc-----------------------------------------------------------------
N <- 100
set.seed(5234)
Y <- matrix(rnorm(N*N), N, N)
Ycp <- crossprod(Y)

bdCreate_hdf5_matrix(filename = example_fn, 
                     object = Ycp, 
                     group = "chol", 
                     dataset = "data",
                     transp = FALSE,
                     overwriteFile = TRUE, overwriteDataset = TRUE, 
                     unlimited = FALSE)

cholh5 <- bdCholesky_hdf5(filename = example_fn, 
                          group = "chol", 
                          dataset = "data",
                          outdataset = "matrixDec", 
                          outgroup = "Cholesky_Dec",
                          overwrite = TRUE)
choldesc_hdf5 <-  h5read(cholh5$fn, cholh5$ds)
choldesc_hdf5[1:3,1:5]

choldesc_r <- chol(Ycp)
choldesc_r[1:3,1:5]

all.equal(choldesc_hdf5, choldesc_r)


## ----cholDesc_50--------------------------------------------------------------
cholh5 <- bdCholesky_hdf5(filename = example_fn, 
                          group = "chol", 
                          dataset = "data",
                          outdataset = "matrixDec_50", 
                          outgroup = "Cholesky_Dec",
                          elementsBlock = 50,
                          overwrite = TRUE)

# Result dataset
cholh5$ds

choldesc_hdf5 <-  h5read(cholh5$fn, cholh5$ds)
choldesc_hdf5[1:3,1:5]

choldesc_r <- chol(Ycp)
choldesc_r[1:3,1:5]

all.equal(choldesc_hdf5, choldesc_r)


## ----QRdec--------------------------------------------------------------------
QRh5 <- bdQR_hdf5(filename = example_fn,
                  group = "chol",
                  dataset = "data",
                  outgroup = "QR_Dec",thin = TRUE,
                  overwrite = TRUE)

# Result dataset
QR_Q_hdf5 <-  h5read(QRh5$fn, QRh5$ds_Q)
QR_R_hdf5 <-  h5read(QRh5$fn, QRh5$ds_R)

# Q matrix
QR_Q_hdf5[1:3,1:5]

# R matrix
QR_R_hdf5[1:3,1:5]

# Q matrix in R
QR_Q_r <- qr.Q(qr(Ycp))
QR_Q_r[1:3,1:5]

all.equal(QR_Q_hdf5, QR_Q_r)


## ----QRdec_blocksize----------------------------------------------------------
QRh5 <- bdQR_hdf5(filename = example_fn,
                  group = "chol",
                  dataset = "data",
                  outgroup = "QR_Dec",
                  block_size = 256,
                  overwrite = TRUE)

# Result dataset
QR_Q_hdf5 <-  h5read(QRh5$fn, QRh5$ds_Q)
QR_R_hdf5 <-  h5read(QRh5$fn, QRh5$ds_R)

# Q matrix
QR_Q_hdf5[1:3,1:5]

# R matrix
QR_R_hdf5[1:3,1:5]

# Q matrix in R
QR_Q_r <- qr.Q(qr(Ycp))
QR_Q_r[1:3,1:5]

all.equal(QR_Q_hdf5, QR_Q_r)


## ----CholInv------------------------------------------------------------------
invCholh5 <- bdInvCholesky_hdf5( filename = example_fn,
                                 group = "chol",
                                 dataset = "data",
                                 outdataset = "invmatrix", 
                                 outgroup = "InvCholesky", 
                                 fullMatrix = FALSE, 
                                 overwrite = TRUE)

# Result dataset
invChol_hdf5 <-  h5read(invCholh5$fn, invCholh5$ds)
invChol_hdf5[1:5,1:5]


## ----CholInv_full-------------------------------------------------------------
invCholh5 <- bdInvCholesky_hdf5( filename = example_fn,
                                 group = "chol",
                                 dataset = "data",
                                 outdataset = "invmatrix", 
                                 outgroup = "InvCholesky", 
                                 fullMatrix = TRUE, 
                                 overwrite = TRUE)

# Result dataset
invChol_hdf5 <-  h5read(invCholh5$fn, invCholh5$ds)
invChol_hdf5[1:5,1:5]

# Inverse Cholesky matrix in R
invChol_r <- solve(Ycp)
invChol_r[1:5,1:5]

all.equal(invChol_hdf5, invChol_r)


## ----sesinfo------------------------------------------------------------------
sessionInfo()

