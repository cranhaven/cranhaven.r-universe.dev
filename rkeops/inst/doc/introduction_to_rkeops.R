## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  progress = TRUE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  # install rkeops
#  install.packages("rkeops")
#  # load rkeops
#  library(rkeops)
#  # create a dedicated Python environment with reticulate (to be done only once)
#  reticulate::virtualenv_create("rkeops")
#  # activate the dedicated Python environment
#  reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)
#  # install rkeops requirements (to be done only once)
#  install_rkeops()

## -----------------------------------------------------------------------------
#  vignette("using_rkeops", package = "rkeops")

## -----------------------------------------------------------------------------
#  # to run computation on CPU (default mode)
#  rkeops_use_cpu()
#  # OR
#  # to run computations on GPU (to be used only if relevant)
#  rkeops_use_gpu()
#  
#  # Data
#  M <- 10000
#  N <- 15000
#  x <- matrix(runif(N * 3), nrow = M, ncol = 3) # arbitrary R matrix representing
#                                                # 10000 data points in R^3
#  y <- matrix(runif(M * 3), nrow = N, ncol = 3) # arbitrary R matrix representing
#                                                # 15000 data points in R^3
#  s <- 0.1                                      # scale parameter
#  
#  # Turn our Tensors into KeOps symbolic variables:
#  x_i <- LazyTensor(x, "i")     # symbolic object representing an arbitrary row of x,
#                                # indexed by the letter "i"
#  y_j <- LazyTensor(y, "j")     # symbolic object representing an arbitrary row of y,
#                                # indexed by the letter "j"
#  
#  # Perform large-scale computations, without memory overflows:
#  D_ij <- sum((x_i - y_j)^2)    # symbolic matrix of pairwise squared distances,
#                                # with 10000 rows and 15000 columns
#  
#  K_ij <- exp(- D_ij / s^2)     # symbolic matrix, 10000 rows and 15000 columns
#  
#  # D_ij and K_ij are only symbolic at that point, no computation is done
#  
#  # Computing the result without storing D_ij and K_ij:
#  a_j <- sum(K_ij, index = "i") # actual R matrix (in fact a row vector of
#                                # length 15000 here)
#                                # containing the column sums of K_ij
#                                # (i.e. the sums over the "i" index, for each
#                                # "j" index)

## -----------------------------------------------------------------------------
#  vignette("LazyTensor_rkeops", package = "rkeops")

## ----formula------------------------------------------------------------------
#  formula = "Sum_Reduction(Exp(-s * SqNorm2(x - y)) * b, 0)"

## ----args---------------------------------------------------------------------
#  args = c("x = Vi(3)",      # vector indexed by i (of dim 3)
#           "y = Vj(3)",      # vector indexed by j (of dim 3)
#           "b = Vj(6)",      # vector indexed by j (of dim 6)
#           "s = Pm(1)")      # parameter (scalar)

## ----compile------------------------------------------------------------------
#  # compilation
#  op <- keops_kernel(formula, args)
#  # data and parameter values
#  nx <- 100
#  ny <- 150
#  X <- matrix(runif(nx*3), nrow=nx)   # matrix 100 x 3
#  Y <- matrix(runif(ny*3), nrow=ny)   # matrix 150 x 3
#  B <- matrix(runif(ny*6), nrow=ny)   # matrix 150 x 6
#  s <- 0.2
#  # computation (order of the input arguments should be similar to `args`)
#  res <- op(list(X, Y, B, s))

## -----------------------------------------------------------------------------
#  vignette("using_rkeops", package = "rkeops")

## -----------------------------------------------------------------------------
#  vignette("using_rkeops", package = "rkeops")

## -----------------------------------------------------------------------------
#  vignette("LazyTensor_rkeops", package = "rkeops")

## -----------------------------------------------------------------------------
#  vignette("kernel_interpolation_rkeops", package = "rkeops")

