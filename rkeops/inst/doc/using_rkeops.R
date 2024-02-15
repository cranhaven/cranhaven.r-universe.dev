## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  progress = TRUE,
  warning = FALSE,
  eval = FALSE
)

## ----install------------------------------------------------------------------
#  install.packages("rkeops")

## ----install_github-----------------------------------------------------------
#  install.packages("remotes")
#  remotes::install_github("getkeops/keops", subdir = "rkeops")

## ----install_requirements-----------------------------------------------------
#  # load rkeops
#  library(rkeops)
#  # create a dedicated Python environment with reticulate (to be done only once)
#  reticulate::virtualenv_create("rkeops")
#  # activate the dedicated Python environment
#  reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)
#  # install rkeops requirements (to be done only once)
#  install_rkeops()

## ----setup--------------------------------------------------------------------
#  # load rkeops
#  library(rkeops)
#  # activate the dedicated Python environment
#  reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)

## ----check--------------------------------------------------------------------
#  check_rkeops()

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

## ----eval=FALSE---------------------------------------------------------------
#  vignette("LazyTensor_rkeops", package = "rkeops")

## -----------------------------------------------------------------------------
#  # implementation of a convolution with a Gaussian kernel
#  formula = "Sum_Reduction(Exp(-s * SqNorm2(x - y)) * b, 0)"
#  # input arguments
#  args = c("x = Vi(3)",      # vector indexed by i (of dim 3)
#           "y = Vj(3)",      # vector indexed by j (of dim 3)
#           "b = Vj(6)",      # vector indexed by j (of dim 6)
#           "s = Pm(1)")      # parameter (scalar)
#  # compilation
#  op <- keops_kernel(formula, args)
#  # data and parameter values
#  nx <- 100
#  ny <- 150
#  X <- matrix(runif(nx*3), nrow=nx)   # matrix 100 x 3
#  Y <- matrix(runif(ny*3), nrow=ny)   # matrix 150 x 3
#  B <- matrix(runif(ny*6), nrow=ny)   # matrix 150 x 6
#  s <- 0.2
#  
#  # to run computation on CPU (default mode)
#  use_cpu()
#  # to run computations on GPU (to be used only if relevant)
#  use_gpu()
#  
#  # computation (order of the input arguments should be similar to `args`)
#  res <- op(list(X, Y, B, s))

## ----formula------------------------------------------------------------------
#  formula = "Sum_Reduction(Exp(-s * SqNorm2(x - y)) * b, 0)"

## ----template_args------------------------------------------------------------
#  args = c("<name1>=<type1>(dim1)", "<name2>=<type2>(dim2)", "<nameX>=<typeX>(dimX)")

## ----args---------------------------------------------------------------------
#  args = c("x = Vi(3)",      # vector indexed by i (of dim 3)
#           "y = Vj(3)",      # vector indexed by j (of dim 3)
#           "b = Vj(6)",      # vector indexed by j (of dim 6)
#           "s = Pm(1)")      # parameter (scalar)

## ----compile------------------------------------------------------------------
#  # compilation
#  op <- keops_kernel(formula, args)

## ----run----------------------------------------------------------------------
#  # data and parameter values
#  nx <- 100
#  ny <- 150
#  X <- matrix(runif(nx*3), nrow=nx)   # matrix 100 x 3
#  Y <- matrix(runif(ny*3), nrow=ny)   # matrix 150 x 3
#  B <- matrix(runif(ny*6), nrow=ny)   # matrix 150 x 6
#  s <- 0.2
#  
#  # to run computation on CPU (default mode)
#  rkeops_use_cpu()
#  # OR
#  # to run computations on GPU (to be used only if relevant)
#  rkeops_use_gpu()
#  
#  # computation (order of the input arguments should be similar to `args`)
#  res <- op(list(x, y, beta, s))

## ----grad---------------------------------------------------------------------
#  # defining a formula with a Gradient
#  formula <- "Grad(Sum_Reduction(SqNorm2(x-y), 0), x, eta)"
#  args <- c("x=Vi(0,3)", "y=Vj(1,3)", "eta=Vi(2,1)")
#  # compiling the corresponding operator
#  op <- keops_kernel(formula, args)
#  
#  # data
#  nx <- 100
#  ny <- 150
#  x <- matrix(runif(nx*3), nrow=nx, ncol=3)     # matrix 100 x 3
#  y <- matrix(runif(ny*3), nrow=ny, ncol=3)     # matrix 150 x 3
#  eta <- matrix(runif(nx*1), nrow=nx, ncol=1)   # matrix 100 x 1
#  
#  # computation
#  input <- list(x, y, eta)
#  res <- op(input)

## ----keops_grad---------------------------------------------------------------
#  # defining an operator (reduction on squared distance)
#  formula <- "Sum_Reduction(SqNorm2(x-y), 0)"
#  args <- c("x=Vi(0,3)", "y=Vj(1,3)")
#  op <- keops_kernel(formula, args)
#  # defining its gradient regarding x
#  grad_op <- keops_grad(op, var="x")
#  
#  # data
#  nx <- 100
#  ny <- 150
#  x <- matrix(runif(nx*3), nrow=nx, ncol=3)     # matrix 100 x 3
#  y <- matrix(runif(ny*3), nrow=ny, ncol=3)     # matrix 150 x 3
#  eta <- matrix(runif(nx*1), nrow=nx, ncol=1)   # matrix 100 x 1
#  
#  # computation
#  input <- list(x, y, eta)
#  res <- grad_op(input)

## ----get_options--------------------------------------------------------------
#  get_rkeops_options()

## ----reset_options------------------------------------------------------------
#  set_rkeops_options()

## ----use_gpu------------------------------------------------------------------
#  rkeops_use_gpu()

## ----precision----------------------------------------------------------------
#  rkeops_use_float32()
#  rkeops_use_float64()

## ----verbosity----------------------------------------------------------------
#  rkeops_enable_verbosity()
#  rkeops_disable_verbosity()

## ----storage_order------------------------------------------------------------
#  # standard column reduction of a matrix product
#  op <- keops_kernel(formula = "Sum_Reduction((x|y), 1)",
#                     args = c("x=Vi(3)", "y=Vj(3)"))
#  
#  # data (inner dimension = columns)
#  nx <- 10
#  ny <- 15
#  # x_i = rows of the matrix X
#  X <- matrix(runif(nx*3), nrow=nx, ncol=3)
#  # y_j = rows of the matrix Y
#  Y <- matrix(runif(ny*3), nrow=ny, ncol=3)
#  # computing the result (here, by default `inner_dim="col"` and columns
#  # corresponds to the inner dimension)
#  res <- op(list(X,Y))
#  
#  # data (inner dimension = rows)
#  nx <- 10
#  ny <- 15
#  # x_i = columns of the matrix X
#  X <- matrix(runif(nx*3), nrow=3, ncol=nx)
#  # y_j = columns of the matrix Y
#  Y <- matrix(runif(ny*3), nrow=3, ncol=ny)
#  # computing the result (we specify `inner_dim="row"` to indicate that rows
#  # corresponds to the inner dimension)
#  res <- op(list(X,Y), inner_dim="row")

