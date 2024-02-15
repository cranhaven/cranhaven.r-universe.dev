## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  progress = TRUE,
  warning = FALSE,
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  # load rkeops
#  library(rkeops)
#  # create a dedicated Python environment with reticulate (to be done only once)
#  reticulate::virtualenv_create("rkeops")
#  # activate the dedicated Python environment
#  reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)
#  # install rkeops requirements (to be done only once)
#  install_rkeops()

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

## ----cplx_example-------------------------------------------------------------
#  # Arbitrary complex R matrix
#  z <- matrix(2 + 1i^(-6:-1), nrow = 2, ncol = 2)
#  z
#  ##      [,1] [,2]
#  ## [1,] 1+0i 3+0i
#  ## [2,] 2-1i 2+1i
#  
#  # Encode as a `ComplexLazyTensor`, indexed by 'i'
#  z_i <- LazyTensor(z, index = 'i', is_complex = TRUE)
#  
#  # extract the data (corresponding to `z` value)
#  z_i$data
#  ## [[1]]
#  ##      [,1] [,2] [,3] [,4]
#  ## [1,]    1    0    3    0
#  ## [2,]    2   -1    2    1
#  
#  # Same idea with a vector of complex
#  v_z <- c(4 + 5i, 2 + 3i, 7 + 1i)
#  v_z
#  ## [1] 4+5i 2+3i 7+1i
#  
#  # Encode as a vector parameter `ComplexLazyTensor`
#  Pm_v_z <- LazyTensor(v_z, is_complex = TRUE)
#  
#  # extract the data (corresponding to `v_z` value)
#  Pm_v_z$data
#  ## [[1]]
#  ##     [,1] [,2] [,3] [,4] [,5] [,6]
#  ## [1,]    4    5    2    3    7    1

## -----------------------------------------------------------------------------
#  # Real R vector
#  v <- c(5, 4, 7, 9)
#  
#  Pm_v <- LazyTensor(v, is_complex = TRUE)
#  Pm_v$data
#  ## [[1]]
#  ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#  ## [1,]    5    0    4    0    7    0    9    0

## ----dimres-------------------------------------------------------------------
#  N <- 100
#  # arbitrary R matrices representing 100 data points in R^3
#  w <- matrix(runif(N * 3), nrow = N, ncol = 3)
#  x <- matrix(runif(N * 3), nrow = N, ncol = 3)
#  y <- matrix(runif(N * 3), nrow = N, ncol = 3)
#  
#  # Create `LazyTensor`s from `w`, `x` and `y`
#  w_i <- LazyTensor(w, "i")
#  x_i <- LazyTensor(x, "i")
#  y_j <- LazyTensor(y, "j")
#  
#  # print `x_i` inner dimension:
#  x_i$dimres
#  
#  # print `y_j` inner dimension:
#  y_j$dimres
#  
#  # Simple addition
#  sum_xy <- x_i + y_j
#  
#  # print `sum_xy` inner dimension:
#  sum_xy$dimres
#  
#  # Euclidean element-wise squared distance
#  sq_dist_sum_xy_w <- sqdist(sum_xy, w_i)
#  
#  # print `sq_dist_sum_xy_w` inner dimension:
#  sq_dist_sum_xy_w$dimres

## ----aliases------------------------------------------------------------------
#  # Data
#  N <- 100
#  x <- matrix(runif(N * 3), nrow = N, ncol = 3) # arbitrary R matrix representing
#                                                # 100 data points in R^3
#  v <- runif(3, 0, 1)                           # arbitrary vector of length 3
#  s <- 0.1                                      # scale parameter
#  
#  # Create symbolic object representing an arbitrary row of x,
#  # indexed by the letter "i":
#  x_i <- LazyTensor(x, "i")
#  # Same as
#  x_i <- Vi(x)
#  
#  # Create symbolic object representing an arbitrary row of x,
#  # indexed by the letter "j":
#  x_j <- LazyTensor(x, "j")
#  # Same as
#  x_j <- Vj(x)
#  
#  # Create symbolic object representing the vector `v` above:
#  LT_v <- LazyTensor(v)
#  # Same as
#  LT_v <- Pm(v)
#  
#  # Create symbolic object representing the scalar `s` above:
#  LT_s <- LazyTensor(s)
#  # Same as
#  LT_s <- Pm(s)

## ----type_checking------------------------------------------------------------
#  D <- 3
#  M <- 100
#  x <- matrix(runif(M * D), M, D)   # matrix of real values
#  x_i <- LazyTensor(x, index = 'i')
#  p <- LazyTensor(runif(3, 0, 1))   # LazyTensor encoding a fixed vector of real values
#  l <- LazyTensor(314)              # LazyTensor encoding a fixed scalar parameter
#  z <- matrix(1i^(-6:5), nrow = 4)  # matrix of complex values
#  z_i <- LazyTensor(z, index = 'i', is_complex = TRUE)
#  
#  scal <- 3.14
#  cplx <- 2 + 3i
#  scal_LT <- LazyTensor(scal)
#  cplx_LT <- LazyTensor(cplx)
#  
#  # check types
#  is.LazyTensor(x_i)
#  
#  is.ComplexLazyTensor(z_i)
#  
#  is.LazyVector(p)
#  
#  is.LazyParameter(scal_LT)
#  
#  is.ComplexLazyParameter(cplx_LT)

