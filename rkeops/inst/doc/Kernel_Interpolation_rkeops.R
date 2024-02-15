## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  progress = TRUE,
  warning = FALSE,
  eval = TRUE
)

## ----requirement, include=FALSE-----------------------------------------------
required <- c("ggplot2", "dplyr", "pracma", "plotly", "reshape")
sapply(
    required,
    function(pkg) {
        if(! pkg %in% .packages(all.available = TRUE)) install.packages(pkg)
    }
)

## ----library, message=FALSE, warning=FALSE------------------------------------
library(ggplot2) # graph generation
library(dplyr)   # data.frame manipulation
library(pracma)  # to create the meshgrid
library(plotly)  # reactive graph generation
library(reshape) # matrix to data.frame reformatting

## ----init, message=FALSE, warning=FALSE, results='hide', eval=FALSE-----------
#  # load rkeops
#  library(rkeops)
#  # create a dedicated Python environment with reticulate (to be done only once)
#  reticulate::virtualenv_create("rkeops")
#  # activate the dedicated Python environment
#  reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)
#  # install rkeops requirements (to be done only once)
#  install_rkeops()
#  # use 64bit floating operation precision
#  rkeops_use_float64()

## ----safe_init, message=FALSE, warning=FALSE, results='hide', include=FALSE----
# load rkeops
library(rkeops)

tryCatch({
    # create a dedicated Python environment with reticulate (to be done only once)
    reticulate::virtualenv_create("rkeops")
    # activate the dedicated Python environment
    reticulate::use_virtualenv(virtualenv = "rkeops", required = TRUE)
    
    # check Python availability
    if(reticulate::py_available(initialize = TRUE)) {
        # install rkeops requirements (to be done only once)
        install_rkeops()
    } else {
        knitr::opts_chunk$set(eval = FALSE)
    }
}, error = function(e) {knitr::opts_chunk$set(eval = FALSE)})


## -----------------------------------------------------------------------------
# check rkeops install
check_rkeops()

# use 64bit floating operation precision
rkeops_use_float64()

## ----check_rkeops, include=FALSE----------------------------------------------
if(!check_rkeops(warn = FALSE)) {
    knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
rkeops_use_cpu(ncore = 2)

## ----data1D-------------------------------------------------------------------
N <- 10000 # number of samples

x <- matrix(runif(N * 1), N, 1)
pert <- matrix(runif(N * 1), N, 1) # random perturbation to create b

# Some random-ish 1D signal:
b <- x + 0.5 * sin(6 * x) + 0.1 * sin(20 * x) + 0.05 * pert

## ----gaussian_kernel----------------------------------------------------------
gaussian_kernel <- function(x, y, sigma = 0.1) {
    x_i <- Vi(x) # symbolic 'i'-indexed matrix
    y_j <- Vj(y) # symbolic 'j'-indexed matrix
    D_ij <- sum((x_i - y_j)^2) # symbolic matrix of squared distances
    res <- exp(-D_ij / (2 * sigma^2)) # symbolic Gaussian kernel matrix
    return(res)
}

## ----CGS----------------------------------------------------------------------
CG_solve <- function(K, b, lambda, eps = 1e-6) {
    # ----------------------------------------------------------------
    # Conjugate gradient algorithm to solve linear systems of the form
    # (K + lambda * Id) * a = b.
    #
    # K: a LazyTensor encoding a symmetric positive definite matrix
    #       (the spectrum of the matrix must not contain zero)
    # b: a vector corresponding to the second member of the equation
    # lambda: Non-negative ridge regularization parameter
    #       (lambda = 0 means no regularization)
    # eps (default=1e-6): precision parameter
    # ----------------------------------------------------------------
    delta <- length(b) * eps^2
    a <- 0
    r <- b
    nr2 <- sum(r^2) # t(r)*r (L2-norm)
    if(nr2 < delta) {
        return(0 * r)
    }
    p <- r
    k <- 0
    while (TRUE) {
      Mp <- K %*% Vj(p) + lambda * p
      alp <- nr2 / sum(p * Mp)
      a <- a + (alp * p)
      r <- r - (alp * Mp)
      nr2new <- sum(r^2)
      if (nr2new < delta) {
          break
      }
      p <- r + (nr2new / nr2) * p
      nr2 <- nr2new
      k <- k + 1
    }
    return(a) # should be such that K%*%a + lambda * Id * a = b (eps close) 
}

## ----call1D-------------------------------------------------------------------
K_xx <- gaussian_kernel(x, x)

lambda <- 1

start <- Sys.time()
a <- CG_solve(K_xx, b, lambda = lambda)
end <- Sys.time()

time <- round(as.numeric(end - start), 5)

print(paste("Time to perform an RBF interpolation with",
            N,"samples in 1D:", time, "s.",
            sep = " "
            )
      )

## ----plot1D, fig.width=7, fig.height=6, fig.align='center', message=FALSE-----
# extrapolate on a uniform sample
t <- as.matrix(seq(from = 0, to = 1, length.out = N))

K_tx <- gaussian_kernel(t, x)
mean_t <- K_tx %*% Vj(a)

D <- as.data.frame(cbind(x, b, t, mean_t))
colnames(D) <- c("x", "b", "t", "mean_t")

# 1D plot
ggplot(aes(x = x, y = b), data = D) +
  geom_point(color = '#1193a8', alpha = 0.5, size = 0.4) +
  geom_line(aes(x = t, y = mean_t), color = 'darkred') +
  annotate("text", x = .75, y = .1,
           label = paste("Number of samples: ", N,
                         sep = "")
           ) +
  theme_bw()

## ----data2D-------------------------------------------------------------------
N <- 10000
# Sampling locations:
x <- matrix(runif(N * 2), N, 2)

# Some random-ish 2D signal:
b <- as.matrix(rowSums((x - 0.5)^2))
b[b > 0.4^2] = 0
b[b < 0.3^2] = 0
b[b >= 0.3^2] = 1

pert <- matrix(runif(N * 1), N, 1) # random perturbation to create b
b <- b + 0.05 * pert

# Add 25% of outliers:
Nout <- N %/% 4
b[(length(b) - Nout + 1):length(b)] <- matrix(runif(Nout * 1), Nout, 1)

## ----laplacian_kernel---------------------------------------------------------
laplacian_kernel <- function(x, y, sigma = 0.1) {
  x_i <- Vi(x)
  y_j <- Vj(y)
  D_ij <- sum((x_i - y_j)^2)
  res <- exp(-sqrt(D_ij) / sigma)
  return(res)
}

## ----call2D-------------------------------------------------------------------
lambda <- 10  # Ridge regularization

start <- Sys.time()
K_xx <- laplacian_kernel(x, x)
a <- CG_solve(K_xx, b, lambda = lambda)
end <- Sys.time()

time <- round(as.numeric(end - start), 5)
print(paste("Time to perform an RBF interpolation with",
            N, "samples in 2D:", time, "s.",
            sep = " "))

## ----interpolate_2D, fig.width=7, fig.height=6, fig.align='center', message=FALSE, warning=FALSE----
# Interpolate on a uniform sample:
X <- seq(from = 0, to = 1, length.out = 100)
Y <- seq(from = 0, to = 1, length.out = 100)

G <- meshgrid(X, Y)
t <- cbind(as.vector(G$X), as.vector(G$Y))

K_tx <- laplacian_kernel(t, x)
mean_t <- K_tx %*% Vj(a)

mean_t <- matrix(mean_t, 100, 100)
mean_t <- mean_t[nrow(mean_t):1, ]

## ----plot2D_ggplot_sample, fig.width=7, fig.height=6, fig.align='center', message=FALSE, warning=FALSE, fig.cap="Data sample"----
# Data
data2plot_sample <- data.frame(
    X = x[,1],
    Y = x[,2],
    value = b
)

ggplot(data2plot_sample, aes(x = X, y = Y, col = value)) + 
    geom_point() +
    scale_color_gradient(low = "#C2C2C9", high = "darkred") + 
    coord_fixed() +
    theme_bw()

## ----plot2D_ggplot_interpol, fig.width=7, fig.height=6, fig.align='center', message=FALSE, warning=FALSE, fig.cap="Kernel interpolation"----
# Interpolation
data2plot_interpol <- reshape::melt(mean_t) %>% 
    mutate(X = X[X1], Y = Y[X2]) %>% dplyr::select(!c(X1, X2))

ggplot(data2plot_interpol, aes(x = X, y = Y, fill = value)) + 
    geom_tile() + 
    scale_fill_gradient(low = "#C2C2C9", high = "darkred") + 
    coord_fixed() +
    theme_bw()


## ----plot2D_plotly, fig.width=7, fig.height=6, fig.align='center', message=FALSE, warning=FALSE, eval=FALSE----
#  # 2D plot: noisy samples and interpolation in the background
#  fig <- plot_ly(z = mean_t,
#                 type = "heatmap",
#                 colors = colorRamp(c("#C2C2C9", "darkred")),
#                 zsmooth ="best"
#                 )
#  
#  fig <- fig %>% add_trace(type = "scatter",
#                           x = ~(100 * x[, 1]),
#                           y = ~(100 * x[, 2]),
#                           mode = "markers",
#                           marker = list(size = 4, color = as.vector(b))
#                           )
#  
#  fig <- fig %>% plotly::layout(xaxis = list(title = ""),
#                        yaxis = list(title = ""))
#  
#  colorbar(fig, limits = c(0, 1), x = 1, y = 0.75)

