## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  out.width = "100%"
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# remotes::install_github("sipemu/fdars")

## ----setup--------------------------------------------------------------------
library(fdars)
library(ggplot2)
theme_set(theme_minimal())

## ----create-fdata-------------------------------------------------------------
# Generate example data: 20 curves evaluated at 100 points
set.seed(42)
n <- 20
m <- 100
t_grid <- seq(0, 1, length.out = m)

# Create curves: sine waves with random phase and noise
X <- matrix(0, n, m)
for (i in 1:n) {
  phase <- runif(1, 0, pi)
  X[i, ] <- sin(2 * pi * t_grid + phase) + rnorm(m, sd = 0.1)
}

# Create fdata object
fd <- fdata(X, argvals = t_grid)
fd

## ----metadata-----------------------------------------------------------------
# Create metadata with covariates
meta <- data.frame(
  group = factor(rep(c("control", "treatment"), each = 10)),
  age = sample(20:60, n, replace = TRUE),
  response = rnorm(n)
)

# Create fdata with IDs and metadata
fd_meta <- fdata(X, argvals = t_grid,
                 id = paste0("patient_", 1:n),
                 metadata = meta)
fd_meta

# Access metadata
fd_meta$id[1:5]
head(fd_meta$metadata)

## ----metadata-subset----------------------------------------------------------
fd_sub <- fd_meta[1:5, ]
fd_sub$id
fd_sub$metadata

## ----plot-fdata---------------------------------------------------------------
plot(fd)

## ----basic-ops----------------------------------------------------------------
# Compute mean function
mean_curve <- mean(fd)

# Center the data
fd_centered <- fdata.cen(fd)

# Compute functional variance
variance <- var(fd)

## ----subset-------------------------------------------------------------------
# First 5 curves
fd_subset <- fd[1:5, ]

# Specific range of t values
fd_range <- fd[, t_grid >= 0.25 & t_grid <= 0.75]

## ----depth--------------------------------------------------------------------
# Fraiman-Muniz depth
depths <- depth(fd, method = "FM")
head(depths)

# Find the median curve (deepest)
median_curve <- median(fd, method = "FM")

## ----distances----------------------------------------------------------------
# L2 (Euclidean) distance
dist_l2 <- metric.lp(fd)

# Dynamic Time Warping
dist_dtw <- metric.DTW(fd)

## ----regression---------------------------------------------------------------
# Generate response
y <- rowMeans(X) + rnorm(n, sd = 0.1)

# Principal component regression
fit_pc <- fregre.pc(fd, y, ncomp = 3)
print(fit_pc)

## ----clustering---------------------------------------------------------------
# K-means clustering
km <- cluster.kmeans(fd, ncl = 2, seed = 123)
plot(km)

## ----outliers-----------------------------------------------------------------
# Add an outlier
X_out <- rbind(X, X[1, ] + 3)
fd_out <- fdata(X_out, argvals = t_grid)

# Detect outliers
out <- outliers.depth.pond(fd_out)
plot(out)

## ----performance, eval=FALSE--------------------------------------------------
# # Generate large dataset
# X_large <- matrix(rnorm(1000 * 200), 1000, 200)
# fd_large <- fdata(X_large)
# 
# # Depth computation is fast even for large datasets
# system.time(depth(fd_large, method = "FM"))
# #>    user  system elapsed
# #>   0.045   0.000   0.045

