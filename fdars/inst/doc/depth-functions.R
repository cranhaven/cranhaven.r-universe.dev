## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(fdars)
library(ggplot2)
theme_set(theme_minimal())

# Create example data
set.seed(42)
n <- 30
m <- 100
t_grid <- seq(0, 1, length.out = m)

# Main sample: sine curves with noise
X <- matrix(0, n, m)
for (i in 1:n) {
  X[i, ] <- sin(2 * pi * t_grid) + rnorm(m, sd = 0.15)
}

# Add some outliers
X[1, ] <- sin(2 * pi * t_grid) + 2  # Magnitude outlier
X[2, ] <- cos(2 * pi * t_grid)       # Shape outlier

fd <- fdata(X, argvals = t_grid)
plot(fd)

## ----depth-fm-----------------------------------------------------------------
depths_fm <- depth(fd, method = "FM")

# Show depths (outliers should have lower depth)
data.frame(
  curve = 1:5,
  depth = round(depths_fm[1:5], 4),
  note = c("magnitude outlier", "shape outlier", rep("normal", 3))
)

## ----depth-fm-scaled----------------------------------------------------------
depths_fm_scaled <- depth(fd, method = "FM", scale = TRUE)
range(depths_fm_scaled)

## ----depth-mode---------------------------------------------------------------
depths_mode <- depth(fd, method = "mode")
head(depths_mode)

## ----depth-rp-----------------------------------------------------------------
depths_rp <- depth(fd, method = "RP", nproj = 50)
head(depths_rp)

## ----depth-rt-----------------------------------------------------------------
depths_rt <- depth(fd, method = "RT", nproj = 50)
head(depths_rt)

## ----depth-fsd----------------------------------------------------------------
depths_fsd <- depth(fd, method = "FSD")
head(depths_fsd)

## ----depth-kfsd---------------------------------------------------------------
depths_kfsd <- depth(fd, method = "KFSD", h = 0.15)
head(depths_kfsd)

## ----depth-rpd----------------------------------------------------------------
depths_rpd <- depth(fd, method = "RPD", nproj = 50)
head(depths_rpd)

## ----compare-depths-----------------------------------------------------------
# Compute all depths using unified depth() function
all_depths <- data.frame(
  FM = depth(fd, method = "FM"),
  mode = depth(fd, method = "mode"),
  RP = depth(fd, method = "RP", nproj = 50),
  RT = depth(fd, method = "RT", nproj = 50),
  FSD = depth(fd, method = "FSD")
)

# Correlation between depth functions
round(cor(all_depths), 2)

## ----depth-comparison-plot----------------------------------------------------
# Which curves are identified as outliers (lowest depth)?
outlier_ranks <- apply(all_depths, 2, function(d) order(d)[1:3])
outlier_ranks

## ----depth-visualization------------------------------------------------------
# Visualize curves colored by their FM depth
df_depth_viz <- data.frame(
  t = rep(t_grid, n),
  value = as.vector(t(X)),
  curve = rep(1:n, each = m),
  depth = rep(depths_fm, each = m)
)

ggplot(df_depth_viz, aes(x = t, y = value, group = curve, color = depth)) +
  geom_line(alpha = 0.8) +
  scale_color_viridis_c(option = "plasma", name = "FM Depth") +
  labs(title = "Curves Colored by Depth",
       subtitle = "Dark = low depth (outlier), Bright = high depth (central)",
       x = "t", y = "X(t)")

## ----median-------------------------------------------------------------------
# Using different depth functions via method parameter
med_fm <- median(fd, method = "FM")
med_mode <- median(fd, method = "mode")
med_rp <- median(fd, method = "RP", nproj = 50)

# The median is one of the original curves
which.max(depth(fd, method = "FM"))

## ----trimmed-mean-------------------------------------------------------------
# 10% trimmed mean using different depth methods
trim_fm <- trimmed(fd, trim = 0.1, method = "FM")
trim_mode <- trimmed(fd, trim = 0.1, method = "mode")

# Compare trimmed mean to regular mean
mean_curve <- mean(fd)

## ----plot-robust--------------------------------------------------------------
# Visualize: trimmed mean is more robust to outliers
df_compare <- data.frame(
  t = rep(t_grid, 2),
  value = c(mean_curve$data[1, ], trim_fm$data[1, ]),
  type = rep(c("Mean", "Trimmed Mean (FM)"), each = m)
)

library(ggplot2)
ggplot(df_compare, aes(x = t, y = value, color = type)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean vs Trimmed Mean",
       x = "t", y = "X(t)", color = "") +
  theme_minimal()

## ----variance-----------------------------------------------------------------
# Regular variance
var_fd <- var(fd)

# Trimmed variance (more robust)
trimvar_fd <- trimvar(fd, trim = 0.1, method = "FM")

## ----performance, eval=FALSE--------------------------------------------------
# # Large dataset benchmark
# X_large <- matrix(rnorm(500 * 200), 500, 200)
# fd_large <- fdata(X_large)
# 
# system.time(depth(fd_large, method = "FM"))
# #>    user  system elapsed
# #>   0.032   0.000   0.032
# 
# system.time(depth(fd_large, method = "RP", nproj = 100))
# #>    user  system elapsed
# #>   0.089   0.000   0.089

