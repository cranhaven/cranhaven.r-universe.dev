## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----araplot, echo=FALSE, fig.align = 'center', fig.cap="ARA plot of four variables of a breakfast cereal dataset. High-dimensional data values can be estimated via projections onto the labeled axes, as in Biplots.", out.width = '80%'----
knitr::include_graphics("../man/figures/vignette_cereals_ara_bb.jpg")

## ----eval=FALSE---------------------------------------------------------------
# install.packages("aramappings")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("manuelrubio/aramappings", build_vignettes = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("pak")
# pak::pak("manuelrubio/aramappings")

## -----------------------------------------------------------------------------
# Load package
library(aramappings)

## -----------------------------------------------------------------------------
# Define subset of (numerical) variables
selected_variables <- c(1, 4, 5, 6) # 1:"mpg", 4:"horsepower", 5:"weight", 6:"acceleration")
n <- length(selected_variables)

# Retain only selected variables and rename dataset as X
X <- auto_mpg[, selected_variables] # Select a subset of variables

## -----------------------------------------------------------------------------
# Remove rows with missing values from X
N <- nrow(X)
rows_to_delete <- NULL
for (i in 1:N) {
  if (sum(is.na(X[i, ])) > 0) {
    rows_to_delete <- c(rows_to_delete, -i)
  }
}
X <- X[rows_to_delete, ]

## -----------------------------------------------------------------------------
# Convert X to matrix
X <- apply(as.matrix.noquote(X), 2, as.numeric)

## -----------------------------------------------------------------------------
# Standardize data
Z <- scale(X)

## -----------------------------------------------------------------------------
# Define axis vectors (2-dimensional in this example)
r <- c(0.8, 1, 1.2, 1)
theta <- c(225, 100, 315, 80) * 2 * pi / 360
V <- pracma::zeros(n, 2)
for (i in 1:n) {
  V[i,1] <- r[i] * cos(theta[i])
  V[i,2] <- r[i] * sin(theta[i])
}

## -----------------------------------------------------------------------------
# Define weights
weights <- c(1, 0.75, 0.75, 1)

## ----echo = T, results = 'show'-----------------------------------------------
# Compute the mapping and print the execution time
start <- Sys.time()
mapping <- ara_unconstrained_l2(
  Z,
  V,
  weights = weights,
  solver = "formula"
)
end <- Sys.time()
message(c('Execution time: ',end - start, ' seconds'))

## -----------------------------------------------------------------------------
# Select variables with labeled axis lines on ARA plot
axis_lines <- c(1, 4) # 1:"mpg", 4:"acceleration")

## -----------------------------------------------------------------------------
# Select variable used for coloring embedded points
color_variable <- 1 # "mpg"

## ----fig.show='hide'----------------------------------------------------------
# Draw the ARA plot
draw_ara_plot_2d_standardized(
  Z,
  X,
  V,
  mapping$P,
  weights = weights,
  axis_lines = axis_lines,
  color_variable = color_variable
)

## ----unc_l2, echo=FALSE, fig.align = 'center', fig.cap="Unconstrained ARA plot with the l2 norm of a subset of the Autompg dataset.", out.width = '90%'----
knitr::include_graphics("../man/figures/vignette_autompg_unconstrained_l2_bb.jpg")

## ----echo = T, results = 'show'-----------------------------------------------
variable <- 1 # "mpg"

## ----echo = T, results = 'show'-----------------------------------------------
# Compute the mapping and print the execution time
start <- Sys.time()
mapping <- ara_exact_l2(
  Z,
  V,
  variable = variable,
  solver = "formula"
)
end <- Sys.time()
message(c('Execution time: ',end - start, ' seconds'))

## ----fig.show='hide'----------------------------------------------------------
# Draw the ARA plot
draw_ara_plot_2d_standardized(
  Z,
  X,
  V,
  mapping$P,
  axis_lines = axis_lines,
  color_variable = color_variable
)

## ----exact_l2, echo=FALSE, fig.align = 'center', fig.cap="Exact ARA plot with the l2 norm of a subset of the Autompg dataset. Exact estimates are obtained for variable 'mpg'.", out.width = '90%'----
knitr::include_graphics("../man/figures/vignette_autompg_exact_l2_bb.jpg")

## ----echo = T, results = 'show'-----------------------------------------------
# Compute the mapping and print the execution time
start <- Sys.time()
mapping <- ara_ordered_l2(
  Z,
  V,
  variable = variable,
  solver = "clarabel"
)
end <- Sys.time()
message(c('Execution time: ',end - start, ' seconds'))

## ----fig.show='hide'----------------------------------------------------------
# Draw the ARA plot
draw_ara_plot_2d_standardized(
  Z,
  X,
  V,
  mapping$P,
  axis_lines = axis_lines,
  color_variable = color_variable
)

## ----ordered_l2, echo=FALSE, fig.align = 'center', fig.cap="Ordered ARA plot with the l2 norm of a subset of the Autompg dataset. The values of 'mpg' are ordered correctly along its corresponding axis.", out.width = '90%'----
knitr::include_graphics("../man/figures/vignette_autompg_ordered_l2_bb.jpg")

## -----------------------------------------------------------------------------
# NCORES <- parallelly::availableCores(omit = 1)
# NCORES <- max(1,parallel::detectCores() - 1)
NCORES <- 2L

## -----------------------------------------------------------------------------
# Create a cluster for parallel processing
cl <- parallel::makeCluster(NCORES)

## ----echo = T, results = 'show'-----------------------------------------------
# Compute the mapping and print the execution time
start <- Sys.time()
mapping <- ara_unconstrained_l1(
  Z,
  V,
  weights = weights,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE,
  cluster = cl
)
end <- Sys.time()
message(c('Execution time: ',end - start, ' seconds'))

## ----fig.show='hide'----------------------------------------------------------
draw_ara_plot_2d_standardized(
  Z,
  X,
  V,
  mapping$P,
  weights = weights,
  axis_lines = axis_lines,
  color_variable = color_variable
)

## ----unc_l1, echo=FALSE, fig.align = 'center', fig.cap="Unconstrained ARA plot with the l1 norm of a subset of the Autompg dataset.", out.width = '90%'----
knitr::include_graphics("../man/figures/vignette_autompg_unconstrained_l1_bb.jpg")

## ----echo = T, results = 'show'-----------------------------------------------
# Compute the mapping and print the execution time
start <- Sys.time()
mapping <- ara_exact_l1(
  Z,
  V,
  variable = variable,
  solver = "glpkAPI",
  use_glpkAPI_simplex = TRUE,
  cluster = cl
)
end <- Sys.time()
message(c('Execution time: ',end - start, ' seconds'))

## ----fig.show='hide'----------------------------------------------------------
draw_ara_plot_2d_standardized(
  Z,
  X,
  V,
  mapping$P,
  axis_lines = axis_lines,
  color_variable = color_variable
)

## ----exact_l1, echo=FALSE, fig.align = 'center', fig.cap="Exact ARA plot with the l1 norm of a subset of the Autompg dataset. Exact estimates are obtained for variable 'mpg'.", out.width = '90%'----
knitr::include_graphics("../man/figures/vignette_autompg_exact_l1_bb.jpg")

## -----------------------------------------------------------------------------
# Stop cluster
parallel::stopCluster(cl)

