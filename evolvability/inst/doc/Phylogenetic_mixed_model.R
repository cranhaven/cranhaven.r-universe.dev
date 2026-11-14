## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(evolvability)

## -----------------------------------------------------------------------------
# Only a very small sample size is used 
# in the interest of computational speed:
set.seed(57)
n_species <- 50 
tree <- ape::rtree(n = n_species)
tree <- ape::chronopl(tree, lambda = 1)

## -----------------------------------------------------------------------------
A <- Matrix::Matrix(ape::vcv(tree), sparse = TRUE)

## -----------------------------------------------------------------------------
colnames(A) <- rownames(A) <- paste("species", 1:n_species, sep = "_")

## -----------------------------------------------------------------------------
y <- 5 + t(chol(A))%*%rnorm(n_species, 0, 2) + # BM process with mean = 5 and sd = 2
     rnorm(n_species, 0, 1)                    # residual variation with sd = 1

## -----------------------------------------------------------------------------
dt <- data.frame(species = colnames(A), y = as.vector(y))

## -----------------------------------------------------------------------------
mod <- Almer(y ~ 1 + (1|species), data = dt, A = list(species = A))
summary(mod)

## -----------------------------------------------------------------------------
dt$SE <- runif(nrow(dt), min = 0.01, max = 0.02) 

## -----------------------------------------------------------------------------
mod_SE <- Almer_SE(y ~ 1 + (1|species), data = dt, SE = dt$SE, A = list(species = A))
summary(mod_SE)

## -----------------------------------------------------------------------------
sim_y <- Almer_sim(mod, nsim = 3)
sim_y[1:3,]

## -----------------------------------------------------------------------------
# The number of bootstrap simulations is kept very low in the interest 
# of computational speed. Often 1000 is used in real analyses.
Almer_boot_obj <- Almer_boot(mod, nsim = 10) 
Almer_boot_obj$fixef
Almer_boot_obj$vcov

## -----------------------------------------------------------------------------
# The number of bootstrap simulations is kept very low in the interest 
# of computational speed. Often 1000 is used in real analyses.
phylH_obj <- phylH(mod, numerator = "species", nsim = 10) 
phylH_obj$phylH

