
# heterogenuouspeereffects

<!-- badges: start -->
<!-- badges: end -->

The goal of heterogeneouspeereffects is to estimate through 2 step GMM methods the peer effect within groups (theta_within) and peer effect inter groups(theta_between). This package enables to have heterogeneous peer effects.


## Installation

You can install the development version of heterogenuouspeereffects like so:

``` r
devtools::install_github("ton-user-github/heterogeneouspeereffects")
```

## Example

TThis example simulates data consistent with the model and estimates the direct effect, the within-group peer effect, and the between-group peer effect using `heter_endo_gmm()`:

``` r
library(heterogeneouspeereffects)

# True parameters
delta   <- -3
thetaW  <- 0.7
thetaB  <- 0.3
beta_NE <- -2
beta_E  <- -1
G <- 1000

# Individual heterogeneity (alpha)
mu_alphaX    <- c(8, 4, -3, 2)
Sigma_alphaX <- matrix(c(4, 1, 0, 0,
                          1, 4, 0, 0,
                          0, 0, 4, -4.2,
                          0, 0, -4.2, 9), nrow = 4)
alphaX <- mvtnorm::rmvnorm(n = G, mean = mu_alphaX, sigma = Sigma_alphaX)

# Share of eligible individuals, correlated with treatment D
logistic <- function(x) 1 / (1 + exp(-x))
s <- runif(n = G)
scaled_s <- (s - mean(s)) / sd(s)
prob_D <- logistic(scaled_s)
D <- rbinom(n = G, size = 1, prob = prob_D)

# Simulate group-level outcomes
YN <- ((1 - thetaW * s) * (alphaX[,1] + beta_NE * alphaX[,3]) +
        thetaB * s * (alphaX[,2] + beta_E * alphaX[,4]) +
        delta * thetaB * s * D) /
      (1 - thetaW + s * (1 - s) * (thetaW^2 - thetaB^2))

YE <- ((1 - thetaW * s) * (1 - s) * thetaB * (alphaX[,1] + beta_NE * alphaX[,3]) +
        (1 + thetaW * (s * (1 - s) * thetaW - 1)) * (alphaX[,2] + beta_E * alphaX[,4]) +
        delta * (1 + thetaW * (s * (1 - s) * thetaW - 1)) * D) /
      ((1 - thetaW * s) * (1 - thetaW + s * (1 - s) * (thetaW^2 - thetaB^2)))

# Estimate the model
result <- heter_endo_gmm(YE, YN, D, s)
result
```

The output is a data frame with the estimated direct effect (`delta`), the within-group peer effect (`theta_within`), the between-group peer effect (`theta_between`), and their standard errors.

For the version with identity orthogonal to eligibility (e.g. gender-based groups), see `ortho_heter_endo_gmm()` and the corresponding vignette: `vignette("Intro_to_ortho_heter_endo_gmm", package = "heterogeneouspeereffects")`.