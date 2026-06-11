Auction Modeling
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# auctionr

A package for R to estimate private-value auction models while allowing
for unobservable auction-specific heterogeneity.

## Installation

``` r
# Install auctionr from CRAN
install.packages("auctionr")

# Or the development version from GitHub:
# install.packages("remotes")
# library(remotes)
install_github("ajmack/auctionr", build_vignettes = T)
```

## Getting started

There are two functions available in the package:

  - `auction_generate_data()` allows the user to generate sample data
    from the principal model used in the package.

  - `auction_model()` calculates maximum likelihood estimates of
    parameters of the principal model for the data provided by the user.

<!-- end list -->

``` r
library(auctionr)

set.seed(100)
dat <- auction_generate_data(obs = 100, mu = 10, alpha = 2, sigma = 0.2,
                             beta = c(-1,1), new_x_mean= c(-1,1), new_x_sd = c(0.5,0.8))

res <- auction_model(dat,
                    init_param =  c(8, 2, .5, .4, .6),
                    num_cores = 1,
                    method = "BFGS",
                    control = list(trace=1, parscale = c(1,0.1,0.1,1,1)),
                    std_err = TRUE)
```

    ## initial  value 1339.327262 
    ## iter  10 value 434.301377
    ## iter  20 value 410.711195
    ## final  value 410.710822 
    ## converged
    ## 

``` r
res
```

    ## 
    ## Estimated parameters (SE):                             
    ##   mu      11.012673 (1.152635)
    ##   alpha    1.752769 (0.185499)
    ##   sigma    0.204230 (0.035286)
    ##   beta[1] -0.920617 (0.057040)
    ##   beta[2]  1.068096 (0.040026)
    ## 
    ## Maximum log-likelihood = -410.711

## For further information

Background and details about the model implemented here are available in
[Mackay, Alexander. 2020. *Contract Duration and the Costs of Market
Transactions.*](https://www.hbs.edu/faculty/Pages/item.aspx?num=53718).
