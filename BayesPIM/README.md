
# BayesPIM: Bayesian prevalence-incidence mixture model

<!-- badges: start -->
<!-- badges: end -->

BayesPIM is a versatile modeling environment for screening and
surveillance data, as described in detail in the main reference Klausch
et al. (2024). It is a so-called prevalence-incidence mixture (PIM)
model with a Bayesian Gibbs sampler as the estimation backend. For
details on usage see the `vignette("BayesPIM_intro")`.

## Installation

You can install the development version of BayesPIM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thomasklausch2/BayesPIM", build_vignettes = TRUE)
```

## Example

This is a basic example of data generation and model fitting.

``` r
library(BayesPIM)

# Generate data according to the Klausch et al. (2024) PIM
set.seed(2025)
dat = gen.dat(kappa = 0.7, n= 1e3, theta = 0.2,
              p = 1, p.discrete = 1,
              beta.X = c(0.2,0.2), beta.W = c(0.2,0.2),
              v.min = 20, v.max = 30, mean.rc = 80,
              sigma.X = 0.2, mu.X=5, dist.X = "weibull",
              prob.r  = 1)

# An initial model fit with fixed test sensitivity kappa (approx. 1-3 minutes, depending on machine)
mod = bayes.2S( Vobs = dat$Vobs,
                Z.X = dat$Z,
                Z.W = dat$Z,
                r= dat$r,
                kappa = 0.7,
                update.kappa = F,
                ndraws= 1e4,
                chains = 4,
                prop.sd.X = 0.008,
                parallel = T,
                dist.X = 'weibull'
)

# Inspect results
mod$runtime # runtime of Gibbs sampler
plot( trim.mcmc( mod$par.X.all, thining = 10) ) # MCMC chains including burn-in
plot( trim.mcmc( mod$par.X.bi, thining = 10) ) # MCMC chains excluding burn-in
apply(mod$ac.X, 2, mean) # Acceptance rates per chain
gelman.diag(mod$par.X.bi) # Gelman convergence diagnostics
```

## References

T. Klausch, B. I. Lissenberg-Witte, and V. M. Coupe (2024). “A Bayesian
prevalence-incidence mixture model for screening outcomes with
misclassification.” arXiv:2412.16065. <https://arxiv.org/abs/2412.16065>
