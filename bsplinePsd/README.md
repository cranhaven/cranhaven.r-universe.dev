## Why should I use bsplinePsd?
This package allows the user to flexibly estimate the spectral density of a stationary time series using a Bayesian nonparametric B-spline prior (of any degree).  It works particularly well for complicated spectral structures (compared to the Bernstein polynomial prior).

## How do I use bsplinePsd?
The primary function gibbs_bspline is straightforward to use.  Most of the arguments are defaults (i.e., a noninformative prior).  All you need to do is input a numeric vector (your time series), the number of iterations to run the MCMC algorithm for, and the amount of burn-in.  

## How do I get bsplinePsd?
Download from CRAN.  Use install.packages("bsplinePsd") in R.