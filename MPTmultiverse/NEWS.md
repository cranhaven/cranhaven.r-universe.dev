
# MPTmultiverse 0.4-1

- Adapted MPTinR unit tests for no pooling approaches to fail even less with 
    R 4.0.0.

# MPTmultiverse 0.4-0

- Added a `NEWS.md` file to track changes to the package.
- Added within-subjects comparisons of parameter estimates that are stored in column
    `test_within` of the results object.
    - For the Bayesian methods, these comparisons rely on posterior distributions
        of parameter differences.
    - For the no-pooling maximum-likelihood methods, comparisons rely on paired *t* tests.
    - For the complete-pooling maximum-likelihood method, comparisons rely on the the point estimates
       of parameters and the estimates of the standard errors of differences calculated
       from the Hessian matrix.
- Changed the default behavior for auto-extending Bayesian models: As of now, chains
    are only combined if Rhat <= Rhat_max*2-1. An informative message is printed whether chains were
    combined with earlier draws or not.
- Prepared `MPTmultiverse` for new `tibble` version.
