
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `ordPens`: Selection and/or Smoothing and Principal Components Analysis for Ordinal Variables

<!-- badges: start -->
<!-- badges: end -->

We provide selection, and/or smoothing/fusion of ordinally scaled
independent variables using a group lasso or generalized ridge penalty.
In addition, nonlinear principal components analysis for ordinal
variables is offered, using a second-order difference penalty.

Also, ANOVA with ordered factors is provided by the function `ordAOV`;
testing for differentially expressed genes can be done using `ordGene`.
For details cf. Gertheiss (2014) and Sweeney et al. (2015),
respectively.

For smoothing, selection and fusion, details may be found in Tutz and
Gertheiss (2014, 2016). All functions are documented in detail in
`vignette("ordPens", package = "ordPens")`. For smoothing only, the
package also builds a bridge to `mgcv::gam()`, see Gertheiss et
al. (2022) for further information.

For the function implementing nonlinear principal components analysis,
`ordPCA`, details can be found in Hoshiyar et al. (2021) and
`vignette("ordPCA", package = "ordPens")`.

Version 1.1.0 is a minor release with new functions:

-   Functions `ordSelect`, `ordFusion` updated/extended to cumulative
    logit model models.
-   Function `ordCV` added, provides cross-validation for penalized
    regression models with ordinal predictors.  
-   Function `StabilityCumu` added, provides stability selection for
    penalized cumulative logit models.

Version 1.0.0 is a major release with new functions:

-   `ordPCA` applies nonlinear principal components analysis for ordinal
    variables. Also, performance evaluation and selection of an optimal
    penalty parameter provided.  
-   `ordFusion` fits dummy coefficients of ordinally scaled independent
    variables with a fused lasso penalty for fusion and selection.
-   A new type of spline basis for ordered factors
    `s(..., bs = "ordinal")`is provided, such that smooth terms in the
    `mgcv::gam()` formula can be used as an alternative and extension to
    `ordSmooth()`. Additionally, generic functions for prediction and
    plotting are provided.

## Installation & getting started

For standard use, install `ordPens` from
[CRAN](https://cran.r-project.org/package=ordPens):

``` r
install.packages("ordPens")
```

The development version of the package may be installed from GitHub:

``` r
devtools::install_git("https://github.com/ahoshiyar/ordPens", build_vignettes = TRUE)
```

For a detailed overview about the functionalities and given examples
type:

``` r
library(ordPens)
vignette("ordPens", package = "ordPens")
vignette("ordPCA", package = "ordPens")
```

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/ahoshiyar/ordPens/issues).

## Contributions & Code of conduct

Contributions are very welcome. Interested contributors should consult
the [contribution
guidelines](https://github.com/ahoshiyar/ordPens/blob/master/Contributing.md)
prior to submitting a pull request.

Please note that the `ordPens` package is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/).
By contributing to this project, you agree to abide by its terms.

## References

-   Gertheiss, J. (2014). ANOVA for factors with ordered levels.
    *Journal of Agricultural, Biological and Environmental Statistics
    19*, 258-277.

-   Gertheiss, J., F. Scheipl, T. Lauer, and H. Ehrhardt (2022).
    Statistical inference for ordinal predictors in generalized linear
    and additive models with application to bronchopulmonary dysplasia.
    *BMC research notes 15*, 112.

-   Hoshiyar, A., H.A.L. Kiers, and J. Gertheiss (2021). Penalized
    non-linear principal components analysis for ordinal variables with
    an application to international classification of functioning core
    sets. *British Journal of Mathematical and Statistical Psychology
    76*, 353-371.

-   Hoshiyar, A., Gertheiss, L.H., and Gertheiss, J. (2023).
    Regularization and model selection for item-on-items regression with
    applications to food products’ survey data. Preprint, available from
    <https://arxiv.org/abs/2309.16373>.

-   Sweeney, E., C. Crainiceanu, and J. Gertheiss (2015). Testing
    differentially expressed genes in dose-response studies and with
    ordinal phenotypes. *Statistical Applications in Genetics and
    Molecular Biology 15*, 213-235.

-   Tutz, G. and J. Gertheiss (2014). Rating scales as predictors – the
    old question of scale level and some answers. *Psychometrica 79*,
    357-376.

-   Tutz, G. and J. Gertheiss (2016). Regularized regression for
    categorical data. *Statistical Modelling 16*, 161-200.
