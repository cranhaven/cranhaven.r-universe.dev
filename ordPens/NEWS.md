
# ordPens 1.1.0

Minor revision with added functionality.

-   Functions `ordSelect`, `ordFusion` updated/extended to cumulative
    logit model models.
-   Function `ordCV` added, provides cross-validation for penalized
    regression models with ordinal predictors.
-   Function `StabilityCumu` added, provides stability selection for
    penalized cumulative logit models.

# ordPens 1.0.0

Major revision with added functionality.

-   Function `ordPCA` added for nonlinear principal components analysis
    for ordinal variables. Also, performance evaluation and selection of
    an optimal penalty parameter provided.  
-   Function `ordFusion` added for fusion and selection of dummy
    coefficients of ordinal predictors. Similar syntax as for
    `ordSmooth()` and `ordSelect()`; generic functions `plot` and
    `predict` work in the same manner.
-   In addition, auxiliary functions included such that `mgcv::gam()`
    can be used for fitting generalized linear and additive models with
    first- and second-order ordinal smoothing penalty as well as
    built-in smoothing parameter selection. A new type of spline basis
    for ordered factors `s(..., bs = "ordinal")`is provided, such that
    smooth terms in the GAM formula can be used as an alternative and
    extension to `ordSmooth()`. Additionally, generic functions for
    prediction and plotting are provided. Also, `mgcv` tools for further
    statistical inference can be used.

Two additional vignettes called `ordPens` and `ordPCA` are supplied to
give an overview and describe the usage of the package.

Bug fixes

-   For `plot.ordPen()`, arguments `whichlam` and `whichx` are changed
    to `whl` and `whx`, respectively.

# ordPens 0.3-1

Minor revision with added functionality.

-   Function `ordGene` added, which provides testing for differentially
    expressed genes.

-   Changed documentation of function `ordAOV`: argument `null.sample`
    added, containing values already simulated from the null
    distribution.

Fixed some minor bugs / cleaned up code.

# ordPens 0.2-1

-   The package starts from version 0.2-1
