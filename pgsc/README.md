# pgsc

The goal of pgsc is to provide an estimation and testing framework for Powell's Generalized Synthetic Control method.  This provides consistent estimates in the presence of unobserved spatially-correlated factors in a panel.  Please see the vignette for further details and an extended example.

## Installation

You can install pgsc from github with:

```{r gh-installation, eval = FALSE}
devtools::install_github("philipbarrett/pgsc")
```

## Example

This is a basic example which provides estimation and testing for a dataset with omitted variables that cannot be addressed by time and unit fixed effects.  Please see the vignette for further details on this example and other options.

```{r example}
#' data("pgsc.dta")
#' library(plm)
#' pan <- plm( y ~ D1 + D2 + X1 + X2 + X3, pgsc.dta, effect = 'twoways', index = c('n','t'))
#' summary(pan)
#'    # Failure of panel estimation: the true coefficients on D1, D2 are c(1,2), 
#'    # which are not recovered due to omitted variables which are spatially
#'    #  correlated.  The "twoway" (time/unit) fixed effects cannot pick this up.
#' sol <- pgsc(pgsc.dta, dep.var = 'y', indep.var = c('D1','D2'), 
#'              b.init = c(0,0), method='twostep.indiv' )
#' summary(sol)
#'    # The unrestricted estimation. 
#' g.i <- function(b) b[1] ; g.i.grad <- function(b) c(1,0)
#' sol.r <- pgsc(pgsc.dta, dep.var = 'y', indep.var = c('D1','D2'), 
#'              b.init = sol$b, method='twostep.indiv', g.i=g.i, g.i.grad=g.i.grad )
#'    # Restricted estimation under the hypothesis that b[1]=0
#' summary(sol.r)
#' wald <- pgsc.wald.test( pgsc.dta, 'y', indep.var = c('D1','D2'), sol.r )
#' summary(wald)
#' plot(wald)
#'    # Testing the hypothesized restriction.  It is comfortably rejected.
```

You can access the vignette with

```{r example}
#' browseVignettes('pgsc')
```

