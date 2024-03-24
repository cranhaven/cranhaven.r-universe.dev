
#' Ternary Plots for Trinomial Regression Models
#'
#' An implementation of the ternary plot for interpreting regression
#' coefficients of trinomial regression models, as proposed in
#' \insertCite{santi2019;textual}{plot3logit}. For details on the features of
#' the package, see \insertCite{santi2022;textual}{plot3logit}.
#'
#' The package permits the covariate effects of trinomial regression models to
#' be represented graphically by means of a ternary plot. The aim of the
#' plots is helping the interpretation of regression coefficients in terms of
#' the effects that a change in regressors' values has on the probability
#' distribution of the dependent variable. Such changes may involve either a
#' single regressor, or a group of them (composite changes), and the package
#' permits both cases to be represented in a user-friendly way. Methodological
#' details are illustrated and discussed in
#' \insertCite{santi2019;textual}{plot3logit}.
#'
#' The package can read the results of **both categorical and ordinal trinomial
#' logit** regression fitted by various functions (see [extract3logit()]) and
#' creates a `field3logit` object which may be represented by means of functions
#' [autoplot()] and [plot()].
#'
#' The `plot3logit` package inherits graphical classes and methods from the
#' package [`ggtern`][ggtern::ggtern_package]
#' \insertCite{hamilton2018}{plot3logit} which, in turn, is based on the
#' [`ggplot2`][ggplot2::ggplot2-package] package
#' \insertCite{wickham2016a}{plot3logit}.
#'
#' Graphical representation based on **standard graphics** is made available
#' through the package `Ternary` \insertCite{smith2017}{plot3logit} by function
#' [TernaryField()] and in particular by the method [`plot`][plot.field3logit]
#' of `field3logit` class.
#'
#' Since version 2.0.0, `plot3logit` can also compute and draw confidence
#' regions associated to the covariate effects. See the vignette of the
#' package (type `vignette("plot3logit-overview")`) and the help of function
#' [stat_conf3logit()] for some examples.
#'
#' @section Compatibility:
#' Function [field3logit()] can read trinomial regression estimates from the
#' output of the following functions:
#' * [`clm`][ordinal::clm] and [`clm2`][ordinal::clm2] of package `ordinal`
#'   (ordinal logit regression);
#' * [`mlogit`][mlogit::mlogit] of package `mlogit` (logit regression);
#' * [`multinom`][nnet::multinom] of package `nnet` (logit regression);
#' * [`polr`][MASS::polr] of package `MASS` (ordinal logit regression);
#' * [`vgam`][VGAM::vgam] and [`vglm`][VGAM::vglm] of package `VGAM` (logit
#'   regression).
#'
#' Moreover, explicit estimates can be passed to [field3logit()]. See examples
#' and functions [field3logit()] and [extract3logit()] for further details.
#'
#' @examples
#' \donttest{
#' data(cross_1year)
#'
#' # Read from "nnet::multinom" (categorical logit)
#' library(nnet)
#' mod0 <- multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
#' field0 <- field3logit(mod0, 'genderFemale')
#' gg3logit(field0) + stat_field3logit()
#'
#' # Read from "MASS::polr" (ordinal logit)
#' library(MASS)
#' mydata <- cross_1year
#' mydata$finalgrade <- factor(mydata$finalgrade,
#'   c('Low', 'Average', 'High'), ordered = TRUE)
#' mod1 <- polr(finalgrade ~ gender + irregularity, data = mydata)
#' field1 <- field3logit(mod1, 'genderFemale')
#' gg3logit(field1) + stat_field3logit()
#'
#' # Read from list
#' mod2 <- list(
#'   B = matrix(
#'     data = c(-2.05, 0.46, -2.46, 0.37),
#'     nrow = 2,
#'     dimnames = list(c('(Intercept)', 'genderFemale'))
#'   ),
#'   levels = c('Employed', 'Unemployed', 'Trainee')
#' )
#' field2 <- field3logit(mod2, c(0, 1))
#' gg3logit(field2) + stat_field3logit()
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [field3logit()], [gg3logit()], [TernaryField()].
#'
#' @encoding UTF-8
#'
#' @docType package
#' @name plot3logit-package
#' @aliases plot3logit-package
#'
#' @import magrittr Ternary ggtern
#'
#' @importFrom dplyr bind_rows mutate pull select
#' @importFrom generics tidy
#' @importFrom ggplot2 arrow autoplot fortify unit
#' @importFrom graphics points arrows plot
#' @importFrom lifecycle deprecated
#' @importFrom Rdpack reprompt
#' @importFrom stats coef uniroot vcov
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom utils modifyList tail
#'
"_PACKAGE"

utils::globalVariables('.')


#' @export
ggplot2::autoplot

#' @export
generics::tidy



