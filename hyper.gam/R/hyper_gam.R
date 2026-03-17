

#' @title \link[mgcv]{gam} with \link[base]{matrix} predictor
#' 
#' @description
#' A generalized additive model \link[mgcv]{gam} with one-and-only-one \link[base]{matrix} predictor.
#' 
#' 
#' @param formula \link[stats]{formula}, e.g., `y~X`, in which
#' \itemize{
#' \item {Response \eqn{y} may be \link[base]{double}, \link[base]{logical} and \link[survival]{Surv}}
#' \item {Predictor \eqn{X} is a \link[base]{double} \link[base]{matrix},
#' the \link[base]{colnames} of which must be convertible to \link[base]{numeric} \link[base]{vector},
#' indicating a *common tabulating grid* shared by all subjects.}
#' }
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param family \link[stats]{family} object, 
#' see function \link[mgcv]{gam} for details.
#' Default values are
#' \itemize{
#' \item `mgcv::cox.ph()` for \link[survival]{Surv} response \eqn{y};
#' \item `stats::binomial(link = 'logit')` for \link[base]{logical} response \eqn{y};
#' \item `stats::gaussian(link = 'identity')` for \link[base]{double} response \eqn{y}
#' }
#' 
#' @param nonlinear \link[base]{logical} scalar, 
#' whether to use nonlinear or linear functional model.
#' Default `FALSE`
#' 
#' @param ... additional parameters for functions \link[mgcv]{s} and \link[mgcv]{ti},
#' most importantly `k`
#' 
#' @details
#' The function [hyper_gam()] fits a \link[mgcv]{gam} model
#' of response \eqn{y} with matrix predictor \eqn{X}.
#' This method was originally defined in the context of \link[stats]{quantile}.
#' In the following text, the matrix predictor \eqn{X} is denoted as \eqn{Q(p)},
#' where \eqn{p} is `as.numeric(colnames(X))`.
#' 
#' Linear quantile index, with a linear functional coefficient \eqn{\beta(p)},
#' \deqn{\text{QI}=\displaystyle\int_0^1\beta(p)\cdot Q(p)\,dp}
#' can be estimated by fitting a functional generalized linear model (FGLM, James, 2002) to exponential-family outcomes, 
#' or by fitting a linear functional Cox model (LFCM, Gellar et al., 2015) to survival outcomes. 
#' 
#' Non-linear quantile index, with a bivariate twice differentiable function \eqn{F(\cdot,\cdot)},
#' \deqn{\text{nlQI}=\displaystyle\int_0^1 F\big(p, Q(p)\big)\,dp}
#' can be estimated by fitting a functional generalized additive model (FGAM, McLean et al., 2014) to exponential-family outcomes, 
#' or by fitting an additive functional Cox model (AFCM, Cui et al., 2021) to survival outcomes. 
#' 
#' 
#' 
#' 
#' 
#' @returns 
#' The function [hyper_gam()] returns a [hyper_gam] object, 
#' which \link[base]{inherits} from class \link[mgcv]{gam}.
#' 
#' @author 
#' Tingting Zhan, Erjia Cui
#' 
#' @references 
#' James, G. M. (2002). *Generalized Linear Models with Functional Predictors*,
#' \doi{10.1111/1467-9868.00342} 
#' 
#' Gellar, J. E., et al. (2015). *Cox regression models with functional covariates for survival data*,
#' \doi{10.1177/1471082X14565526}
#' 
#' Mathew W. M., et al. (2014) *Functional Generalized Additive Models*,
#' \doi{10.1080/10618600.2012.729985}
#' 
#' Cui, E., et al. (2021). *Additive Functional Cox Model*,
#' \doi{10.1080/10618600.2020.1853550}
#' 
#' @keywords internal
#' @importFrom mgcv gam cox.ph s ti
#' @export
hyper_gam <- function(
    formula, data,
    family,
    nonlinear = FALSE,
    ...
) {

  xname <- formula[[3L]] # right-hand-side
  data_aug <- augdata(data = data, xname = xname)
  
  trm_ <- if (nonlinear) as.call(list(
    quote(ti), 
    quote(x), # `x`: augmented column
    xname, 
    by = quote(L), # `L`: augmented column
    bs = 'cr', # cubic regression spline
    mc = c( # see ?mgcv::ti; which marginals should have centering constraints applied
      FALSE, 
      TRUE
    ), 
    ...
  )) else as.call(list(
    quote(s),
    quote(x), # `x`: augmented column
    by = call(name = '*', quote(L), xname), # `L`: augmented column
    bs = 'cr', ...
  ))
  
  y <- eval(formula[[2L]], envir = data_aug)
  
  gam_cl <- call(
    name = 'gam', 
    data = quote(data_aug),
    control = list(keepData = TRUE)
  ) 
  
  if (inherits(y, what = 'Surv')) {
    gam_cl$formula <- call(name = '~', call(name = '[', formula[[2L]], alist(x = )[[1L]], 1L), trm_)
    gam_cl$weights <- call(name = '[', formula[[2L]], alist(x = )[[1L]], 2L)
    gam_cl$family <- if (missing(family)) quote(cox.ph()) else substitute(family)
  } else {
    gam_cl$formula <- call(name = '~', formula[[2L]], trm_)
    gam_cl$family <- if (!missing(family)) {
      substitute(family) 
    } else if (is.logical(y) || all(y %in% c(0, 1))) {
      quote(binomial(link = 'logit'))
    } else if (is.numeric(y)) {
      quote(gaussian(link = 'identity'))
    } else stop('not supported yet')
  }
  
  ret <- eval(gam_cl) # 'gam' if `fit = TRUE`; 'gam.prefit' if `fit = FALSE`
  
  attr(ret, which = 'xname') <- xname
  if (inherits(ret, what = 'gam')) {
    # instead of 'gam.prefit' (when `fit = FALSE`)
    class(ret) <- c('hyper_gam', class(ret))
  }
  return(ret)
  
}




augdata <- function(
    data, xname
) {
  
  if (
    all(c('L', 'x') %in% names(data)) &&
    is.matrix(data$L) && is.matrix(data$x)
  ) {
    # `data` already augmented
    return(data)
  }
  
  if (any(c('L', 'x') %in% names(data))) {
    stop('input data cannot contain names `x` and `L`')
  }
  
  if (!is.symbol(xname)) stop('Right-hand-side ', xname |> deparse1() |> col_magenta(), ' must be a symbol')
  
  X <- data[[xname]]
  if (!inherits(X, what = 'listof')) stop(xname |> col_blue(), ' in `data` must be hypercolumn')
  
  X. <- X |> do.call(what = rbind)
  
  cnm <- X. |> 
    colnames()
  x. <- if (all(grepl(pattern = '%$', x = cnm))) {
    # returned from ?stats:::quantile.default
    tmp <- cnm |>
      gsub(pattern = '%$', replacement = '') |>
      as.double()
    tmp / 1e2
  } else cnm |> as.double() 
  
  if (!length(x.) || !is.numeric(x.) || anyNA(x.) || is.unsorted(x., strictly = TRUE)) {
    stop(xname |> col_blue(), ' must have names convertible to strictly-increasing numerics')
  }
  
  colnames(X.) <- x.
  
  dat <- unclass(data)$df
  dat[[xname]] <- X.
  dat$x <- tcrossprod(rep(1, times = length(X)), x.)
  
  nx <- length(x.)
  # for numeric integration of the functional term - Erjia's comment
  dat$L <- array(1/nx, dim = dim(X.))
  
  return(dat)
  
}



