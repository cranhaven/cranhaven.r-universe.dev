#### MSOpt ####

#' Experimental setup
#'
#' @description The \code{MSOpt} function allows the user to define the
#' structure of the experiment, the set of optimization criteria and the a priori
#' model to be considered. The output is a list containing all information about
#' the settings of the experiment. According to the declared criteria, the list
#' also contains the basic matrices for their implementation, such as information
#' matrix, matrix of moments and matrix of weights. The returned list
#' is argument of the \code{\link[multiDoE]{Score}} and \code{\link[multiDoE]{MSSearch}}
#' functions of the \code{multiDoE} package.
#'
#' @usage MSOpt(facts, units, levels, etas, criteria, model)
#'
#' @param facts A list of vectors representing the distribution of factors
#' across strata. Each item in the list represents a stratum and the first item
#' is the highest stratum of the multi-stratum structure of the experiment.
#' Within the vectors, experimental factors are indicated by progressive integer
#' from \eqn{1} (the first factor of the highest stratum) to the total number of
#' experimental factors (the last factor of the lowest stratum). Blocking
#' factors are denoted by empty vectors.
#'
#' @param units A list whose \eqn{i}-th element is the number of experimental
#' units within each unit at the previous stratum \eqn{i-1}.
#' The first item in the list, \eqn{n_1}{n1}, represents the number of experimental
#' units in the stratum \eqn{0}, defined as the entire experiment (so that
#' \eqn{n_0 = 1}{n0 = 1}).
#'
#' @param levels A vector containing the number of available levels for each
#' experimental factor in \code{facts} (blocking factors are excluded). If all
#' experimental factors share the number of levels one integer is sufficient.
#'
#' @param etas A list specifying the ratios of error variance between subsequent
#' strata. It follows that \code{length(etas)} must be equal to
#' \code{length(facts)-1}.
#'
#' @param criteria A list specifying the criteria to be optimized. It can
#' contain any combination of:
#' \itemize{
#'   \item{``I" : I-optimality}
#'   \item{``Id" : Id-optimality}
#'   \item{``D" : D-optimality}
#'   \item{``A" : Ds-optimality}
#'   \item{``Ds" : A-optimality}
#'   \item{``As" : As-optimality}
#' }
#' More detailed information on the available criteria is given under
#' \strong{Details}.
#'
#' @param model A string which indicates the type of model, among ``main",
#' ``interaction" and ``quadratic".
#'
#' @details A little notation is introduced to show the criteria that can be
#' used in the multi-objective approach of the \code{multiDoE} package. \cr
#'
#' For an experiment with \eqn{N} runs and \eqn{s} strata, with stratum \eqn{i}
#' having \eqn{n_i}{ni} units within each unit at stratum \eqn{i-1} and
#' stratum 0 being defined as the entire experiment (\eqn{n_0 = 1}{n0 = 1}), the
#' general form of the model can be written as:
#'
#' \deqn{y = X\beta + \sum\limits_{i = 1}^{s} Z_i\varepsilon_i}{y = X\beta +
#' \sum{i=1}^{s} Zi \epsiloni,}
#'
#' where
#' \eqn{y} is an \eqn{N}-dimensional vector of responses (\eqn{N = \prod_{j = 1}^{s}n_j}{N = \prod{j = 1}^{s}nj}),
#' \eqn{X} is an \eqn{N} by \eqn{p} model matrix,
#' \eqn{\beta} is a \eqn{p}-dimensional vector containing the \eqn{p} fixed model parameters,
#' \eqn{Z_i}{Zi} is an \eqn{N} by \eqn{b_i}{bi} indicator matrix of \eqn{0} and
#'     \eqn{1} for the units in stratum \eqn{i} (i.e. the (\eqn{k,l})th element
#'     of \eqn{Z_i}{Zi} is \eqn{1} if the \eqn{k}th run belongs to the \eqn{l}th
#'     block in stratum \eqn{i} and \eqn{0} otherwise) and
#' \eqn{b_i = \prod_{j = 1}^{i}n_j}{bi = \prod{j = 1}^{i}nj}.
#' Finally, the vector \eqn{\varepsilon_i \sim N(0,\sigma_i^2I_{b_i})}{%
#' \epsiloni ~ N(0, \sigmai^{2} I_{bi})} is a \eqn{b_i}{bi}-dimensional vector
#' containing the random effects, which are all uncorrelated. The variance
#' components \eqn{\sigma^{2}_{i} (i = 1, \dots, s)}{\sigmai^{2}
#' (i = 1, \dots, s)} have to be estimated and this is usually done using
#' the REML (REstricted Maximum Likelihood) method.
#'
#' The best linear unbiased estimator for the parameter vector \eqn{\beta} is
#' the generalized least square estimator:
#' \deqn{ \hat{\beta}_{GLS} = (X'V^{-1}X)^{-1}X'V^{-1}y}{%
#'        hat{\beta}_{GLS} = (X' V^{-1} X)^{-1} X' V^{-1} y.}
#'
#' This estimator has variance-covariance matrix:
#' \deqn{Var(\hat{\beta}_{GLS}) = \sigma^{2}(X'V^{-1}X)^{-1}}{%
#'       Var(hat{\beta}_{GLS}) = \sigma^{2} (X' V^{-1} X)^{-1}, }
#'
#' where
#' \eqn{V = \sum\limits_{i = 1}^{s}\eta_i Z_i'Zi}{V = \sum{i = 1}^{s} (\etai Zi'Zi)},
#' \eqn{\eta_i = \frac{\sigma_i^{2}}{\sigma^{2}}}{\etai = \sigmai^{2} / \sigma^{2}} and
#' \eqn{\sigma^{2} = \sigma^{2}_{s}}{\sigma^{2} = \sigmas^{2}}.
#'
#' Let \eqn{M = (X' V^{-1} X)} be the information matrix about \eqn{\hat{\beta}}{%
#' hat{\beta}} and let \eqn{\eta} be the vector of the variance components.
#'
#' \itemize{
#' \item{ \strong{D-optimality.} It is based on minimizing the generalized
#' variance of the parameter estimates. This can be done either by minimizing the
#' determinant of the variance-covariance matrix of the factor effects' estimates
#' or by maximizing the determinant of \eqn{M}. \cr
#' The objective function to be minimized is:
#' \deqn{f_{D}(d; \eta) = \left(\frac{1}{\det(M)}\right)^{1/p}}{%
#'       f_D(d; \eta) = (1 / det(M))^{1/p},}
#' where \eqn{d} is the design with information matrix \eqn{M} and \eqn{p} is the
#' number of model parameters.}
#'
#' \item{ \strong{A-optimality.} This criterion is based on
#' minimizing the average variance of the estimates of the regression coefficients.
#' The sum of the variances of the parameter estimates (elements of
#' \eqn{\hat{\beta}}{hat{\beta}}) is taken as a measure, which is equivalent to
#' considering the trace of \eqn{M^{-1}}. \cr
#' The objective function to be minimized is:
#' \deqn{f_{A}(d; \eta) = trace(M^{-1})/p}{f_A(d; \eta) = trace(M^{-1})/p,}
#' where \eqn{d} is the design with information matrix \eqn{M} and \eqn{p} is the
#' number of model parameters.}
#'
#' \item{ \strong{I-optimality.} It seeks to minimize the average
#' prediction variance. \cr
#' The objective function to be minimized is:
#' \deqn{f_{I}(d; \eta) = \frac{\int_{\chi} f'(x)(M)^{-1}f(x)\,dx }{\int_{\chi} \,dx}}{%
#'       f_I(d; \eta) = (integral_{\chi} f'(x)(M)^{-1}f(x) dx) / (integral_{\chi} dx), }
#' where \eqn{d} is the design with information matrix \eqn{M} and \eqn{\chi}
#' represents the design region. \cr
#' It can be proved that when there are \eqn{k} treatment factors each with two
#' levels, so that the experimental region is of the form \eqn{[-1, +1]^{k}},
#' the objective function can also be written as:
#' \deqn{f_{I}(d; \eta) = trace \left[(M)^{-1} B\right]}{%
#'       f_I(d; \eta) = trace[(M)^{-1} B],}
#' where \eqn{d} is the design with information matrix \eqn{M} and
#' \eqn{B = 2^{-k} \int_{\chi}f'(x)f(x) \,dx }{%
#'      B = 2^{-k} (integral_{\chi} f'(x)f(x) dx)} is the moments matrix.
#' To know the implemented expression for calculating the moments matrix for a
#' cuboidal design region see section 2.3 of Sambo, Borrotti, Mylona, and Gilmour
#' (2016).}
#'
#' \item{ \strong{Ds-optimality.} Its aim is to minimize the generalized
#' variance of the parameter estimates by excluding the intercept from the set
#' of parameters of interest. Let \eqn{\beta_i}{\betai} be the model parameter
#' vector of dimension (\eqn{p_i - 1}{pi-1}) to be estimated in stratum \eqn{i}.
#' Let \eqn{X_i}{Xi} be the associated model matrix \eqn{m_i}{mi} by \eqn{(p_i-1)}{%
#' (pi-1)}, where \eqn{m_i}{mi} is the number of units in stratum \eqn{i}.
#' The partition of interest of the matrix of variances and covariances of
#' \eqn{\hat{\beta}_i}{hat{\betai}} is
#' \deqn{(M_i^{-1})_{22} = [X'_i (I - \frac{1}{m_i} 11^{'})X_i]^{-1}}{%
#' (Mi^{-1})_{22} = [Xi' (I - (1 / mi) 11') Xi]^{-1}.} \cr
#' The objective function to be minimized is:
#' \deqn{f_{D_s}(d; \eta) = (|(M_i^{-1})_{22}|)^{1/(p_i-1)}}{%
#' f_Ds(d; \eta) = (|(Mi^{-1})_{22}|)^{1/(pi-1)}.}}
#'
#'
#' \item{\strong{As-optimality.} This criterion is based on minimizing
#' the average variance of the estimates of the regression coefficients by excluding
#' the intercept from the set of parameters of interest. \cr
#' With reference to the notation introduced for the previous criterion, the
#' objective function to be minimized is:
#' \deqn{f_{A_s}(d; \eta) = trace(W_i(M_i^{-1})_{22})}{%
#'       f_As(d; \eta) = trace(Wi (Mi^{-1})_{22}),}
#' where \eqn{W_i}{Wi} is a diagonal matrix of weights, with the weights scaled
#' so that the trace of \eqn{W_i}{Wi} is equal to 1. Specifically the implemented
#' matrix assigns to each main effect and each interaction effect an absolute
#' weight equal to 1, while to the quadratic effects it assigns an absolute weight
#' equal to 1/4.}
#'
#' \item{ \strong{Id-optimality.} It seeks to minimize the average
#' prediction variance by excluding the intercept from the set of parameters of
#' interest. \cr
#' The objective function to be minimized is the same as the
#' I-optimality criterion where the first row and columns of the \eqn{B}
#' matrix (see the \strong{Id-optimality} criterion) are deleted.}
#'}
#'
#'
#'
#' @return \code{MSOpt} returns a list containing the following components:
#' \itemize{
#' \item{\code{facts}: The argument \code{facts}.}
#' \item{\code{nfacts}: An integer. The number of experimental factors (blocking factors are excluded from the count).}
#' \item{\code{nstrat}: An integer. The number of strata.}
#' \item{\code{units}: The argument \code{units}.}
#' \item{\code{runs}: An integer. The total number of runs.}
#' \item{\code{etas}: The argument \code{etas}.}
#' \item{\code{avlev}: A list showing the available levels for each experimental factor.
#' The design space for each factor is [-1, 1].}
#' \item{\code{levs}: A vector showing the number of available levels for each experimental factor.}
#' \item{\code{Vinv}: The inverse of the variance-covariance matrix of the responses.}
#' \item{\code{model}: The argument \code{model}.}
#' \item{\code{crit}: The argument \code{criteria}.}
#' \item{\code{ncrit}: An integer. The number of criteria considered.}
#' \item{\code{M}: The moment matrix. Only with I-optimality criteria.}
#' \item{\code{M0}: The moment matrix. Only with Id-optimality criteria.}
#' \item{\code{W}: The diagonal matrix of weights. Only with As-optimality criteria.}
#' }
#'
#' @references
#' M. Borrotti and F. Sambo and K. Mylona and S. Gilmour. A multi-objective
#' coordinate-exchange two-phase local search algorithm for multi-stratum
#' experiments. Statistics & Computing, 2017.
#'
#' S. G. Gilmour, J. M. Pardo, L. A. Trinca, K. Niranjan, D.S. Mottram. A
#' split-plot response surface design for improving aroma retention in freeze
#' dried coffee. In: Proceedings of the 6th. European conference on Food-Industry
#' Statist, 2000.
#'
#' @examples
#' ## This example uses MSOpt to setup a split-plot design with
#' ## 1 whole-plot factor and 4 subplot factors, which in the \code{facts}
#' ## element appear numbered from 2 to 5.
#' ## The experiment must be structured as follows: 6 whole plots and 5 subplots
#' ## per whole plot, for a total of 30 runs.
#' ## Each experimental factor has 3 different levels.
#'
#' ## To check the number of digits to be printed.
#' backup_options <- options()
#' options(digits = 10)
#'
#' facts <- list(1, 2:5)
#' units <- list(6, 5)
#' levels <- 3
#' etas <- list(1)
#' criteria <- c('I', 'D', 'A')
#' model <- "quadratic"
#'
#' msopt <- MSOpt(facts, units, levels, etas, criteria, model)
#'
#' options(backup_options)
#'
#' @export
MSOpt <- function(facts, units, levels, etas, criteria, model) {

  msopt <- list()

  msopt$facts <- facts
  msopt$nfacts <- length(unlist(facts))
  msopt$nstrat <- length(facts)
  msopt$units <- units
  msopt$runs <- prod(unlist(units))
  msopt$etas <- etas
  msopt$avlev <- as.list(rep(NA, msopt$nfact))

  if (length(levels) == 1) {
    msopt$levs <- rep(levels, msopt$nfacts)
    for (i in 1:msopt$nfacts) {
      msopt$avlev[[i]] <- (2 * 0:(levels - 1) / (levels - 1)) - 1
    }
  } else {
    msopt$levs <- levels
    for (i in 1:msopt$nfacts) {
      msopt$avlev[[i]] <- (2 * 0:(levels[[i]] - 1) / (levels[[i]] - 1)) - 1
    }
  }

  V <- diag(msopt$runs)
  for (i in 1:(msopt$nstrat - 1)) {
    if (i + 1 > length(units)) {
      ones_shape <- 1
    } else {
      ones_shape <- prod(unlist(units)[(i + 1):length(units)])
    }
    V <- V + etas[[1]] * kronecker(diag(prod(unlist(units)[1:i])),
                                   matrix(1, ones_shape, ones_shape))
  }

  msopt$Vinv <- t(solve(V))
  msopt$model <- model
  msopt$crit <- criteria
  msopt$ncrit <- length(criteria)

  if ("I" %in% criteria) {
    k <- msopt$nfacts
    k2 <- k * (k - 1) / 2
    switch (model,
           "main" = {
             msopt$M <- rbind(
               cbind(1, t(integer(k))),
               cbind(integer(k), diag(k) / 3)
               )
             },
           "interaction" = {
             msopt$M <- rbind(
               cbind(1, t(integer(k)), t(integer(k2))),
               cbind(integer(k), diag(k) / 3, matrix(0, k, k2)),
               cbind(integer(k2), matrix(0, k2, k), diag(k2) / 9)
               )
             },
           "quadratic" = {
             msopt$M <- rbind(
               cbind(1, t(integer(k)), t(rep(1, k)) / 3, t(integer(k2))),
               cbind(integer(k), diag(k) / 3, matrix(0, k, k), matrix(0, k, k2)),
               cbind(rep(1, k) / 3, matrix(0, k, k),
                     (4 * diag(k) + 5 * matrix(1, k, k)) / 45, matrix(0, k, k2)),
               cbind(integer(k2), matrix(0, k2, k), matrix(0, k2, k), diag(k2) / 9)
               )
             },
           stop("Model type not valid")
    )
  }

  if ("Id" %in% criteria) {
    k <- msopt$nfacts
    k2 <- k * (k - 1) / 2
    switch(model,
           "main" = {
             msopt$M0 <- rbind(
               cbind(1, t(integer(k))),
               cbind(integer(k), diag(k) / 3)
               )
             },
           "interaction" = {
             msopt$M0 <- rbind(
               cbind(1, t(integer(k)), t(integer(k2))),
               cbind(integer(k), diag(k) / 3, matrix(0, k, k2)),
               cbind(integer(k2), matrix(0, k2, k), diag(k2) / 9)
               )
             },
           "quadratic" = {
             msopt$M0 <- rbind(
               cbind(1, t(integer(k)), t(rep(1, k)) / 3, t(integer(k2))),
               cbind(integer(k), diag(k) / 3, matrix(0, k, k), matrix(0, k, k2)),
               cbind(rep(1, k) / 3, matrix(0, k, k),
                     (4 * diag(k) + 5 * matrix(1, k, k)) / 45, matrix(0, k, k2)),
               cbind(integer(k2), matrix(0, k2, k), matrix(0, k2, k), diag(k2) / 9)
               )
             },
           stop("Model type not valid")
    )
    msopt$M0[, 1] <- 0
    msopt$M0[1, ] <- 0
  }

  if ("As" %in% criteria) {
    switch(model,
           "main" = {
             w <- t(rep(1, msopt$nfacts))
             },
           "interaction" = {
             w <- t(rep(1, msopt$nfacts + msopt$nfacts * (msopt$nfacts - 1) / 2))
             },
           "quadratic" = {
             w <- c(
               rep(1, msopt$nfacts),
               rep(1, msopt$nfacts) / 4,
               rep(1, msopt$nfacts * (msopt$nfacts - 1) / 2)
               )
             },
           stop("Model type not valid")
    )
    a <- length(w / sum(w))
    msopt$W <- c(w / sum(w)) * diag(a)
  }
  class(msopt) <- c("MSOpt", "list")
  return(msopt)
}



#### print.MSOpt ####
#' @export
#'
print.MSOpt <- function(x, ...) {
  cat("MSOpt object of", length(x), "elements defining the experimental structure.")
  return(invisible(NULL))
}

#### summary.MSOpt ####
#' @export
#'
summary.MSOpt <- function(object, ...) {
  msopt <- object
  cat("- Number of experimental factors: ", msopt$nfacts, "\n")
  cat("- Number of strata: ", msopt$nstrat, "\n")
  cat("- Total number of runs: ", msopt$runs, "\n")
  cat("- Etas: ", unlist(msopt$etas), "\n" )
  cat("- Levels per experimental factor: ", msopt$levs, "\n" )
  cat("- Model type: ", msopt$model, "\n" )
  cat("- Criteria: ", msopt$crit, "\n" )
  cat("\n")
  cat("For further information see ?MSOpt")
  return(invisible(NULL))
}





#### colprod ####

colprod <- function(X) {

  if (is.matrix(X)) {
    n_row <- nrow(X)
    n_col <- ncol(X)
    out <- matrix(NA, n_row, n_col * (n_col - 1) / 2)

    k <- 1
    for (i in 1:(n_col - 1)) {
      for (j in (i + 1):n_col) {
        out[, k] <- X[, i] * X[, j]
        k <- k + 1
      }
    }
  } else if (is.vector(X) & length(X) > 1) {

    n <- length(X)
    out <- c()

    k <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        out[k] <- X[i] * X[j]
        k <- k + 1
      }
    }
  } else {
    out = NULL
  }
  return(out)
}

#### Score ####
#' Criteria values for design matrix
#'
#' The \code{Score} function returns the optimization criteria values for a
#' given \code{\link[multiDoE]{MSOpt}} list and design matrix.
#'
#' @param msopt A list as returned by the function \link[multiDoE]{MSOpt}.
#' @param settings The design matrix for which criteria scores have to be calculated.
#'
#' @return The data frame of criteria and scores.
#' @export
Score <- function(msopt, settings) {

  switch(msopt$model,
         "main" = {
           X <- cbind(rep(1, msopt$runs), settings)
           },
         "interaction" = {
           X <- cbind(rep(1, msopt$runs), settings, colprod(settings))
           },
         "quadratic" = {
           X <- cbind(rep(1, msopt$runs), settings, settings ^ 2, colprod(settings))
           },
         )

  B <- t(X) %*% msopt$Vinv %*% X
  determ <- det(B)
  scores <- as.vector(matrix(Inf, length(msopt$crit)))


  if (rcond(B) > 1e-5 & determ > 0) {

    ind <- msopt$crit == "D"             # true/false
    if (any(ind)) {
       scores[ind] <- 1 / determ ^ (1 / dim(X)[2])
    }

    if (any(c("I", "Id", "Ds", "A", "As") %in% msopt$crit)) {
      Binv <- solve(B)
    }

    ind <- msopt$crit == "I"
    if (any(ind)) {
      scores[ind] <- sum(diag(Binv %*% msopt$M))
    }

    ind <- msopt$crit == "Id"
    if (any(ind)) {
      scores[ind] <- sum(diag(Binv %*% msopt$M0))
    }

    ind <- msopt$crit == "A"
    if (any(ind)) {
      scores[ind] <- sum(diag(Binv)) / dim(X)[2]
    }

    ind <- msopt$crit == "Ds"
    if (any(ind)) {
      rws <- dim(Binv)[1]
      cls <- dim(Binv)[2]
      scores[ind] <- (det(Binv[2:rws, 2:cls])) ^ ( 1 / (dim(X)[2] - 1))
    }

    ind <- msopt$crit == "As"
    if (any(ind)) {
      rws <- dim(Binv)[1]
      cls <- dim(Binv)[2]
      scores[ind] <- sum(diag(msopt$W %*% Binv[2:rws, 2:cls]))
    }
  }
  return(data.frame("criteria" = msopt$crit, "score" = scores))
}


