#' Conduct a Univariate or Multivariate Decision (D) Study
#'
#' \code{dstudy} calculates generalizability and dependability coefficients from variance components.  It also provides standards errors of measurement and estimation.
#'
#' @param x an object of class \code{\link{gstudy}}, \code{dstudy}, or \code{components}
#' @param colname.objects a string naming the source of variation for the object of measurement
#' @param data an optional data frame in long format with a column for item scores and columns for sources of variance
#' @param colname.scores an optional string that specifies the name of the column containing scores
#' @param colname.strata an optional string that specifies the name of the column containing strata (if conducting a multivariate G study)
#' @param weights an optional numeric vector containg one weight per stratum for composite scoring (if conducting a multivariate G study); defaults to equal weights
#' @param ... ignored
#' @references Brennan, R. L. (2001). \emph{Generalizability theory}. New York: Springer.
#' @references Rajaratnam, N., Cronbach, L. J., & Gleser, G. C. (1965). Generalizability of stratified-parallel tests. \emph{Psychometrika}, \emph{30}(1), 39-56.
#' @details A typical decision (D) study starts with updating variance components from the generalizaiblity (G) with the number of facet levels from the D-study data.  D-study data may or may not be the same data collected for the G study.  \code{dstudy} will update the variance components when you supply decision data and specify the name of the column identifying objects of measurement.  If you do not supply data or specify the score column, then \code{dstudy} will use the G-study variance components (i.e., with all n = 1) and return what is commonly known as intraclass correlation (i.e., the generalizability and dependability of a single observation).  If your D-study data are unbalanced (i.e., if the number of facet levels vary from one object of measurement to another), then \code{dstudy} will return an overall \code{components} object based on the median number of levels of the main facet effects and will store object-specific variance components as attributes (i.e., to facilitate scoring).
#' @return an object of class "\code{dstudy}" that lists the variance components and corresponding measures of signal and noise (i.e., generalizability and dependability coefficients, universe score variance, relative and absolute error variance, and relative and absolute standard errors of measurement and estimation).
#' @export
#' @examples
#' #A univariate D study.
#' #Compare to results on page 116 of Brennan (2001).
#' data(Brennan.3.2)
#' formula.Brennan.3.2 <- "Score ~ (1 | Person) + (1 | Task) + (1 | Rater:Task) + 
#'   (1 | Person:Task)"
#' gstudy.out <- gstudy(data = Brennan.3.2, formula = formula.Brennan.3.2)
#' dstudy(gstudy.out, colname.objects = "Person", data = Brennan.3.2, colname.scores = "Score")
#' 
#' #A multivariate D study.
#' #Compare to results on pages 270-272 of Brennan (2001).
#' data(Rajaratnam.2)
#' formula.Rajaratnam.2 <- "Score ~ (1 | Person) + (1 | Item)"
#' gstudy.out <- gstudy(data = Rajaratnam.2, formula = formula.Rajaratnam.2, 
#'   colname.strata = "Subtest", colname.objects = "Person")
#' dstudy(gstudy.out, colname.objects = "Person", data = Rajaratnam.2, colname.scores = "Score", 
#'   colname.strata = "Subtest", weights = c(0.25, 0.5, 0.25))
dstudy <- function (x, ...) {
  UseMethod("dstudy", x)
}

#' @describeIn dstudy D study of a \code{components} object
#' @method dstudy components
#' @export
dstudy.components <- function(x, colname.objects, ...) {
  var.universe <- x$var[x$source == colname.objects]
  sources.error <- x$source != colname.objects & (
    grepl(pattern = colname.objects, x = x$source) | 
    x$source == "Residual"
  )
  var.error.rel <- sum(x[sources.error, "var"])
  sem.rel <- sqrt(var.error.rel)
  generalizability <- var.universe / (var.universe + var.error.rel)
  see.rel <- sem.rel * sqrt(generalizability)
  sources.error <- x$source != colname.objects
  var.error.abs <- sum(x[sources.error, "var"])
  sem.abs <- sqrt(var.error.abs)
  dependability <- var.universe / (var.universe + var.error.abs)
  see.abs <- sem.abs * sqrt(dependability)
  dstudy.out <- list()
  dstudy.out$components <- x
  dstudy.out <- c(
    dstudy.out, as.list(
      data.frame(
        var.universe, 
        generalizability, 
        var.error.rel, 
        sem.rel, 
        see.rel, 
        dependability, 
        var.error.abs, 
        sem.abs, 
        see.abs
      )
    )
  )
  dstudy.out
}

#' @describeIn dstudy D study of \code{\link{gstudy}} object
#' @method dstudy gstudy
#' @export
dstudy.gstudy <- function(x, colname.objects, data = NULL, colname.scores = NULL, 
  colname.strata = NULL, weights = NULL, ...) {
  if(is.null(data) | is.null(colname.scores)) {
    warning("Did you intend to calculate intraclass correlation for a single observation (i.e., n = 1 for all sources of variation)?\nYou must specify the data and which column contains scores if you wish to conduct a D study based on observed scores.")
    dstudy.out <- x
    class(dstudy.out) <- ifelse(class(dstudy.out) == "gstudy", "dstudy", class(dstudy.out))
  } else {
    dstudy.out <- update(x, colname.objects = colname.objects, data = data, 
      colname.scores = colname.scores, colname.strata = colname.strata)
  }
  dstudy.out <- dstudy(dstudy.out, colname.objects = colname.objects, weights = weights)
  dstudy.out
}

#' @describeIn dstudy D study of a \code{dstudy} object
#' @method dstudy dstudy
#' @export
dstudy.dstudy <- function(x, colname.objects, weights = NULL, ...) {
  dstudy.out <- x
  if("within" %in% names(x)) {
    dstudy.out$within <- sapply(
      X = dstudy.out$within, 
      FUN = function(stratum) {
        dstudy(
          stratum$components, 
          colname.objects = colname.objects
        )
      }, 
      simplify = F
    )
    dstudy.out$between$var.universe <- dstudy.out$between$var.obs
    diag(dstudy.out$between$var.universe) <- sapply(
      X = dstudy.out$within, 
      FUN = function(stratum) {
        stratum[["var.universe"]]
      }
    )
    strata <- names(dstudy.out$within)
    for(name in c(
      "generalizability", 
      "var.error.rel", 
      "sem.rel", 
      "see.rel", 
      "dependability", 
      "var.error.abs", 
      "sem.abs", 
      "see.abs")
    ) {
      dstudy.out$between[[name]] <- diag(
        sapply(
          X = dstudy.out$within, 
          FUN = function(stratum) {
            stratum[[name]]
          }
        )
      )
      dimnames(dstudy.out$between[[name]]) <- list(strata, strata)


#      if(name == "see.rel"){
#        dstudy.out$between[["scores.universe.rel"]] <- t(colMeans(dstudy.out$between$scores.obs) + 
#          dstudy.out$between$generalizability %*% 
#          (t(dstudy.out$between$scores.obs) - colMeans(dstudy.out$between$scores.obs)))
#        dstudy.out$between[["betas.mv.rel"]] <- dstudy.out$between$var.universe %*% 
#          solve((dstudy.out$between$var.universe + dstudy.out$between$var.error.rel))
#        dstudy.out$between[["scores.universe.mv.rel"]] <- t(
#          colMeans(dstudy.out$between$scores.obs) + 
#            dstudy.out$between$betas.mv.rel %*% 
#            (t(dstudy.out$between$scores.obs) - colMeans(dstudy.out$between$scores.obs))
#        )
#        dstudy.out$between[["r2.rel"]] <- diag(
#          var(dstudy.out$between[["scores.universe.mv.rel"]])
#        ) / diag(dstudy.out$between$var.universe)
#        dstudy.out$between[["see.mv.rel"]] <- sqrt(diag(dstudy.out$between$var.universe)) * 
#          sqrt(1 - dstudy.out$between[["r2.rel"]])
#      }
#      if(name == "see.abs"){
#        dstudy.out$between[["scores.universe.abs"]] <- t(colMeans(dstudy.out$between$scores.obs) + 
#          dstudy.out$between$dependability %*% 
#          (t(dstudy.out$between$scores.obs) - colMeans(dstudy.out$between$scores.obs)))
#        dstudy.out$between[["betas.mv.abs"]] <- dstudy.out$between$var.universe %*% 
#          solve((dstudy.out$between$var.universe + dstudy.out$between$var.error.abs))
#        dstudy.out$between[["scores.universe.mv.abs"]] <- t(
#          colMeans(dstudy.out$between$scores.obs) + 
#          dstudy.out$between$betas.mv.abs %*% 
#          (t(dstudy.out$between$scores.obs) - colMeans(dstudy.out$between$scores.obs))
#        )
#        dstudy.out$between[["r2.abs"]] <- diag(
#          var(dstudy.out$between[["scores.universe.mv.abs"]])
#        ) / diag(dstudy.out$between$var.universe)
#        dstudy.out$between[["see.mv.abs"]] <- sqrt(diag(dstudy.out$between$var.universe)) * 
#          sqrt(1 - dstudy.out$between[["r2.abs"]])
#      }


    }
    dstudy.out$within <- sapply(
      X = dstudy.out$within, 
      FUN = function(stratum) {
        stratum["components"]
      }, 
      simplify = F
    )
    if(is.null(weights)) {
      weights <- rep(1, length(strata)) / length(strata)
    }
    dstudy.out[["composite"]] <- list()
    dstudy.out$composite[["var.universe"]] <- t(weights) %*% dstudy.out$between$var.universe %*% 
      weights
    dstudy.out$composite[["generalizability"]] <- t(weights) %*% 
      dstudy.out$between$var.universe %*% weights / t(weights) %*% 
      (dstudy.out$between$var.universe + dstudy.out$between$var.error.rel) %*% weights
    dstudy.out$composite[["var.error.rel"]] <- sum(
      weights ^ 2 * t(
        diag((dstudy.out$between$var.universe + dstudy.out$between$var.error.rel)) * 
        (1 - diag(dstudy.out$between$var.universe / 
          (dstudy.out$between$var.universe + dstudy.out$between$var.error.rel)))
      )
    )
    dstudy.out$composite[["sem.rel"]] <- sqrt(dstudy.out$composite$var.error.rel)
    dstudy.out$composite[["see.rel"]] <- dstudy.out$composite$sem.rel * 
      sqrt(dstudy.out$composite$generalizability)
    dstudy.out$composite[["dependability"]] <- t(weights) %*% dstudy.out$between$var.universe %*% 
      weights / t(weights) %*% (dstudy.out$between$var.universe + 
      dstudy.out$between$var.error.abs) %*% weights
    dstudy.out$composite[["var.error.abs"]] <- sum(
      weights ^ 2 * t(
        diag((dstudy.out$between$var.universe + dstudy.out$between$var.error.abs)) * 
        (1 - diag(dstudy.out$between$var.universe / 
          (dstudy.out$between$var.universe + dstudy.out$between$var.error.abs)))
      )
    )
    dstudy.out$composite[["sem.abs"]] <- sqrt(dstudy.out$composite$var.error.abs)
    dstudy.out$composite[["see.abs"]] <- dstudy.out$composite$sem.abs * 
      sqrt(dstudy.out$composite$dependability)
    #dstudy.out$composite <- sapply(X = dstudy.out$composite, FUN = as.numeric, simplify = F)
    dstudy.out
  } else {
    dstudy.out <- dstudy(
      dstudy.out$components, 
      colname.objects = colname.objects
    )
    class(dstudy.out) <- c("dstudy", class(dstudy.out))
  }
  dstudy.out
}