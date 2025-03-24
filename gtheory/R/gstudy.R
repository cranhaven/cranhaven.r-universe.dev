#' Conduct a Univariate or Multivariate Generalizability (G) Study
#'
#' \code{gstudy} estimates variance components attributable to objects of measurement (e.g., persons) and facets (e.g., items and raters), as well as unexplained variation.
#'
#' @param data a data frame in long format with a column for item scores and columns for sources of variance
#' @param formula a formula specifying the model to be estimated by \code{\link{lmer}}
#' @param colname.strata an optional string that specifies the name of the column containing strata (if conducting a multivariate G study)
#' @param colname.objects an optional string that specifies the name of the column containing objects of measurement for calculating observed-score covariance (if conducting a multivariate G study)
#' @param keep.mer a logical indicating whether or not to store the output from \code{\link{lmer}} as an attribute of the variance components data frame
#' @param ... additional arguments to be passed to \code{\link{lmer}}
#' @references Brennan, R. L. (2001). \emph{Generalizability theory}. New York: Springer.
#' @references Rajaratnam, N., Cronbach, L. J., & Gleser, G. C. (1965). Generalizability of stratified-parallel tests. \emph{Psychometrika}, \emph{30}(1), 39-56.
#' @return an object of class "\code{gstudy}" that lists variance components of class "\code{components}".  It will also list observed-score variance and covariance between strata if you specify the names of the columns identifying strata and objects of measurement.
#' @export
#' @examples
#' #Conduct a univariate G study.
#' #Compare to results on page 116 of Brennan (2001).
#' data(Brennan.3.2)
#' formula.Brennan.3.2 <- "Score ~ (1 | Person) + (1 | Task) + 
#'   (1 | Rater:Task) + (1 | Person:Task)"
#' gstudy(data = Brennan.3.2, formula = formula.Brennan.3.2)
#' 
#' #Conduct a multivariate G study.
#' #Compare to results on page 270 of Brennan (2001).
#' data(Rajaratnam.2)
#' formula.Rajaratnam.2 <- "Score ~ (1 | Person) + (1 | Item)"
#' gstudy(data = Rajaratnam.2, formula = formula.Rajaratnam.2, colname.strata = "Subtest", 
#'   colname.objects = "Person")
gstudy <- function(data, ...) {
  UseMethod("gstudy", data)
}

#' @describeIn gstudy G study of a \code{data.frame} object
#' @method gstudy data.frame
#' @export
gstudy.data.frame <- function(data, formula, colname.strata = NULL, colname.objects = NULL, 
  keep.mer = F, ...) {
  if(is.null(colname.strata) == F & is.null(colname.objects)) {
    warning("Observed-score covariance was not calculated.\nWhich column contains objects of measurement?", call. = F)
  }
  if(class(formula) != "formula") formula <- as.formula(formula)
  if(is.null(colname.strata) == F) {
    strata <- unique(data[, colname.strata])
    data <- sapply(
      X = strata, 
      FUN = function(name.stratum) {
        cases.keep <- is.na(data[, colname.strata]) == F & 
          data[, colname.strata] == name.stratum
        data.stratum <- data[cases.keep, ]
        class(data.stratum) <- c("univariate", class(data.stratum))
        data.stratum
      }, 
      simplify = F
    )
    names(data) <- strata
    class(data) <- c("multivariate", class(data))
  } else {
    class(data) <- c("univariate", class(data))
  }
  gstudy.out <- gstudy(
    data = data, 
    formula = formula, 
    colname.strata = colname.strata, 
    colname.objects = colname.objects, 
    keep.mer = keep.mer
  )
  gstudy.out
}

#' @describeIn gstudy G study of a \code{univariate} object
#' @method gstudy univariate
#' @export
gstudy.univariate <- function(data, formula, colname.strata = NULL, colname.objects = NULL, 
  keep.mer = F, ...) {
  lmer.out <- lmer(data = data, formula = formula, ...)
  gstudy.out <- as.data.frame(VarCorr(lmer.out))
  gstudy.out <- gstudy.out[, c("grp", "vcov")]
  names(gstudy.out) <- c("source", "var")
  gstudy.out$percent <- round(gstudy.out$var / sum(gstudy.out$var) * 100, 1)
  gstudy.out$n <- 1
  class(gstudy.out) <- c("components", class(gstudy.out))
  if(keep.mer) attributes(gstudy.out)$mer <- lmer.out
  gstudy.out <- list("components" = gstudy.out)
  if(is.null(colname.strata) == F & is.null(colname.objects) == F) {
    x <- model.matrix(
      object = as.formula(paste("~ - 1 +", colname.objects)), 
      data = data
    )
    colname.scores <- all.vars(formula)[1]
    y <- data[, colname.scores]
    gstudy.out$scores.obs <- solve(t(x) %*% x) %*% t(x) %*% y
  }
  if(is.null(colname.strata)) {
    class(gstudy.out) <- c("gstudy", class(gstudy.out))
  }
  gstudy.out
}

#' @describeIn gstudy G study of a \code{multivariate} object
#' @method gstudy multivariate
#' @export
gstudy.multivariate <- function(data, formula, colname.strata = NULL, colname.objects = NULL, 
  keep.mer = F, ...) {
  gstudy.out <- list()
  for(name.stratum in names(data)) {
    gstudy.out[[name.stratum]] <- gstudy(
      data = data[[name.stratum]], 
      formula = formula, 
      colname.strata = colname.strata, 
      colname.objects = colname.objects, 
      keep.mer = keep.mer
    )
    if(is.null(colname.objects) == F) {
      if(name.stratum == names(data)[1]) {
        scores.obs <- gstudy.out[[name.stratum]]$scores.obs
        colnames(scores.obs) <- name.stratum
      } else {
        scores.obs.temp <- gstudy.out[[name.stratum]]$scores.obs
        colnames(scores.obs.temp) <- name.stratum
        scores.obs <- merge(scores.obs, scores.obs.temp, by = 0, all = T, sort = F)
        rownames(scores.obs) <- scores.obs$Row.names
        vars.keep <- names(scores.obs) != "Row.names"
        scores.obs <- scores.obs[, vars.keep]
      }
      gstudy.out[[name.stratum]]$scores.obs <- NULL
    }
  }
  gstudy.out <- list("within" = gstudy.out)
  if(is.null(colname.objects) == F) {
    scores.obs <- as.matrix(scores.obs)
    #gstudy.out$between$scores.obs <- scores.obs
    var.obs <- var(scores.obs)
    attributes(var.obs)$colname.objects <- colname.objects
    gstudy.out$between$var.obs <- var.obs
  }
  class(gstudy.out) <- c("gstudy", class(gstudy.out))
  gstudy.out
}