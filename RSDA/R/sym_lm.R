#' CM and CRM Linear regression model.
#' @name sym.lm
#' @aliases sym.lm
#' @author Oldemar Rodriguez Rojas
#' @description To execute the Center Method (CR) and Center and Range Method (CRM) to Linear regression.
#' @usage sym.lm(formula, sym.data, method = c('cm', 'crm'))
#' @param formula An object of class 'formula' (or one that can be coerced to that class): a symbolic description
#' of the model to be fitted.
#' @param sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param method 'cm' to Center Method and 'crm' to Center and Range Method.
#' @details
#' Models for lm are specified symbolically. A typical model has the form response ~
#' terms where response is the (numeric) response vector and terms is a series of
#' terms which specifies a linear predictor for response. A terms specification of
#' the form first + second indicates all the terms in first together with all the
#' terms in second with duplicates removed. A specification of the form first:second indicates
#' the set of terms obtained by taking the interactions of all terms in first with all terms
#' in second. The specification first*second indicates the cross of first and second.
#' This is the same as first + second + first:second.
#' @return sym.lm returns an object of class 'lm' or for multiple responses of class c('mlm', 'lm')
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#'
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' res.cm <- sym.lm(lpsa ~ ., sym.data = int_prost_train, method = "cm")
#' res.cm
#' @keywords Symbolic lm
#' @importFrom  stats lm
#' @export
#'
sym.lm <- function(formula, sym.data, method = c("cm", "crm")) {
  all_interval <- all(sapply(sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  method <- match.arg(method)
  if (method == "cm") {
    centers <- interval.centers(sym.data)
    out <- stats::lm(formula, data = centers)
    class(out) <- c("symbolic_lm_cm", class(out))
    return(out)
  }
  if (method == "crm") {
    # Center Model
    centers <- interval.centers(sym.data)
    centers <- as.data.frame(centers)
    modelc <- stats::lm(formula, data = centers)
    # Range Model
    range <- interval.ranges(sym.data)
    range <- as.data.frame(range)
    modelr <- stats::lm(formula, data = range)
    out <- list(CenterModel = modelc, RangeModel = modelr)
    class(out) <- c("symbolic_lm_crm", class(out))
    return(out)
  }
}
