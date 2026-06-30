#' Create new mvalpha class object
#' @description
#' Wrapper for creating mvalpha class object.
#' @param mvalpha Multi-valued alpha estimate
#' @param mvDo Observed disagreement
#' @param mvDe Expected disagreement
#' @param bootstrap_mvalpha Bootstrap estimates of mvalpha
#' @param unique_cardinalities Numeric vector of the unique cardinalities observed in the data
#' @param units Names of units
#' @param observers Names of observers
#' @param labels Unique labels used in data
#' @param values Unique values used in data
#' @param values_by_unit Table of values by unit
#' @param dist_CK Distance matrix for label sets C and K
#' @param p_CK Probability matrix for label sets C and K
#' @inheritParams mvalpha
#' @returns an mvalpha object
#'
new_mvalpha <-
  function(mvalpha, type, mvDo, mvDe, bootstrap_mvalpha, unique_cardinalities, units, observers, labels, values, values_by_unit, dist_CK, p_CK, data){
    structure(
      .Data = list(
        mvalpha = mvalpha,
        type = type,
        mvDo = mvDo,
        mvDe = mvDe,
        bootstrap_mvalpha = bootstrap_mvalpha,
        unique_cardinalities = unique_cardinalities,
        units = units,
        observers = observers,
        labels = labels,
        values = values,
        values_by_unit = values_by_unit,
        dist_CK = dist_CK,
        p_CK = p_CK
      ),
      class = "mvalpha"
    )
  }


#' @title Print mvalpha class object
#' @description
#' Print generic
#' @param x mvalpha object
#' @param ... additional parameters
#' @method print mvalpha
#' @returns invisibly returns the alpha estimate of an mvalpha object
#' @export

print.mvalpha <-
  function(x, ...){
    cat(paste0("mvalpha (", x$type, "): \n", round(x$mvalpha, 4)))
  }




