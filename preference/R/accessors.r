
#' @title Preference trial parameter accessors
#' @description Accessor function have been created to get the sample size 
#' (sample_size), power (power), effect size (effect_size), arm proportion
#' (proportion), significance (significance), and trial variance estimates
#' (sigma2) for a set of preference trials.
#'
#' Note that these methods are preferred over accessing the underlying
#' data frame directly since the structure is slightly non-standard (some
#' columns are lists) and some values, like power, are not stored directly.
#' @param x the set of preference trials.
#' @docType methods
#' @rdname pt-accessors
#' @aliases sample_size sample_size.preference.trial
#' @aliases power power.preference.trial
#' @aliases effect_size effect_size.preference.trial
#' @aliases proportion proportion.preference.trial
#' @aliases significance significance.preference.trial
#' @aliases sigma2 sigma2.preference.trial
#' @examples
#' # Create a set of trials with a sequence of preference effects.
#' trials <- preference.trial(pref_ss=100, pref_effect=seq(0.1, 2, by=0.5), 
#'                            selection_ss=100, selection_effect=1, 
#'                            treatment_ss=100, treatment_effect=1, sigma2=1, 
#'                            pref_prop=0.6)
#' 
#' # the sample sizes
#' sample_size(trials)
#'
#' # the powers
#' power(trials)
#'
#' # the effect sizes
#' effect_size(trials)
#'
#' # the arm proportions
#' proportion(trials)
#'
#' # the significance
#' significance(trials)
#'
#' # the variance estimates
#' sigma2(trials)
#'
#' @export
sample_size <- function(x) {
  UseMethod("sample_size")
}

#' @rdname pt-accessors
#' @export
sample_size.preference.trial <- function(x) {
  ret <- x[,c("pref_ss", "selection_ss", "treatment_ss"), drop=FALSE]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

#' @rdname pt-accessors
#' @export
power <- function(x) {
  UseMethod("power")
}

#' @rdname pt-accessors
#' @export
power.preference.trial <- function(x) {
  ret <- x[,c("pref_power", "selection_power", "treatment_power")]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

#' @rdname pt-accessors
#' @export
effect_size <- function(x) {
  UseMethod("effect_size")
}

#' @rdname pt-accessors
#' @export
effect_size.preference.trial <- function(x) {
  ret <- x[, c("pref_effect", "selection_effect", "treatment_effect"), 
           drop=FALSE]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

#' @rdname pt-accessors
#' @export
proportion <- function(x) {
  UseMethod("proportion")
}

#' @rdname pt-accessors
#' @export
proportion.preference.trial <- function(x) {
  ret <- x[,c("pref_prop", "choice_prop", "stratum_prop"), drop=FALSE]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

#' @rdname pt-accessors
#' @export
significance <- function(x) {
  UseMethod("significance")
}

#' @rdname pt-accessors
#' @export
significance.preference.trial <- function(x) {
  ret <- x[, c("alpha")]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret
}

#' @rdname pt-accessors
#' @export
sigma2 <- function(x) {
  UseMethod("sigma2")
}

#' @rdname pt-accessors
#' @export
sigma2.preference.trial <- function(x) {
  ret <- x[, c("sigma2")]
  class(ret) <- setdiff(class(ret), "preference.trial")
  ret

}
