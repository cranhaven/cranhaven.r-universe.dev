#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%$%"
#' @rawNamespace import(rlang, except=c("prepend", "flatten_raw"))
#' @rawNamespace import(tibble, except="has_name")
#' @import dplyr
#' @rawNamespace import(purrr, except=c("flatten", "flatten_int", "flatten_chr", "flatten_dbl", "flatten_lgl", "flatten_raw", "as_function", "splice", "invoke", "%||%", "%@%", "list_along", "rep_along", "modify"))
#' @import tidyr
#' @import stringr
#' @import forcats
#' @import survival
#' @import ggplot2
#' @import survminer
#' @import tidytidbits
#' @importFrom grDevices dev.off
#' @importFrom graphics strwidth
#' @importFrom stats as.formula formula na.omit qnorm
#' @importFrom utils capture.output write.csv
#' @importFrom cowplot ggdraw draw_label
#' @importFrom stats pchisq
NULL

## quiets concerns of R CMD check regarding the .'s that appear in pipelines,
## variables in vars() calls, from %$% overscoping and those shared via the local_variables() mechanism
if(getRversion() >= "2.15.1") utils::globalVariables(c(".",
                                                       "Lower.CI",
                                                       "Lower_CI",
                                                       "Inv_Lower_CI",
                                                       "HR",
                                                       "Upper.CI",
                                                       "Upper_CI",
                                                       "Inv_Upper_CI",
                                                       "Inv_HR",
                                                       "p",
                                                       "breakByYears",
                                                       "breakByHalfYear",
                                                       "breakByQuarterYear",
                                                       "scaleByMonths",
                                                       "scaleByYears",
                                                       "label",
                                                       "factor.name",
                                                       "factor.value",
                                                       "factor.id",
                                                       "endpoint",
                                                       "subgroup_n",
                                                       "breakAfter",
                                                       "ordered_index",
                                                       "breaks",
                                                       "endpointLabel",
                                                       "factorLabel",
                                                       "x",
                                                       "n_string",
                                                       "HR_string",
                                                       "CI_string",
                                                       "p_string",
                                                       "subgroup_n_string",
                                                       "Likelihood ratio test p",
                                                       "Score (logrank) test p",
                                                       "Wald test p",
                                                       "lower",
                                                       "n.event",
                                                       "n.risk",
                                                       "rate",
                                                       "stratum",
                                                       "stratum_value",
                                                       "surv",
                                                       "time",
                                                       "upper"))
