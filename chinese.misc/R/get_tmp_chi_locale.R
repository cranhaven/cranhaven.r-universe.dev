#' Check The Locale Functions are to Assume
#'
#' The locale setting of R is different on different operating systems or different versions of one system.
#' However, some functions in this package try to convert the locale setting of R to a new value.
#' The new value, by default, is 
#' "Chinese (Simplified)_China.936" in Windows, and "zh_CN.UTF-8" in other systems. But
#' users can modify this by \code{options(tmp_chi_locale = "...")} and then check this by 
#' \code{get_tmp_chi_locale( )}. Note: if this value is \code{NULL} or \code{NA}, it means no
#' locale modification will be done by functions in this package. If this value is "auto", it will be 
#' automatically converted to the default values.
#'
#' @export
get_tmp_chi_locale <- function(){
	return(getOption("tmp_chi_locale"))
}
