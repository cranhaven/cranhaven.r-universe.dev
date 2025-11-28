#' f_baf
#' @keywords internal
#' @export
f_baf <- function(x,beta,var=.0008) -sum(log(exp(-(beta-x)^2/var/2)+exp(-(beta-1+x)^2/var/2)))
