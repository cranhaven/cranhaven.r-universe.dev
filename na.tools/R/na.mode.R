#' @details 
#' 
#' `na.mode` replaces all `NA` with the most frequently occuring value. In 
#' the event of ties, the value encounter first in `.x` is used.
#' 
#' `na.most_freq` is an alias for `na.mode`.
#' 
#' @examples 
#'   na.mode( c(1,1,NA,4) )
#'   na.mode( c(1,1,4,4,NA) ) 
#'
#' @rdname impute-commutative
#' @md
#' @export

na.mode <- function(.x, ... ) 
  na.replace(.x, .na=function(x) most_freq(x, na.omit, ...) )

#' @rdname impute-commutative
#' @export
na.most_freq <- na.mode 
