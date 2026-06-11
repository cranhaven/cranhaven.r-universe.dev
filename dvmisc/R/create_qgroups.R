#' Create Quantile Groups
#' 
#' Combines \code{\link[stats]{quantile}} and \code{\link[base]{cut}} into a 
#' single function, with strata-specific quantiles possible. For example, you 
#' could create sex-specific height tertiles with 
#' \code{create_qgroups(height, groups = 3, strata = sex)}. Compatible with 
#' \strong{dplyr} functions like \code{\link[dplyr]{mutate}} and 
#' \code{\link[dplyr:mutate]{transmute}}.
#' 
#' @param x Numeric vector.
#' @param groups Numeric value, e.g. 3 for tertiles, 4 for quartiles, etc.
#' @param probs Numeric vector.
#' @param strata Factor specifying subgroups to calculate quantiles within. For 
#' multivariable subgroups, you can use \code{\link{interaction}}.
#' @param quantile_list Arguments to pass to \code{\link[stats]{quantile}}.
#' @param cut_list Arguments to pass to \code{\link{cut}}.
#' 
#' @return Factor variable.
#' 
#' @examples 
#' # In mtcars dataset, create tertiles for mpg
#' mtcars$mpg_tertiles <- create_qgroups(mtcars$mpg, groups = 3)
#' table(mtcars$mpg_tertiles)
#' 
#' # Define tertile cutpoints separately for 4-, 6-, and 8-cylinder vehicles
#' mtcars$mpg_tertiles <- create_qgroups(mtcars$mpg, groups = 3, strata = mtcars$cyl)
#' table(mtcars$mpg_tertiles)
#' 
#' # Works with dplyr functions like mutate
#' mtcars <- mtcars %>% 
#'   dplyr::mutate(mpg_tertiles = create_qgroups(mpg, groups = 3, strata = cyl))
#' table(mtcars$mpg_tertiles)
#' 
#' # Can embed in lm, glm, etc.
#' summary(lm(mpg ~ create_qgroups(wt), data = mtcars))
#' 
#' @export
# x <- mtcars$mpg
# groups <- 4
# strata <- as.factor(mtcars$cyl)
# quantile_list <- list(na.rm = TRUE)
# cut_list <- list(include.lowest = TRUE)
# create_qgroups(mtcars$mpg)
# create_qgroups(mtcars$mpg, strata = mtcars$cyl)
# x <- rnorm(1000)
# groups <- create_qgroups(x, 5)
# table(groups)
create_qgroups <- function(x, 
                           groups = 4, 
                           probs = seq(1/groups, 1 - 1/groups, 1/groups), 
                           strata = NULL, 
                           quantile_list = list(na.rm = TRUE), 
                           cut_list = list(include.lowest = TRUE)) {
  
  if (is.null(strata)) {
    cutpoints <- do.call(quantile, c(list(x = x, probs = probs), quantile_list))
    return(do.call(cut, c(list(x = x, breaks = c(-Inf, cutpoints, Inf)), 
                          cut_list)))
  }
  
  if (class(strata) != "factor") {
    strata <- as.factor(strata)
  }
  if (is.null(cut_list$labels)) {
    cut_list$labels <- paste("Q", 1: (length(probs) + 1), sep = "")
  }
  y <- rep(NA, length(x))
  for (ii in levels(strata)) {
    locs <- which(strata == ii)
    x_ii <- x[locs]
    cutpoints <- do.call(quantile, c(list(x = x_ii, probs = probs), quantile_list))
    y[locs] <- as.vector(do.call(cut, c(list(x = x_ii, breaks = c(-Inf, cutpoints, Inf)), 
                                             cut_list)))
  }
  as.factor(y)
  
}
