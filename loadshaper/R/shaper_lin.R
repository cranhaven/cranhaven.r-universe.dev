
#'
#' @title Linear Method for Matching Peak and Load Factor
#'
#'
#'
#'
#' @param x A numeric array, representing reference load shape.
#' All values must be strictly positive containing no \code{NA}(s).
#' The length of \code{x}
#' must be greater > 167.
#'
#'
#'
#' @param target_max Target peak value of resultant load shape, must
#' be > 0.
#'
#' @param target_lf Target load factor of resultant load shape, must be
#' numeric in between 0 and 1 (exclusive).
#'
#'
#'
#'
#' @description \code{lslin} applies linear method to a reference load
#' shape to match the peak and load factor to target values. See "Details"
#' for the algorithm.
#'
#'
#'
#'
#' @import stats
#'
#'
#'
#' @return A list of class \code{"lslin"}, having following elements:
#' \itemize{
#'
#' \item{\code{df}: A data frame. See "Details".}
#' \item{\code{beta}: Slope of the linearly increasing/decreasing multipliers.
#' See "Details".}
#'
#' \item{\code{max_mult}: Maximum of the multipliers.}
#' \item{\code{min_mult}: Minimum of the multipliers.}
#' \item{\code{base_load_factor}: Load factor of the reference load
#' shape \code{x}.}
#'
#' \item{\code{target_load_factor}: Target load factor.}
#' \item{\code{derived_load_factor}: Load factor of the derived load shape
#' (\code{object$df$y}).}
#' \item{\code{base_max}: Peak value of the base load shape, \code{x}}
#' \item{\code{target_max}: Target peak value of the new load shape.}
#' \item{\code{derived_max}: Peak value of the derived load shape
#'  (\code{object$df$y}) }
#'
#' \item{\code{base_min}: Minimum value of the base load shape, \code{x}}
#' \item{\code{derived_min}: Minimum value of the derived load shape
#'  (\code{object$df$y}) }
#'
#' \item{\code{dec_flag}: A logical flag  stating
#' whether the multipliers resulted in strictly decreasing values.
#' \code{TRUE} indicates the order was not preserved.
#' Only applicable for \code{target_max} > \code{base_max}.
#' See "Details".}
#' \item{\code{lf_flag}: A logical flag indicating if the
#' load factor of the derived shape differs from the target
#' by more than 1\%.
#' }
#' \item{\code{min_pu_flag}: A logical flag indicating existence of
#' negative values in the derived load shape. \code{TRUE} indicates
#' the existence of negative values.
#' Only applicable for \code{target_max} < \code{base_max}.
#' See "Details".}
#'
#'
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @details The algorithm first evaluates the load factor of the
#' reference load shape \code{x}, which is
#' defined by the ratio of average to peak value. If the target load
#' factor is greater than reference level, then all base
#' values are multiplied by a number \code{ > 1}. If the target
#' load factor is less than reference level, then all base values are
#' multiplied by a number \code{ < 1}. The multipliers increase/decrease
#' linearly and are applied to the based values after ordered.
#'
#'
#' If \eqn{x'} is the ordered version of \eqn{x}, then \eqn{x'_{i}}
#' will be multiplied by \eqn{1-(i-1)*\beta}, where \eqn{\beta} is
#' a constant calculated as:
#'
#'
#' \deqn{\beta = \frac{\sum_{i=1}^n x'_{i} - target\ load\ factor }
#' {\sum_{i=1}^n x'_{i}(i-1)}}
#'
#' The load factor of the derived series matches the target.
#' For \eqn{target < base}, \eqn{\beta} is positive and vice-versa.
#'
#' The algorithm attempts hard to match the load factor of the derived
#' load shape to the base load factor. \eqn{\beta} becomes large in
#' magnitude for large difference of base and target load factor.
#' In case \eqn{\beta > 1}, it is possible to get negative
#' multipliers which force the values to be negative. This particular
#' situation can occur when target load factor is significantly smaller
#' than the base load factor.
#'
#' If the target load factor is much bigger than the base
#' load factor, one/both of the followings can occur:
#' \itemize{
#'  \item As a  linearly increasing function is multiplied
#'  by a  decreasing function (\eqn{x'}), it is possible
#'  that the maximum of the product can exceed the maximum value
#'  of the base  (\eqn{x'}), resulting in a different load factor.
#'
#'  \item As a  linearly increasing function is multiplied
#'  by a  decreasing function (\eqn{x'}), it is possible
#'  that the product is not strictly decreasing. The product
#'  array is re-ordered to produce the final values.
#'  }
#'
#'
#'
#' The return object contains a data frame \code{df}, having the
#' following columns:
#' \itemize{
#'
#' \item{\code{x_index}: An index given to the original load
#' shape \code{x}, starting from 1 to \code{length(x)}}.
#'
#' \item{\code{x}: The original array \code{x}, unaltered.}
#'
#' \item{\code{x_rank}: The rank of the data points of the
#' given array \code{x}, from 1 for the peak to
#' \code{length(x)} for the lowest value.}
#'
#' \item{\code{x_ordered}: Sorted \code{x} (largest to smallest).}

#' \item{\code{x_pu}: Per unit \code{x}, derived by diving \code{x}
#' by \code{max(x)}}.
#'
#' \item{\code{x_ordered_pu}: Per unit \code{x}, sorted from largest
#' to smallest.}
#'
#'
#' \item{\code{mult}: Derived multipliers, would be applied to
#' sorted per unit \code{x}.}
#'
#'
#' \item{\code{y_ordered_pu}: Product of per unit sorted \code{x}
#' and \code{mult}.}
#'
#'
#' \item{\code{y_ordered_pu2}: \code{y_ordered_pu}, sorted again,
#' in case \code{y_ordered_pu} does not become decreasing.}
#'
#'
#' \item{\code{y_pu}: Resultant load shape in per unit. This is
#' derived by re-ordering \code{y_ordered_pu2} with respect to their
#' original rank.}
#'
#'
#' \item{\code{y}: Resultant load shape. This is derived by
#' multiplying \code{y_pu} by \code{taget_max} / \code{base_max}
#' }
#'
#'
#'
#' }
#'
#' @seealso \code{\link{lslog}},
#' \code{\link{print.lslin}},
#' \code{\link{summary.lslin}},
#' \code{\link{plot.lslin}},
#' \code{\link{lscore}}
#'
#'
#'
#'
#' @examples
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' plot(loads, type = "l")
#' linear_loadshape <- lslin(loads, target_lf = 0.50)
#' summary(linear_loadshape)
#' #-------------------------------------
#' loads2 <- ercot[ercot$Year == 2020, ]$ERCOT
#' plot(loads2, type = "l")
#' linear_loadshape2 <- lslin(loads2, target_lf = 0.7)
#' summary(linear_loadshape2)#'
#' #-------------------------------------
#' loads3 <- ercot[ercot$Year == 2020, ]$ERCOT
#' plot(loads3, type = "l")
#' linear_loadshape3 <- lslin(loads3, target_lf = 0.95)
#' summary(linear_loadshape3)
#' #-------------------------------------
#' loads4 <- ercot[ercot$Year == 2020, ]$SCENT
#' plot(loads3, type = "l")
#' linear_loadshape4 <- lslin(loads4, target_lf = 0.3)
#' summary(linear_loadshape4)
#'
#'
#'
#' @export
lslin <- function(x, target_max = 10000,  target_lf = 0.7)

{

  # x
  ####################
  if(any(is.na(x))){
    stop("x contains NA(s)")
  }

  if(!(is.numeric(x))){
    stop("x must be numeric")
  }

  if(length(x) < 168){
    stop("the length of x must be > 167")
  }


  if(min(x) <= 0){
    stop("x must be strictly positive")
  }
  ####################



  # target_max
  ####################

  if(!(is.numeric(target_max) && (length(target_max) == 1))){
    stop("target_max must be numeric of length 1")
  }

  if(!(target_max > 0)){
    stop("target_max must be > 0")
  }
  ####################




  # target_lf
  ####################

  if(!(is.numeric(target_lf) && (length(target_lf) == 1))){
    stop("target load factor (target_lf) must
        be numeric of length 1")
  }

  if(!((target_lf > 0) && (target_lf < 1))){
    stop("target load factor (target_lf) must be
        in between 0 and 1 (exclusive)")
  }
  ####################


  n <- length(x)
  x_max <- max(x)
  x_min <- min(x)
  x_avg <- mean(x)
  x_sum <- sum(x)

  x_lf <- x_avg / x_max



  x_index <- c(1:length(x))
  df_work <- data.frame(cbind(x_index, x))

  df_work$x_rank <- n + 1 - rank(x, ties.method = "random")
  df_work$x_ordered <- sort(x, decreasing = TRUE)



  df_work$x_pu <- df_work$x / x_max
  df_work$x_ordered_pu <- df_work$x_ordered / x_max




  temp_num = sum(df_work$x_ordered_pu) - target_lf * n
  temp_den = sum(df_work$x_ordered_pu * (df_work$x_index - 1))

  beta = temp_num / temp_den



  tmp_mult_ordered = 1 - (df_work$x_index - 1) * beta

  df_work$mult <- tmp_mult_ordered
  df_work$y_ordered_pu <- df_work$x_ordered_pu * tmp_mult_ordered

  df_work$y_ordered_pu2 <- sort(df_work$y_ordered_pu,
                                decreasing = TRUE)



  df_work$y_pu <- 1
  df_work$y_pu2 <- 1
  for(i in 1:n){
    df_work$y_pu[i] <- df_work$y_ordered_pu[df_work$x_rank[i]]
  }
  for(i in 1:n){
    df_work$y_pu2[i] <- df_work$y_ordered_pu2[df_work$x_rank[i]]
  }

  df_work$y <- df_work$y_pu * target_max
  df_work$y2 <- df_work$y_pu2 * target_max

  base_load_factor <- x_lf
  target_load_factor <- target_lf
  derived_load_factor <- mean(df_work$y) / max(df_work$y)




  test_decreasing <- max(diff(df_work$y_ordered_pu)) > 0
  test_lf <- abs(target_load_factor - derived_load_factor) > 0.01
  test_minval <- min(df_work$y_ordered_pu) < 0

  if(test_decreasing){
    message("Note: Increasing multipliers (mult) do not yield
           decreasing y_ordered_pu")
  }

  if(test_lf){
    warning("Derived load factor differs from target by
            more than 1%; see summary()")
  }

  if(test_minval){
    warning("Minimum per unit value of y_pu is negative, yielding
           negative load")
  }







  base_max <- x_max
  target_max <- target_max
  derived_max <- max(df_work$y)



  base_min <- x_min
  derived_min <- min(df_work$y)

  max_mult <- max(df_work$mult)
  min_mult <- min(df_work$mult)



  return_object <- list(
    df = df_work,
    beta = beta,
    max_mult = max_mult,
    min_mult = min_mult,


    base_load_factor = base_load_factor,
    target_load_factor = target_load_factor,
    derived_load_factor = derived_load_factor,

    base_max = base_max,
    target_max = target_max,
    derived_max = derived_max,

    base_min = base_min,
    derived_min = derived_min,

    dec_flag = test_decreasing,
    lf_flag = test_lf,
    min_pu_flag = test_minval

  )

  class(return_object) <- "lslin"
  return(return_object)

}


