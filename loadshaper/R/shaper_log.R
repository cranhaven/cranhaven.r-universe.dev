
#' @title Logistic Method for Matching Peak and Load Factor


#' @param x A numeric array, representing reference load shape.
#' All values must be strictly positive containing no \code{NA}(s).
#' The length of \code{x}
#' must be > 167.
#'
#' @param target_max Target peak value of resultant load shape, must
#' be > 0.
#'
#'
#' @param target_lf Target load factor of resultant load shape, must be
#' numeric in between 0 and 1 (exclusive).
#'
#'
#'
#' @param k Steepness parameter, must be a  positive number.
#' See "Details".
#'
#'
#'
#' @param inf_pos Inflection point parameter.
#' See "Details".
#'
#'
#'
#'
#' @param iter Number of iterations for solving certain parameter.
#' Must be  >= 30.
#' See "Details".
#'
#' @param def_l Start parameter for solving \code{l}, must be
#' a positive numeric.
#'
#'
#'
#'
#'
#'
#'
#' @return A list of class \code{"lslog"}, having following elements:
#' \itemize{
#'
#' \item{\code{df}: A data frame. See "Details".}
#'
#' \item{\code{k}: Steepness parameter. See "Details".}
#'
#' \item{\code{inf_pos}: Inflection point parameter.
#' See "Details".}
#'
#'
#' \item{\code{L}: Numerically solved optimized L parameter.
#'  See "Details".}
#'
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
#' @seealso \code{\link{lslin}},
#' \code{\link{print.lslog}},
#' \code{\link{summary.lslog}},
#' \code{\link{plot.lslog}},
#' \code{\link{lscore}}
#'
#'
#' @details The algorithm first evaluates the load factor of the
#' reference load shape \code{x}, which is
#' defined by the ratio of average to peak value. If the target load
#' factor is greater than reference level, then all base
#' values are multiplied by a number \code{ > 1}. If the target
#' load factor is less than reference level, then all base values are
#' multiplied by a number \code{ < 1}. The multipliers
#' increase or decrease with a sigmoid pattern.
#'
#' The sigmoid function is a transformed version of
#' \deqn{
#' f(x)=\frac{L}{1 - exp(-k(x-x_0))}
#' }
#' Parameter \eqn{k} is shape parameter, shaping the
#' "sigmoidness" of the function. Larger value of \code{k}
#' indicates  more steepness in the function and lower value
#' results in changes in multipliers in more linear fashion.
#'
#' Location parameter \eqn{x_0} controls the inflection point
#' of the function and derived from \code{inf_pos}.
#' \code{inf_pos = 0.5} indicates the inflection point of
#' the sigmoid multipliers is halfway.
#'
#' The \eqn{L} parameter in the sigmoid is numerically solved.
#' The number of iterations is equal to the \code{iter} argument,
#' optimized based on the minimum difference between the derived
#' and target load factor.
#'
#'
#'
#'
#'
#'
#'
#'
#'
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
#'
#'
#' @examples
#' loads <- ercot[ercot$Year == 2019, ]$COAST
#' plot(loads, type = "l")
#' logistic_loadshape <- lslog(loads, target_lf = 0.50, k = 0.5)
#' summary(logistic_loadshape)
#' #---------------------------------------------------
#' loads2 <- ercot[ercot$Year == 2020, ]$ERCOT
#' plot(loads2, type = "l")
#' logistic_loadshape2 <- lslog(loads2, target_lf = 0.6,
#'                             k = 0.5, inf_pos = 0.4)
#' summary(logistic_loadshape2)
#' #---------------------------------------------------
#' loads3 <- ercot[ercot$Year == 2020, ]$ERCOT
#' plot(loads3, type = "l")
#' logistic_loadshape3 <- lslog(loads3, target_lf = 0.9)
#' summary(logistic_loadshape3)
#'
#'
#'
#'
#'
#'
#' @import stats
#'
#'
#' @export
lslog <- function(x, target_max = 10000,
                  target_lf = 0.7, k = 1,
                  inf_pos = 0.5, iter = 500, def_l = 1)
{

  # iter
  #--------------------------------------------------
  if(!(is.numeric(iter) && (length(iter) == 1))){
    stop("iter must be numeric of length 1")
  }

  # if(!(is.integer(iter))){
  #   stop("iter must be an integer")
  # }

  if(iter < 30){
    stop("iter must be >= 30")
  }



  # inf_pos
  #--------------------------------------------------
  if(!(is.numeric(inf_pos) && (length(inf_pos) == 1))){
    stop("inf_pos must be numeric of length 1")
  }


  # k
  #--------------------------------------------------
  if(!(is.numeric(k) && (length(k) == 1))){
    stop("k must be numeric of length 1")
  }


  if(k <= 0){
    stop("k must be strictly positive")
  }


  # target_max
  #--------------------------------------------------

  if(!(is.numeric(target_max) && (length(target_max) == 1))){
    stop("target_max must be numeric of length 1")
  }

  if(!(target_max > 0)){
    stop("target_max must be > 0")
  }


  # target_lf
  #--------------------------------------------------

  if(!(is.numeric(target_lf) && (length(target_lf) == 1))){
    stop("target load factor (target_lf) must
        be numeric of length 1")
  }

  if(!((target_lf > 0) && (target_lf < 1))){
    stop("target load factor (target_lf) must be
        in between 0 and 1 (exclusive)")
  }


  # def_l
  #--------------------------------------------------
  tmp_cond_1 <- !(class(def_l) == "numeric")
  tmp_cond_2 <- !(length(def_l) == 1)
  tmp_cond_3 <- !(def_l > 0)
  if(tmp_cond_1 || tmp_cond_2 || tmp_cond_3){
    stop("def_l must be a positive numeric of
         length 1")
  }







  # x
  #--------------------------------------------------
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
  #--------------------------------------------------


  n <- length(x)
  x_max <- max(x)
  x_min <- min(x)
  x_avg <- mean(x)
  x_sum <- sum(x)
  x_lf <- x_avg / x_max


  # if(target_lf < x_lf)
  # {
  x_index <- c(1:length(x))
  df_work <- data.frame(cbind(x_index, x))

  df_work$x_rank <- n + 1 - rank(x, ties.method = "random")
  df_work$x_ordered <- sort(x, decreasing = TRUE)



  df_work$x_pu <- df_work$x / x_max
  df_work$x_ordered_pu <- df_work$x_ordered / x_max


  tmp_logistic <- function(x, l, k, x0){
    return(l/(1 + exp(-k*(x - x0))))
  }


  k_factor <- 12 / n
  x0 <- inf_pos * n


  del_l <- 1 / iter
  l_ser <- seq(0, (def_l - del_l/2), del_l)
  delta_lf_ser <- rep(0, length(l_ser))



  if(target_lf <= x_lf)

  {
    for(i in 1:length(l_ser)){
      l <- l_ser[i]
      tmp_mult_ordered <- 1:n
      tmp_mult_ordered <- 1 - tmp_logistic(c(1:n),
                                           l, k*k_factor, x0)
      tmp_mult_ordered <- tmp_mult_ordered / max(tmp_mult_ordered)
      tmp_pu_ordered <- df_work$x_ordered_pu * tmp_mult_ordered
      delta_lf_ser[i] <- abs(mean(tmp_pu_ordered) - target_lf)
    }

    l <- l_ser[which.min(delta_lf_ser)]
    tmp_mult_ordered <- 1:n
    tmp_mult_ordered <- 1 - tmp_logistic(c(1:n),
                                         l, k * k_factor, x0)
    tmp_mult_ordered <- tmp_mult_ordered / max(tmp_mult_ordered)
  }else
  {
    for(i in 1:length(l_ser)){
      l <- l_ser[i]
      tmp_mult_ordered <- 1:n
      tmp_mult_ordered <- 1 + tmp_logistic(c(1:n),
                                           l, k*k_factor, x0)
      tmp_mult_ordered <- tmp_mult_ordered / min(tmp_mult_ordered)
      tmp_pu_ordered <- df_work$x_ordered_pu * tmp_mult_ordered
      delta_lf_ser[i] <- abs(mean(tmp_pu_ordered) - target_lf)
    }

    l <- l_ser[which.min(delta_lf_ser)]
    tmp_mult_ordered <- 1:n
    tmp_mult_ordered <- 1 + tmp_logistic(c(1:n),
                                         l, k * k_factor, x0)
    tmp_mult_ordered <- tmp_mult_ordered / min(tmp_mult_ordered)
  }




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
  derived_load_factor <- mean(df_work$y2) / max(df_work$y2)





  test_decreasing <- max(diff(df_work$y_ordered_pu)) > 0
  test_lf <- abs(derived_load_factor - target_lf) > 0.01
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





  lf_flag <- abs(target_load_factor - derived_load_factor) > 0.01


  base_max <- x_max
  target_max <- target_max
  derived_max <- max(df_work$y)



  base_min <- x_min
  derived_min <- min(df_work$y)

  max_mult <- max(df_work$mult)
  min_mult <- min(df_work$mult)


  return_object <- list(
    df = df_work,

    k = k,
    inf_pos = inf_pos,
    L = l,

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

  class(return_object) <- "lslog"
  return(return_object)
}
