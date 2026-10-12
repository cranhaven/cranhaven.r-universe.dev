#' Input data check
#' 
#' Check input data for: \itemize{
#'   \item minimum length of reported
#'   \item integer for reported
#'   \item positivity for delay_dist and reported
#'   \item sums to 1 for delay_dist}
#' Throw an error if any conditions are violated.
#' @inheritParams fit_incidence
#' @export
data_check <- function(
  reported,
  delay_dist
) {
  if(length(reported) < 10) stop("More observations needed to fit incidence.")
  if(sum(reported) < 10) warning("More observations needed to fit incidence.")
  if(any(reported != round(reported))) stop("Observations need to be integers.")
  if(any(reported < 0)) stop("Observations need to be positive.")
  if(any(delay_dist < 0)) stop("Delay distribution needs to be positive.")
  if(abs(sum(delay_dist) - 1) > 1E-8)  stop("Delay distribution needs to sum to 1.")
}

#' Pad reported data with zeros in front
#'
#' Add zeros in front of reported data avoid infections from before first reported date all being placed on first reported date.
#' 
#' @param reported An integer vector of reported cases
#' @param size An integer size of zero-padding
#' @return An integer vector of cases with size 0's in front
#' @export
front_zero_pad <- function(
  reported,
  size
) {
  return(c(rep(0, size), reported))
}

#' Make AR samples for extrapolation past end point
#'
#' Make auto-regressive (AR) samples for extrapolation past end point to help with right-censoring problems.
#'  
#' @inheritParams fit_incidence
#' @return A matrix of size (num_ar_samps x n + num_ar_steps)
#' @export
make_ar_extrap_samps <- function(
  reported, 
  num_ar_steps = 10, 
  num_ar_samps = 50, 
  seed = 1, 
  linear_tail = 14,
  extrapolation_prior_precision = 2) {
  n = length(reported)
  # Make a variance stabilizing transformation
  x = 2 * sqrt(reported + 3/8)
  if (linear_tail >= 7) {
    # Do some data checks; 0's in the right tail cause problems
    xtrend = utils::tail(x, linear_tail)
    z = 0:(linear_tail - 1)
    # Remove any data points more than 3 sd's away
    mean_val = mean(xtrend)
    sd_val = stats::sd(xtrend)
    valid_entries = (xtrend <= mean_val + 8*sd_val) & (xtrend >= mean_val - 8*sd_val)
    # 1 is selected as a threshold so that there will be enough case differences for a linear trend
    if ((sum(valid_entries) > 5) && (sd_val > 0.5)) {
      # Fit a ridge linear model to the tail
      # Center and scale data
      data_temp = data.frame(xtrend = xtrend[valid_entries], z = z[valid_entries])
      xtrend_mean = mean(data_temp$xtrend)
      xtrend_sd = stats::sd(data_temp$xtrend)
      z_mean = mean(data_temp$z)
      z_sd = stats::sd(data_temp$z)
      centered_data = as.data.frame(scale(data_temp, center = TRUE, scale = TRUE))
      # Now assume Gaussian prior over slope parameter: beta ~ N(0, 1/lambda)
      # Find MAP on scaled data
      beta = sum(centered_data$z * centered_data$xtrend) / (sum(centered_data$z^2) + extrapolation_prior_precision)
      scaled_extrapolation = (seq(linear_tail, num_ar_steps+linear_tail - 1) - z_mean) / z_sd
      xpred = beta * scaled_extrapolation * xtrend_sd + xtrend_mean
      xpred = round(pmax(xpred, 0))
    } else {
      xpred = rep(mean_val, num_ar_steps)
    }
  } else {
    # Use mean of last week for centering
    xpred = rep(mean(utils::tail(x, 7)), num_ar_steps)
  }
  sig = stats::sd(diff(x, lag = 1))
  set.seed(seed = seed)
  xsamps = mat.or.vec(num_ar_samps, n + num_ar_steps)
  for (it in 1:num_ar_samps) {
    # innovations, and 0-bounded process
    eps = sig*stats::rnorm(num_ar_steps)
    # follow the trend for linear tail days, and then flatten
    xrw = cumsum(eps) + xpred
    xsamps[it,] = c(x, xrw)
  }
  rsamps = round(pmax((xsamps / 2)^2 - 3/8, 0))
  return(rsamps)
}

#' Data processing wrapper
#' 
#' Does basic checks for reported data and delay distribution, front pads, and makes AR extrapolation.
#' 
#' @inheritParams fit_incidence
#' @return A list with elements: \itemize{
#'	\item extrap =  a matrix of size (num_ar_samps x n + num_ar_steps + front_pad_size)
#'	\item original = a vector of logicals for whether in original time series range}
#' @export
data_processing <- function(
  reported,
  delay_dist,
  num_ar_steps = 10,
  num_ar_samps = 100,
  seed = 1,
  linear_tail = 14,
  front_pad_size = 10,
  extrapolation_prior_precision = 2
) {
  # Basic data check
  data_check(
    reported,
    delay_dist
  )
  original = rep(TRUE, length(reported))
  # Front pad with 0's
  padded_reported <- front_zero_pad(reported = reported, size = front_pad_size)
  original = c(rep(FALSE, front_pad_size), original)
  # Make AR extrapolation
  padded_ar_matrix <- make_ar_extrap_samps(
    reported = padded_reported, 
    num_ar_steps = num_ar_steps, 
    num_ar_samps = num_ar_samps, 
    seed = seed, 
    linear_tail = linear_tail,
    extrapolation_prior_precision = extrapolation_prior_precision
  )
  original = c(original, rep(FALSE, num_ar_steps))
  return(list(
    extrap = padded_ar_matrix,
    original = original
  ))
}
