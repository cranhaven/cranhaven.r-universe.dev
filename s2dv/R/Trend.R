#'Compute the trend 
#'
#'Compute the linear trend or any degree of polynomial regression along the 
#'forecast time. It returns the regression coefficients (including the intercept)
#'and the detrended array. The confidence intervals and p-value are also 
#'provided if needed.\cr
#'The confidence interval relies on the student-T distribution, and the p-value 
#'is calculated by ANOVA.
#'
#'@param data An numeric array including the dimension along which the trend 
#'  is computed.
#'@param time_dim A character string indicating the dimension along which to 
#'  compute the trend. The default value is 'ftime'.
#'@param interval A positive numeric indicating the unit length between two 
#' points along 'time_dim' dimension. The default value is 1.
#'@param polydeg A positive integer indicating the degree of polynomial 
#'  regression. The default value is 1.
#'@param alpha A numeric indicating the significance level for the statistical
#'  significance test. The default value is 0.05. 
#'@param conf A logical value indicating whether to retrieve the confidence 
#'  intervals or not. The default value is TRUE.
#'@param pval A logical value indicating whether to compute the p-value or not. 
#'  The default value is TRUE.
#'@param sign A logical value indicating whether to retrieve the statistical
#'  significance based on 'alpha'. The default value is FALSE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing:
#'\item{$trend}{
#'  A numeric array with the first dimension 'stats', followed by the same 
#'  dimensions as parameter 'data' except the 'time_dim' dimension. The length
#'  of the 'stats' dimension should be \code{polydeg + 1}, containing the 
#'  regression coefficients from the lowest order (i.e., intercept) to the 
#'  highest degree.   
#'}
#'\item{$conf.lower}{
#'  A numeric array with the first dimension 'stats', followed by the same 
#'  dimensions as parameter 'data' except the 'time_dim' dimension. The length
#'  of the 'stats' dimension should be \code{polydeg + 1}, containing the 
#'  lower limit of the \code{(1-alpha)}\% confidence interval for all the 
#'  regression coefficients with the same order as \code{$trend}. Only present 
#'  \code{conf = TRUE}.
#'}
#'\item{$conf.upper}{
#'  A numeric array with the first dimension 'stats', followed by the same 
#'  dimensions as parameter 'data' except the 'time_dim' dimension. The length
#'  of the 'stats' dimension should be \code{polydeg + 1}, containing the 
#'  upper limit of the \code{(1-alpha)}\% confidence interval for all the 
#'  regression coefficients with the same order as \code{$trend}. Only present 
#'  \code{conf = TRUE}.
#'}
#'\item{$p.val}{
#'  A numeric array of p-value calculated by anova(). The first dimension 
#'  'stats' is 1, followed by the same dimensions as parameter 'data' except 
#'  the 'time_dim' dimension. Only present if \code{pval = TRUE}.
#'}
#'\item{$sign}{
#'  The statistical significance. Only present if \code{sign = TRUE}.
#'}
#'\item{$detrended}{
#'  A numeric array with the same dimensions as paramter 'data', containing the 
#'  detrended values along the 'time_dim' dimension.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'months_between_startdates <- 60
#'trend <- Trend(sampleData$obs, polydeg = 2, interval = months_between_startdates)
#'
#'@rdname Trend
#'@import multiApply
#'@importFrom stats anova
#'@export
Trend <- function(data, time_dim = 'ftime', interval = 1, polydeg = 1, alpha = 0.05,
                  conf = TRUE, pval = TRUE, sign = FALSE, ncores = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector
    dim(data) <- c(length(data))
    names(dim(data)) <- time_dim
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## interval
  if (!is.numeric(interval) | interval <= 0 | length(interval) > 1) {
    stop("Parameter 'interval' must be a positive number.")
  }
  ## polydeg
  if (!is.numeric(polydeg) | polydeg %% 1 != 0 | polydeg <= 0 |
      length(polydeg) > 1) {
    stop("Parameter 'polydeg' must be a positive integer.")
  }
  ## alpha
  if (!is.numeric(alpha) | any(alpha < 0) | any(alpha > 1) | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores)) {
      stop("Parameter 'ncores' must be a positive integer.")
    } else if (ncores %% 1 != 0 | ncores <= 0 | length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  ###############################
  # Calculate Trend

  ## output_dims
  output_dims <- list(trend = 'stats')
  if (conf) output_dims <- c(output_dims, list(conf.lower = 'stats', conf.upper = 'stats'))
  if (pval) output_dims <- c(output_dims, list(p.val = 'stats'))
  if (sign) output_dims <- c(output_dims, list(sign = 'stats'))

  output_dims <- c(output_dims, list(detrended = time_dim))
  
  output <- Apply(list(data),
                  target_dims = time_dim,
                  fun = .Trend,
                  output_dims = output_dims,
                  interval = interval, 
                  polydeg = polydeg, alpha = alpha, conf = conf,
                  pval = pval, sign = sign,
                  ncores = ncores)

  return(invisible(output))
}

.Trend <- function(x, interval = 1, polydeg = 1, alpha = 0.05,
                   conf = TRUE, pval = TRUE, sign = FALSE) {
  # x: [ftime]

  mon <- seq(x) * interval

  # remove NAs for potential poly()
  NApos <- seq_along(x)
  NApos[which(is.na(x))] <- NA
  x2 <- x[!is.na(NApos)]
  mon2 <- mon[!is.na(NApos)]

  if (length(x2) > 0) {
#    lm.out <- lm(x ~ mon, na.action = na.omit)
    lm.out <- lm(x2 ~ poly(mon2, degree = polydeg, raw = TRUE), na.action = na.omit)
    trend <- lm.out$coefficients  #intercept, slope1, slope2,...

    if (conf) {
      conf.lower <- confint(lm.out, level = (1 - alpha))[, 1]
      conf.upper <- confint(lm.out, level = (1 - alpha))[, 2]
    }

    if (pval | sign) {
      p.value <- as.array(stats::anova(lm.out)[['Pr(>F)']][1])
      if (pval) p.val <- p.value
      if (sign) signif <- !is.na(p.value) & p.value <= alpha 
    }

    detrended <- NULL
    detrended[!is.na(x)] <- x[!is.na(x)] - lm.out$fitted.values

  } else {

    trend <- rep(NA, polydeg + 1)
    detrended <- rep(NA, length(x))

    if (conf) {
      conf.lower <- rep(NA, polydeg + 1)
      conf.upper <- rep(NA, polydeg + 1)
    }

    if (pval) p.val <- as.array(NA)
    if (sign) signif <- as.array(FALSE)

  }

  output <- list(trend = trend)
  if (conf) output <- c(output, list(conf.lower = conf.lower, conf.upper = conf.upper))
  if (pval) output <- c(output, list(p.val = p.val))
  if (sign) output <- c(output, list(sign = signif))
  output <- c(output, list(detrended = detrended))

  return(output)
}
