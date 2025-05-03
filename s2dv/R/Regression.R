#'Compute the regression of an array on another along one dimension.
#'
#'Compute the regression of the array 'datay' on the array 'datax' along the
#''reg_dim' dimension by least square fitting (default) or self-defined model. 
#'The function provides the slope of the regression, the intercept, and the
#'associated p-value and confidence interval. The filtered datay from the 
#'regression onto datax is also provided.\cr
#'The p-value relies on the F distribution, and the confidence interval relies 
#'on the student-T distribution.
#'
#'@param datay An numeric array as predictand including the dimension along
#'  which the regression is computed.
#'@param datax An numeric array as predictor. The dimension should be identical
#'  as parameter 'datay'.
#'@param reg_dim A character string indicating the dimension along which to 
#'  compute the regression. The default value is 'sdate'.
#'@param formula An object of class "formula" (see function \code{link[stats]{lm}}).
#'@param pval A logical value indicating whether to retrieve the p-value 
#'  or not. The default value is TRUE.
#'@param conf A logical value indicating whether to retrieve the confidence 
#'  intervals or not. The default value is TRUE.
#'@param sign A logical value indicating whether to compute or not the 
#'  statistical significance of the test The default value is FALSE.
#'@param alpha A numeric of the significance level to be used in the 
#'  statistical significance test. The default value is 0.05.
#'@param na.action A function or an integer. A function (e.g., na.omit, 
#'  na.exclude, na.fail, na.pass) indicates what should happen when the data 
#'  contain NAs. A numeric indicates the maximum number of NA position (it
#'  counts as long as one of datay and datax is NA) allowed for compute 
#'  regression. The default value is na.omit-
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. Default value is NULL.
#'
#'@import multiApply
#'@importFrom stats pf
#'@return 
#'A list containing:
#'\item{$regression}{
#'  A numeric array with same dimensions as parameter 'datay' and 'datax' except 
#'  the 'reg_dim' dimension, which is replaced by a 'stats' dimension containing 
#'  the regression coefficients from the lowest order (i.e., intercept) to 
#'  the highest degree. The length of the 'stats' dimension should be 
#'  \code{polydeg + 1}.  
#'}
#'\item{$conf.lower}{
#'  A numeric array with same dimensions as parameter 'daty' and 'datax' except
#'  the 'reg_dim' dimension, which is replaced by a 'stats' dimension containing 
#'  the lower value of the \code{siglev}\% confidence interval for all
#'  the regression coefficients with the same order as $regression. The length 
#'  of 'stats' dimension should be \code{polydeg + 1}. Only present if 
#'  \code{conf = TRUE}.
#'}
#'\item{$conf.upper}{
#'  A numeric array with same dimensions as parameter 'daty' and 'datax' except
#'  the 'reg_dim' dimension, which is replaced by a 'stats' dimension containing 
#'  the upper value of the \code{siglev}\% confidence interval for all
#'  the regression coefficients with the same order as $regression. The length 
#'  of 'stats' dimension should be \code{polydeg + 1}. Only present if 
#'  \code{conf = TRUE}.
#'}
#'\item{$p.val}{
#'  A numeric array with same dimensions as parameter 'daty' and 'datax' except
#'  the 'reg_dim' dimension, The array contains the p-value.
#'}
#'\item{sign}{
#'  A logical array of the statistical significance of the regression with the
#'  same dimensions as $regression. Only present if \code{sign = TRUE}.
#'}
#'\item{$filtered}{
#'  A numeric array with the same dimension as paramter 'datay' and 'datax', 
#'  the filtered datay from the regression onto datax along the 'reg_dim' 
#'  dimension.
#'}
#'
#'@examples
#'# Load sample data as in Load() example:
#'example(Load)
#'datay <- sampleData$mod[, 1, , ]
#'names(dim(datay)) <- c('sdate', 'ftime')
#'datax <- sampleData$obs[, 1, , ]
#'names(dim(datax)) <- c('sdate', 'ftime')
#'res1 <- Regression(datay, datax, formula = y~poly(x, 2, raw = TRUE))
#'res2 <- Regression(datay, datax, alpha = 0.1)
#'
#'@importFrom stats lm na.omit confint
#'@import multiApply
#'@export
Regression <- function(datay, datax, reg_dim = 'sdate', formula = y ~ x, 
                       pval = TRUE, conf = TRUE, sign = FALSE, alpha = 0.05,
                       na.action = na.omit, ncores = NULL) {

  # Check inputs 
  ## datay and datax
  if (is.null(datay) | is.null(datax)) {
    stop("Parameter 'datay' and 'datax' cannot be NULL.")
  }
  if (!is.numeric(datay) | !is.numeric(datax)) {
    stop("Parameter 'datay' and 'datax' must be a numeric array.")
  }
#  if (is.null(dim(datay)) | is.null(dim(datax))) {
#    stop("Parameter 'datay' and 'datax' must be at least one dimension 'reg_dim'.")
#  }
  if (is.null(dim(datay))) {  #is vector
    dim(datay) <- c(length(datay))
    names(dim(datay)) <- reg_dim
  }
  if (is.null(dim(datax))) {  #is vector
    dim(datax) <- c(length(datax))
    names(dim(datax)) <- reg_dim
  }
  if (any(is.null(names(dim(datay)))) | any(nchar(names(dim(datay))) == 0) |
      any(is.null(names(dim(datax)))) | any(nchar(names(dim(datax))) == 0)) {
    stop("Parameter 'datay' and 'datax' must have dimension names.")
  }
  if (!all(names(dim(datay)) %in% names(dim(datax))) | 
      !all(names(dim(datax)) %in% names(dim(datay)))) {
    stop("Parameter 'datay' and 'datax' must have same dimension name")
  }
  name_datay <- sort(names(dim(datay)))
  name_datax <- sort(names(dim(datax)))
  if (!all(dim(datay)[name_datay] == dim(datax)[name_datax])) {
    stop("Parameter 'datay' and 'datax' must have same length of all dimensions.")
  }
  ## reg_dim
  if (!is.character(reg_dim) | length(reg_dim) > 1) {
    stop("Parameter 'reg_dim' must be a character string.")
  }
  if (!reg_dim %in% names(dim(datay)) | !reg_dim %in% names(dim(datax))) {
    stop("Parameter 'reg_dim' is not found in 'datay' or 'datax' dimension.")
  }
  ## formula
  if (!inherits(formula, 'formula')) {
    stop("Parameter 'formula' must the an object of class 'formula'.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## alpha
  if (!is.numeric(alpha) | alpha < 0 | alpha > 1 | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## na.action
  if (!is.function(na.action) & !is.numeric(na.action)) {
      stop("Parameter 'na.action' must be a function for NA values or ",
           "a numeric indicating the number of NA values allowed ",
           "before returning NA.")
  }
  if (is.numeric(na.action)) {
    if (any(na.action %% 1 != 0) | any(na.action < 0) | length(na.action) > 1) {
      stop("Parameter 'na.action' must be a function for NA values or ",
           "a numeric indicating the number of NA values allowed ",
           "before returning NA.")
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Sort dimension
  name_datay <- names(dim(datay))
  name_datax <- names(dim(datax))
  order_datax <- match(name_datay, name_datax)
  datax <- Reorder(datax, order_datax)


  ###############################
  # Calculate Regression

  ## output_dims
  output_dims <- list(regression = 'stats', filtered = reg_dim)
  if (conf) output_dims <- c(output_dims, list(conf.lower = 'stats', conf.upper = 'stats'))
  if (pval) output_dims <- c(output_dims, list(p.val = NULL))
  if (sign) output_dims <- c(output_dims, list(sign = NULL))

  res <- Apply(list(datay, datax), 
               target_dims = reg_dim, 
               output_dims = output_dims,
               fun = .Regression, 
               formula = formula, pval = pval, conf = conf, sign = sign,
               alpha = alpha, na.action = na.action, 
               ncores = ncores)

  return(invisible(res))
}


.Regression <- function(y, x, formula = y~x, pval = TRUE, conf = TRUE,
                        sign = FALSE, alpha = 0.05, na.action = na.omit) {

  NApos <- seq_along(x)
  NApos[which(is.na(x) | is.na(y))] <- NA
  filtered <- rep(NA, length(x))
  check_na <- FALSE

  if (is.numeric(na.action)) {
    na_threshold <- na.action
    na.action <- na.omit
    check_na <- TRUE
  }

  lm.out <- lm(formula, data = data.frame(x = x, y = y), na.action = na.action)
  coeff <- lm.out$coefficients
  if (conf) {
    conf.lower <- confint(lm.out, level = 1 - alpha)[, 1]
    conf.upper <- confint(lm.out, level = 1 - alpha)[, 2]
  }
  if (pval | sign) {
    f <- summary(lm.out)$fstatistic
    p.val <- pf(f[1], f[2], f[3], lower.tail = F)
    if (sign) signif <- !is.na(p.val) & p.val <= alpha
  }
  filtered[!is.na(NApos)] <- y[!is.na(NApos)] - lm.out$fitted.values

  # Check if NA is too many
  if (check_na && sum(is.na(NApos)) > na_threshold) {  #turn everything into NA
    coeff[which(!is.na(coeff))] <- NA
    if (conf) {
      conf.lower[which(!is.na(conf.lower))] <- NA
      conf.upper[which(!is.na(conf.upper))] <- NA
    }
    if (pval) p.val[which(!is.na(p.val))] <- NA
    if (sign) signif[which(!is.na(signif))] <- NA
    filtered[which(!is.na(filtered))] <- NA
  }

  res <- list(regression = coeff, filtered = filtered)
  if (conf) res <- c(res, list(conf.lower = conf.lower, conf.upper = conf.upper))
  if (pval) res <- c(res, list(p.val = p.val))
  if (sign) res <- c(res, list(sign = signif))

  return(res)
}

