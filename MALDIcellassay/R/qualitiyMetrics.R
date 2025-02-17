.getSigmaFit <- function(res) {
  fits <- getCurveFits(res)

  purrr::map_dbl(fits, function(x) {
    yfit =  getFitValues(x$model)
    y =  getY(x$model)
    n <- length(y)

    sigmaFit <- sqrt(1/n*sum((y - yfit)^2))
    return(sigmaFit)
  })
}

.calculateZPrime <- function(pos, neg) {
  if(length(pos) < 2 | length(neg) < 2) {
    return(NA_integer_)
  }

  meanPos <- mean(pos, na.rm = TRUE)
  meanNeg <- mean(neg, na.rm = TRUE)
  sdPos <- sd(pos, na.rm = TRUE)
  sdNeg <- sd(neg, na.rm = TRUE)

  z <- 1 - (3 * (sdPos + sdNeg) / (abs(meanPos - meanNeg)))

  return(z)
}


#' internal calculation of ssmd
#'
#' @param pos numeric vector, positive sample
#' @param neg numeric vector, negative sample
#'
#' @return ssmd
#' @importFrom stats sd
#' @noRd

.calculateSSMD <- function(pos, neg) {
  # strictly standardized mean difference
  if(length(pos) < 2 | length(neg) < 2) {
    return(NA_integer_)
  }

  meanPos <- mean(pos, na.rm = TRUE)
  meanNeg <- mean(neg, na.rm = TRUE)
  sdPos <- sd(pos, na.rm = TRUE)
  sdNeg <- sd(neg, na.rm = TRUE)

  ssmd <- abs(meanPos - meanNeg) /  sqrt(sdNeg^2 + sdPos^2)

  return(ssmd)
}

.getTopAndBottomIntensityMatrix <- function(res, nConc) {
  concs <- unique(getConc(res))

  topIdx <- which(getConc(res) %in% concs[1:nConc])
  botIdx <- which(getConc(res) %in% concs[(length(concs)-nConc+1):length(concs)])

  intmatPos <- getIntensityMatrix(res,
                                  avg = FALSE,
                                  excludeNormMz = FALSE)[botIdx,]
  intmatNeg <- getIntensityMatrix(res,
                                  avg = FALSE,
                                  excludeNormMz = FALSE)[topIdx,]

  return(list(pos = intmatPos,
              neg = intmatNeg))
}

.calculateMetric <- function(res, nConc = 2, fun) {

  int <- .getTopAndBottomIntensityMatrix(res = res,
                                         nConc = nConc)


  metric <- vapply(X = seq_along(getAllMz(res,
                                          excludeNormMz = FALSE)),
                   FUN = function(i) {
                     fun(pos = int$pos[, i],
                         neg = int$neg[, i])
                   },
                   FUN.VALUE = numeric(1))

  return(metric)
}

#' Calculate Z'-factor of assay quality
#'
#' @param res      Object of class MALDIassay
#' @param internal Logical, currently only the internal implementation,
#'                 using `nConc` top and bottom concentrations, is implemented.
#' @param nConc    Numeric, number of top and bottom concentrations to be used
#'                 to calculate the pseudo positive and negative control.
#'                 Only used if `internal` is TRUE
#'
#' @details
#' The most common way to measure the quality of an assay is the so-called Z'-factor,
#' which describes the separation of the positive and negative control in terms of their standard deviations \eqn{\sigma_p} and \eqn{\sigma_n}.
#' The Z'-factor is defined as [Ji-Hu Zhang et al., A simple statistical parameter for use in evaluation and validation of high throughput screening assays](https://pubmed.ncbi.nlm.nih.gov/10838414/).
#' \deqn{Z' = 1 - (3 * (\sigma_p+\sigma_n))/|\mu_p-\mu_n|}
#'
#' where \eqn{\mu_p} and \eqn{\mu_p} is the mean value of the positive (response expected) and negative (no response expected) control, respectively.
#' Therefore, the assay quality is **independent of the shape of the concentration response curve** and solely depend on two control values.
#'
#' Note, if `internal` is set to TRUE, the `nConc` highest concentrations are assumed as positive control,
#' whereas the `nConc` lowest concentrations are used as negative.
#'
#' |**Value** |**Interpretation** |
#' |--|--|
#' |Z' ~ 1 | perfect assay |
#' |1 > Z' > 0.5 | excellent assay |
#' |0.5 > Z' > 0 | moderate assay |
#' |Z' = 0 | good only for yes/no response |
#' |Z' < 0 | unacceptable  |#'
#'
#' @return
#' Numeric vector of Z'-factors.
#' @importFrom purrr map_dbl
#' @export
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' calculateZPrime(Blank2022res, nConc = 2)       
#'  
calculateZPrime <- function(res, internal = TRUE, nConc = 2) {
  if(!internal) {
    stop("Currently only the internal implementation,
        using nConc top and bottom concentrations, is implemented.\n")
  }

  .calculateMetric(res = res,
                   nConc = nConc,
                   fun = .calculateZPrime)
}


#' Calculate strictly standardized mean difference (SSMD)
#'
#' @param res      Object of class MALDIassay
#' @param internal Logical, currently only the internal implementation,
#'                 using `nConc` top and bottom concentrations, is implemented.
#' @param nConc    Numeric, number of top and bottom concentrations to be used
#'                 to calculate the pseudo positive and negative control.
#'                 Only used if `internal` is TRUE
#'
#' @details
#' The strictly standardized mean difference (SSMD) is a measure of effect size.
#' It is the mean divided by the standard deviation of a difference between the positive and negative control.
#'
#' \deqn{\gamma=\frac{\mid\mu_n - \mu_p\mid}{\sqrt{\sigma_n^2 + \sigma_p^2}}}
#'
#' The SSMD can be easily be interpreted as it denotes the difference between positve and negative controls in units of standard deviation.
#'
#'
#'
#' @return
#' Numeric vector of strictly standardized mean differences (SSMD)
#' @export
#' 
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' 
#' calculateSSMD(Blank2022res, nConc = 2)       
#' 
calculateSSMD <- function(res, internal = TRUE, nConc = 2) {
  if(!internal) {
    stop("Currently only the internal implementation,
        using nConc top and bottom concentrations, is implemented.\n")
  }

  .calculateMetric(res = res,
                   nConc = nConc,
                   fun = .calculateSSMD)
}

#'  Calculate V'-Factor
#'
#' @param res      Object of class MALDIassay
#' @param internal Logical, currently only the internal implementation,
#'                 using `nConc` top and bottom concentrations, is implemented.
#'
#' @details
#' The V'-factor is a generalization of the Z'-factor to a dose-response curve.
#' See [M.-A. Bray and A. Carpenter, Advanced assay development guidelines for image-based high content screening and analysis](https://www.ncbi.nlm.nih.gov/books/NBK126174/pdf/Bookshelf_NBK126174.pdf) for details.
#' It is defined as:
#' \deqn{V' = 1 - 6 * \sigma_f/|\mu_p - \mu_n|}
#'
#' with
#'
#' \deqn{\sigma_f = \sqrt{1/N * \sum{y_fit - y_measured}^2}}
#'
#' In other words, \eqn{\sigma_f} is the standard deviation of residuals.
#'
#' Note, we do not need to estimate the variance for the mean of the positive and negative value.
#' So, this function uses the top and bottom asymptote directly instead of taking the top and bottom concentrations in consideration.
#'
#' @return
#' Numeric vector of V'-factors
#' @export
#' 
#' @examples
#' # see example for `fitCurve()` to see how this data was generated
#' data(Blank2022res)
#' 
#' calculateVPrime(Blank2022res) 
calculateVPrime <- function(res, internal = TRUE) {
  if(!internal) {
    stop("Currently only the internal implementation,
        using nConc top and bottom concentrations, is implemented.\n")
  }

  param <- getFittingParameters(res)

  meanPos <- as.numeric(param$top)
  meanNeg <- as.numeric(param$bottom)

  sigmaFit <- .getSigmaFit(res)

  v <- 1 - 6 * (sigmaFit/(abs(meanNeg - meanPos)))

  return(v)
}

