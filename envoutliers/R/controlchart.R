#' Table of Control Charts Constants - Only intended for developer use
#'
#' Creation of Table of Control Chart Constants.
#' The function is called by \code{\link{control.limits.x}}, \code{\link{control.limits.R}} and \code{\link{control.limits.s}}. This function is not intended for use by regular users of the package.
#'
#' @details This function creates a table with columns giving constants for computation limits of control charts.
#' The function is exported for developer use only. It does not have any input parameters and does not perform any checks on inputs since it is only a convenience function for computing control chart limits.
#' @return data.frame whose columns are numeric vectors giving constants for control charts limits computation
#' @references JOGLEKAR, Anand M. Statistical methods for six sigma: in R&D and manufacturing. Hoboken, NJ: Wiley-Interscience. ISBN sbn0-471-20342-4.
get.norm <- function() {
  nk = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
  nA = c(2.121,1.732,1.500,1.342,1.225,1.134,1.061,1.000,0.949,0.905,0.866,0.832,0.802,0.775,0.750,0.728,0.707,0.688,0.671,0.655,0.640,0.626,0.612,0.600)
  nA2 = c(1.880,1.023,0.729,0.577,0.483,0.419,0.373,0.337,0.308,0.285,0.266,0.249,0.235,0.223,0.212,0.203,0.194,0.187,0.180,0.173,0.167,0.162,0.157,0.153)
  nA3 = c(2.659,1.954,1.628,1.427,1.287,1.182,1.099,1.032,0.975,0.927,0.886,0.850,0.817,0.789,0.763,0.739,0.718,0.698,0.680,0.663,0.647,0.633,0.619,0.606)
  nB3 = c(0.000,0.000,0.000,0.000,0.030,0.118,0.185,0.239,0.284,0.321,0.354,0.382,0.406,0.428,0.448,0.466,0.482,0.497,0.510,0.523,0.534,0.545,0.555,0.565)
  nB4 = c(3.267,2.568,2.266,2.089,1.970,1.882,1.815,1.761,1.716,1.679,1.646,1.618,1.594,1.572,1.552,1.534,1.518,1.503,1.490,1.477,1.466,1.455,1.445,1.435)
  nB5 = c(0.000,0.000,0.000,0.000,0.029,0.113,0.179,0.232,0.276,0.313,0.346,0.374,0.399,0.421,0.440,0.458,0.475,0.490,0.504,0.516,0.528,0.539,0.549,0.559)
  nB6 = c(2.606,2.276,2.088,1.964,1.874,1.806,1.751,1.707,1.669,1.637,1.610,1.585,1.563,1.544,1.526,1.511,1.496,1.483,1.470,1.459,1.448,1.438,1.429,1.420)
  nD1 = c(0.000,0.000,0.000,0.000,0.000,0.204,0.388,0.547,0.687,0.811,0.922,1.025,1.118,1.203,1.282,1.356,1.424,1.487,1.549,1.605,1.659,1.710,1.759,1.806)
  nD2 = c(3.686,4.358,4.698,4.918,5.078,5.204,5.306,5.393,5.469,5.535,5.594,5.647,5.696,5.741,5.782,5.820,5.856,5.891,5.921,5.951,5.979,6.006,6.031,6.056)
  nD3 = c(0.000,0.000,0.000,0.000,0.000,0.076,0.136,0.184,0.223,0.256,0.283,0.307,0.328,0.347,0.363,0.378,0.391,0.403,0.415,0.425,0.434,0.443,0.451,0.459)
  nD4 = c(3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777,1.744,1.717,1.693,1.672,1.653,1.637,1.622,1.608,1.597,1.585,1.575,1.566,1.557,1.548,1.541)
  nC4 = c(0.7979,0.8862,0.9213,0.9400,0.9515,0.9594,0.9650,0.9693,0.9727,0.9754,0.9776,0.9794,0.9810,0.9823,0.9835,0.9845,0.9854,0.9862,0.9869,0.9876,0.9882,0.9887,0.9892,0.9896)
  nX1.C4 = c(1.2533,1.1284,1.0854,1.0638,1.0510,1.0423,1.0363,1.0317,1.0281,1.0252,1.0229,1.0210,1.0194,1.0180,1.0168,1.0157,1.0148,1.0140,1.0133,1.0126,1.0119,1.0114,1.0109,1.0105)
  nd2 = c(1.128,1.693,2.059,2.326,2.534,2.704,2.847,2.970,3.078,3.173,3.258,3.336,3.407,3.472,3.532,3.588,3.640,3.689,3.735,3.778,3.819,3.858,3.895,3.931)
  nl.d2 = c(0.8865,0.5907,0.4857,0.4299,0.3946,0.3698,0.3512,0.3367,0.3249,0.3152,0.3069,0.2998,0.2935,0.2880,0.2831,0.2787,0.2747,0.2711,0.2677,0.2647,0.2618,0.2592,0.2567,0.2544)
  nd3 = c(0.8525, 0.8884, 0.8798, 0.8641, 0.8480, 0.8332, 0.8198, 0.8078, 0.7971, 0.7873, 0.7785, 0.7704, 0.7630, 0.7562, 0.7499, 0.7441, 0.7386, 0.7335, 0.7287, 0.7242, 0.7199, 0.7159, 0.7121, 0.7084)

  norm = data.frame(nk, nA, nA2, nA3, nB3, nB4, nB5, nB6, nD1, nD2, nD3, nD4, nC4, nX1.C4, nd2, nl.d2, nd3)
  names(norm) = c("k", "A", "A2", "A3", "B3", "B4", "B5", "B6", "D1", "D2", "D3", "D4", "C4", "X1.C4", "d2", "l.d2", "d3")

  return(norm)
}

#' Limits for control chart \emph{x} - Only intended for developer use
#'
#' Estimation of limits of control chart \emph{x}. The function is called by \code{\link{KRDetect.outliers.controlchart}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param group.size a positive integer giving the number of observations in individual segments used for computation of control chart limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis.
#' @param method a character string specifying the preferred estimate of standard deviation parameter.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"range"}} {for estimation based on sample ranges}
#'   \item{\code{"sd"}} {for estimation based on sample standard deviations}
#' }
#' @param L a positive numeric value giving parameter \code{L} specifying the width of control limits.
#' @details This function computes parameters based on which control chart \emph{x} can be constructed.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for identification limits based on control chart \emph{x}.
#' @return A list is returned with elements:
#' \item{x}{a numeric vector of data}
#' \item{mean}{a numeric value giving sample mean of vector \code{x}}
#' \item{groups.count}{a numeric value giving a number of segments used for estimating parameters of control chart}
#' \item{groups.mean}{a numeric vector giving sample means in individual segments used for estimating parameters of control chart}
#' \item{LCL}{numeric value giving lower control limit of control chart \emph{x}}
#' \item{UCL}{numeric value giving upper control limit of control chart \emph{s}}
#' @references Shewhart W (1931). Quality control chart. Bell System Technical Journal, 5, 593–603.
#'
#' SAS/QC User's Guide, Version 8, 1999. SAS Institute, Cary, N.C.
#'
#' Wild C, Seber G (2000). Chance encounters: A first course in data analysis and inference. New York: John Wiley.
#' @importFrom stats sd
control.limits.x <- function(x,
                             method = "range",
                             group.size,
                             L) {
  # input data check
  if (!method %in% c("range", "sd")) {
    stop("Invalid method")
  }

  if (length(which(is.na(x))) > 0) {
    stop("NA values in input data")
  }

  if (group.size < 2) {
    stop("Group size must be grater than 1")
  }

  data = x
  k = length(data) %/% group.size # groups count

  norm = get.norm()
  d2 = norm$d2[norm$k == group.size]
  A2 = L / (d2 * sqrt(group.size))
  C4 = norm$C4[norm$k == group.size]
  A3 = L / (C4 * sqrt(group.size))

  if (!(k * group.size == length(data))) {
    warning(paste("Data can not be equidistantly divided, the first", length(data) %% group.size, "values are excluded from the analysis"))
    data = data[c((length(data) - k * group.size + 1):length(data))]
  }

  groups = seq(from = 1, to = length(data), by = group.size)
  groups.mean = c(rep(NA, k))
  groups.range = c(rep(NA, k))
  groups.sd = c(rep(NA, k))

  for (i in 1:k) {
    group.data = data[(groups[i]):(groups[i] + group.size - 1)]
    groups.mean[i] = mean(group.data)
    groups.range[i] = max(group.data) - min(group.data)
    groups.sd[i] = sd(group.data)
  }

  LCL.R = mean(data) - A2 * mean(groups.range)
  UCL.R = mean(data) + A2 * mean(groups.range)
  LCL.s = mean(data) - A3 * mean(groups.sd) # upper control limit
  UCL.s = mean(data) + A3 * mean(groups.sd) # lower control limit

  if (method == "range") {
    LCL = LCL.R
    UCL = UCL.R
  } else if (method == "sd") {
    LCL = LCL.s
    UCL = UCL.s
  } else {
    stop("Invalid method")
  }

  result = list(x = data,
                mean = mean(data),
                groups.count = k,
                groups.mean = groups.mean,
                LCL = LCL,
                UCL = UCL)

  return(result)
}

#' Limits for control chart \emph{s} - Only intended for developer use
#'
#' Estimation of limits of control chart \emph{s}. The function is called by \code{\link{KRDetect.outliers.controlchart}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param group.size a positive integer giving the number of observations in individual segments used for computation of control chart limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis.
#' @param L a positive numeric value giving parameter \code{L} specifying the width of control limits.
#' @details This function computes parameters based on which control chart \emph{s} can be constructed.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for identification limits based on control chart \emph{s}.
#' @return A list is returned with elements:
#' \item{x}{a numeric vector of data}
#' \item{sd.est}{a numeric value giving an estimate of standard deviation parameter}
#' \item{groups.count}{a numeric value giving a number of segments used for estimating parameters of control chart}
#' \item{groups.sd}{a numeric vector giving sample standard deviations in individual segments used for estimating parameters of control chart}
#' \item{LCL}{a numeric value giving lower control limit of control chart \emph{s}}
#' \item{UCL}{a numeric value giving upper control limit of control chart \emph{s}}
#' @references Shewhart W (1931). Quality control chart. Bell System Technical Journal, 5, 593–603.
#'
#' SAS/QC User's Guide, Version 8, 1999. SAS Institute, Cary, N.C.
#'
#' Wild C, Seber G (2000). Chance encounters: A first course in data analysis and inference. New York: John Wiley.
#' @importFrom stats sd
control.limits.s <- function(x,
                             group.size,
                             L) {
  # input data check
  if (length(which(is.na(x))) > 0) {
    stop("NA values in input data")
  }

  if (group.size < 2) {
    stop("Group size must be grater than 1")
  }

  data = x
  k = length(data) %/% group.size # groups count

  norm = get.norm()
  C4 = norm$C4[norm$k == group.size]
  B3 = max(0, 1 - (L / C4) * sqrt(1 - C4 ^ 2))
  B4 = 1 + (L / C4) * sqrt(1 - C4 ^ 2)

  if (!(k * group.size == length(data))) {
    warning(paste("Data can not be equidistantly divided, the first", length(data) %% group.size, "values are excluded from the analysis"))
    data = data[c((length(data) - k * group.size + 1):length(data))]
  }

  groups = seq(from = (1 + length(data) %% group.size), to = length(data), by = group.size)
  groups.sd = c(rep(NA, k))

  for (i in 1:k) {
    group.data = data[(groups[i]):(groups[i] + group.size - 1)]
    groups.sd[i] = sd(group.data)
  }

  LCL = B3 * mean(groups.sd)
  UCL = B4 * mean(groups.sd)

  result = list(x = data,
                sd.est = mean(groups.sd),
                groups.count = k,
                groups.sd = groups.sd,
                LCL = LCL,
                UCL = UCL)

  return(result)
}

#' Limits for control chart \emph{R} - Only intended for developer use
#'
#' Estimation of limits of control chart \emph{R}. The function is called by \code{\link{KRDetect.outliers.controlchart}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param group.size a positive integer giving the number of observations in individual segments used for computation of control chart limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis.
#' @param L a positive numeric value giving parameter \code{L} specifying the width of control limits.
#' @details This function computes parameters based on which control chart \emph{R} can be constructed.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for identification limits based on control chart \emph{R}.
#' @return A list is returned with elements:
#' \item{x}{a numeric vector of data}
#' \item{range.est}{a numeric value giving an estimate of range parameter}
#' \item{groups.count}{a numeric value giving a number of segments used for estimating parameters of control chart}
#' \item{groups.range}{a numeric vector giving sample ranges in individual segments used for estimating parameters of control chart}
#' \item{LCL}{a numeric value giving lower control limit of control chart \emph{R}}
#' \item{UCL}{a numeric value giving upper control limit of control chart \emph{R}}
#' @references Shewhart W (1931). Quality control chart. Bell System Technical Journal, 5, 593–603.
#'
#' SAS/QC User's Guide, Version 8, 1999. SAS Institute, Cary, N.C.
#'
#' Wild C, Seber G (2000). Chance encounters: A first course in data analysis and inference. New York: John Wiley.
#' @importFrom stats sd
control.limits.R <- function(x,
                             group.size,
                             L) {
  # input data check
  if (length(which(is.na(x))) > 0) {
    stop("NA values in input data")
  }

  if (group.size < 2) {
    stop("Group size must be grater than 1")
  }

  data = x
  k = length(data) %/% group.size # groups count

  norm = get.norm()
  d2 = norm$d2[norm$k == group.size]
  d3 = norm$d3[norm$k == group.size]
  D3 = max(0, 1 - L * d3 / d2)
  D4 = 1 + L * d3 / d2

  if (!(k * group.size == length(data))) {
    warning(paste("Data can not be equidistantly divided, the first", length(data) %% group.size, "values are excluded from the analysis"))
    data = data[c((length(data) - k * group.size + 1):length(data))]
  }

  groups = seq(from = (1 + length(data) %% group.size), to = length(data), by = group.size)
  groups.range = c(rep(NA, k))

  for (i in 1:k) {
    group.data = data[(groups[i]):(groups[i] + group.size - 1)]
    groups.range[i] = max(group.data) - min(group.data)
  }

  LCL = D3 * mean(groups.range)
  UCL = D4 * mean(groups.range)

  result = list(x = data,
                range.est = mean(groups.range),
                groups.count = k,
                groups.range = groups.range,
                LCL = LCL,
                UCL = UCL)

  return(result)
}

#' Identification of outliers using control charts
#'
#' Identification of outliers in environmental data using two-step method based on kernel smoothing and control charts (Campulova et al., 2017).
#' The outliers are identified as observations corresponding to segments of smoothing residuals exceeding control charts limits.
#'
#' @param x data values.
#' Supported data types
#' \itemize{
#'   \item{a numeric vector}
#'   \item{a time series object \code{ts}}
#'   \item{a time series object \code{xts}}
#'   \item{a time series object \code{zoo}}
#' }
#' @param perform.smoothing a logical value specifying if data smoothing is performed. If \code{TRUE} (default), data are smoothed.
#' @param bandwidth.type a character string specifying the type of bandwidth.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"local"}} {(default) to use local bandwidth}
#'   \item{\code{"global"}} {to use global bandwidth}
#' }
#' @param bandwidth.value a local bandwidth array (for \code{bandwidth.type = "local"}) or global bandwidth value (for \code{bandwidth.type = "global"}) for kernel regression estimation. If \code{bandwidth.type = "NULL"} (default) a data-adaptive local plug-in (Herrmann, 1997) (for \code{bandwidth.type = "local"}) or data-adaptive global plug-in (Gasser et al., 1991) (for \code{bandwidth.type = "global"}) bandwidth is used instead.
#' @param kernel.order a nonnegative integer giving the order of the optimal kernel (Gasser et al., 1985) used for smoothing.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{kernel.order = 2}} {(default)}
#'   \item{\code{kernel.order = 4}}
#' }
#' @param method a character string specifying the preferred estimate of standard deviation parameter.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"range"}} {(default) for estimation based on sample ranges}
#'   \item{\code{"sd"}} {for estimation based on sample standard deviations}
#' }
#' @param group.size.x a positive integer giving the number of observations in individual segments used for computation of \emph{x} chart control limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis. Default is \code{group.size.x = 3}.
#' @param group.size.R a positive integer giving the number of observations in individual segments used for computation of \emph{R} chart control limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis. Default is \code{group.size.R = 3}.
#' @param group.size.s a positive integer giving the number of observations in individual segments used for computation of \emph{s} chart control limits.
#' If the data can not be equidistantly divided, the first extra values will be excluded from the analysis. Default is \code{group.size.s = 3}.
#' @param L.x a positive numeric value giving parameter \code{L} specifying the width of \emph{x} chart control limits. Default is \code{L.x = 3}.
#' @param L.R a positive numeric value giving parameter \code{L} specifying the width of \emph{R} chart control limits. Default is \code{L.R = 3}.
#' @param L.s a positive numeric value giving parameter \code{L} specifying the width of \emph{s} chart control limits. Default is \code{L.s = 3}.
#' @details This function identifies outliers in environmental data using two-step procedure (Campulova et al., 2017).
#' The procedure consists of kernel smoothing and subsequent identification of observations corresponding to segments of smoothing residuals exceeding control charts limits.
#' This way the method does not identify individual outliers but segments of observations, where the outliers occur.
#' The output of the method are three logical vectors specyfing the outliers identified based on each of the three control charts.
#' Beside that logical vector specyfing the outliers identified based on at least one type of control limits is returned.
#' Crucial for the method is the choice of paramaters \code{L.x}, \code{L.R} and \code{L.s} specifying the width of control limits.
#' Different values of the parameters determine different criteria for outlier detection. For more information see (Campulova et al., 2017).
#' @return A \code{"KRDetect"} object which contains a list with elements:
#' \item{method.type}{a character string giving the type of method used for outlier idetification}
#' \item{x}{a numeric vector of observations}
#' \item{index}{a numeric vector of index design points assigned to individual observations}
#' \item{smoothed}{a numeric vector of estimates of the kernel regression function (smoothed data)}
#' \item{outlier.x}{a logical vector specyfing the identified outliers based on limits of control chart \emph{x}, \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{outlier.R}{a logical vector specyfing the identified outliers based on limits of control chart \emph{R}, \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{outlier.s}{a logical vector specyfing the identified outliers based on limits of control chart \emph{s}, \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{outlier}{a logical vector specyfing the identified outliers based on at least one type of control limits. \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{LCL.x}{a numeric value giving lower control limit of control chart \emph{x}}
#' \item{UCL.x}{a numeric value giving upper control limit of control chart \emph{x}}
#' \item{LCL.s}{a numeric value giving lower control limit of control chart \emph{s}}
#' \item{UCL.s}{a numeric value giving upper control limit of control chart \emph{s}}
#' \item{LCL.R}{a numeric value giving lower control limit of control chart \emph{R}}
#' \item{UCL.R}{a numeric value giving upper control limit of control chart \emph{R}}
#' @references Campulova M, Veselik P, Michalek J (2017). Control chart and Six sigma based algorithms for identification of outliers in experimental data, with an application to particulate matter PM10. Atmospheric Pollution Research. Doi=10.1016/j.apr.2017.01.004.
#'
#' Shewhart W (1931). Quality control chart. Bell System Technical Journal, 5, 593–603.
#'
#' SAS/QC User's Guide, Version 8, 1999. SAS Institute, Cary, N.C.
#'
#' Wild C, Seber G (2000). Chance encounters: A first course in data analysis and inference. New York: John Wiley.
#'
#' Joglekar, Anand M. Statistical methods for six sigma: in R&D and manufacturing. Hoboken, NJ: Wiley-Interscience. ISBN sbn0-471-20342-4.
#'
#' Gasser T, Kneip A, Kohler W (1991). A flexible and fast method for automatic smoothing. Journal of the American Statistical Association, 86, 643–652.
#'
#' Herrmann E (1997). Local bandwidth choice in kernel regression estimation. Journal of Computational and Graphical Statistics, 6(1), 35–54.
#'
#' Eva Herrmann; Packaged for R and enhanced by Martin Maechler (2016). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-8. https://CRAN.R-project.org/package=lokern
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' result = KRDetect.outliers.controlchart(x)
#' summary(result)
#' plot(result)
#' plot(result, plot.type = "x")
#' plot(result, plot.type = "R")
#' plot(result, plot.type = "s")
#' @importFrom lokern glkerns lokerns
#' @importFrom stats na.omit
#' @export
KRDetect.outliers.controlchart <- function(x,
                                           perform.smoothing = TRUE,
                                           bandwidth.type = "local",
                                           bandwidth.value = NULL,
                                           kernel.order = 2,
                                           method = "range",
                                           group.size.x = 3,
                                           group.size.R = 3,
                                           group.size.s = 3,
                                           L.x = 3,
                                           L.R = 3,
                                           L.s = 3) {
  # input data check
  if (any(c("ts", "zoo", "xts") %in% class(x))) {
    x = as.numeric(x)
  }

  if (!bandwidth.type %in% c("local", "global")) {
    stop("Invalid bandwidth type")
  }

  if (!kernel.order %in% c(2, 4)) {
    stop("Invalid kernel order.")
  }

  if (!method %in% c("range", "sd")) {
    stop("Invalid method")
  }

  if (group.size.x < 2 | group.size.R < 2 | group.size.s < 2) {
    stop("Group size must be grater than 1")
  }

  data.input = data.frame(x)
  data.input$index = c(1:dim(data.input)[1])
  data = na.omit(data.input)

  if (dim(data)[1] < 4) {
    stop("Not enough data")
  }

  data$smoothed = NA

  if (perform.smoothing) {
    data$smoothed = smoothing(data$index, data$x, bandwidth.type, bandwidth.value, kernel.order)$data.smoothed
    data$residuals = data$x - data$smoothed
  } else {
    data$residuals = data$x
  }

  data$outlier.x = FALSE
  data$outlier.R = FALSE
  data$outlier.s = FALSE

  x = control.limits.x(x = data$residuals, method = "range", group.size = group.size.x, L = L.x)
  R = control.limits.R(x = data$residuals, group.size = group.size.R, L = L.R)
  s = control.limits.s(x = data$residuals, group.size = group.size.s, L = L.s)

  data$x.groups.means = NA
  data$R.groups.ranges = NA
  data$s.groups.sd = NA

  for (i in 1:length(x$groups.mean)) {
    begin = length(data$residuals) - group.size.x * x$groups.count
    data$x.groups.means[(begin + 1 + (i - 1) * group.size.x):(begin + i * group.size.x)] = x$groups.mean[i]
  }

  data$outlier.x[data$x.groups.means < x$LCL | data$x.groups.means > x$UCL] = TRUE

  for (i in 1:length(R$groups.range)) {
    begin = length(data$residuals) - group.size.R * R$groups.count
    data$R.groups.ranges[(begin + 1 + (i - 1) * group.size.R):(begin + i * group.size.R)] = R$groups.range[i]
  }

  data$outlier.R[data$R.groups.ranges < R$LCL | data$R.groups.ranges > R$UCL] = TRUE

  for (i in 1:length(s$groups.sd)) {
    begin = length(data$residuals) - group.size.s * s$groups.count
    data$s.groups.sd[(begin + 1 + (i - 1) * group.size.s):(begin + i * group.size.s)] = s$groups.sd[i]
  }

  data$outlier.s[data$s.groups.sd < s$LCL | data$s.groups.sd > s$UCL] = TRUE
  data$outlier = data$outlier.x | data$outlier.R | data$outlier.s
  data.output = merge(data.input, data, by = c("index", "x"), all.x = TRUE)
  data.output = data.output[order(data.output$index),]

  result = list(method.type = "control chart",
                x = data.output$x,
                index = data.output$index,
                smoothed = data.output$smoothed,
                outlier.x = data.output$outlier.x,
                outlier.R = data.output$outlier.R,
                outlier.s = data.output$outlier.s,
                outlier = data.output$outlier,
                LCL.x = x$LCL,
                UCL.x = x$UCL,
                LCL.s = s$LCL,
                UCL.s = s$UCL,
                LCL.R = R$LCL,
                UCL.R = R$UCL)

  class(result) = "KRDetect"

  return(result)
}
