#' TrendSLR: A package providing improved techniques to estimate mean sea level
#' (trend), velocity and acceleration from sea level records.
#'
#' The \dQuote{TrendSLR} package provides improved estimates of mean sea level (trend)
#' and associated real-time velocities and accelerations from individual, annual
#' average ocean water level data records. Improved trend estimates are based on
#' Singular Spectrum Analysis (SSA) methods. Various gap-filling options are included
#' to accommodate incomplete time series records along with a range of diagnostic
#' tools to investigate the SSA decomposition of the time series. A wide range of
#' screen and plot to file options are available within the package.
#'
#' @section TrendSLR functions:
#' The \code{\link{msl.trend}} function is one of the key functions of the package
#' deconstructing annual average time series into a trend and
#' associated velocities and accelerations, filling necessary internal structures
#' which facilitate all functions in this package. The fixed settings built into
#' this function are based on the detailed research and development summarised
#' in Watson (2016a,b; 2018).
#'
#' The \code{\link{custom.trend}} function is the other key function which permits
#' customisation of key input parameters to enable improved isolation of trend
#' components (mean sea level) and estimated associated velocities and accelerations.
#' This function provides more flexibility for the expert analyst than the
#' \code{\link{msl.trend}} function with fixed inbuilt parameterisation.
#'
#' @references Watson, P.J., 2016a. Identifying the best performing time series
#' analytics for sea-level research. In: \emph{Time Series Analysis and
#' Forecasting, Contributions to Statistics}, pp. 261-278, ISBN 978-3-319-28725-6.
#' Springer International Publishing.
#'
#' Watson, P.J., 2016b. How to improve estimates of real-time acceleration in
#' the mean sea level signal. In: Vila-Concejo, A., Bruce, E., Kennedy, D.M.,
#' and McCarroll, R.J. (eds.), Proceedings of the 14th International Coastal
#' Symposium (Sydney, Australia). \emph{Journal of Coastal Research},
#' Special Issue, No. 75, pp. 780-785. Coconut Creek (Florida), ISSN 0749-0208.
#'
#' Watson, P.J., 2018. \emph{Improved Techniques to Estimate Mean Sea Level,
#' Velocity and Acceleration from Long Ocean Water Level Time Series to Augment
#' Sea Level (and Climate Change) Research.} PhD Thesis, University of New South
#' Wales, Sydney, Australia.
#'
#' @docType package
#'
#' @name TrendSLR
NULL
