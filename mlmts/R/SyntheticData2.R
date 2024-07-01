#'
#' @title SyntheticData2
#' @description Synthetic dataset containing 65 MTS generated from five
#' different generating processes.
#' @usage data(SyntheticData1)
#' @format A \code{list} with two elements, which are:
#' \describe{
#' \item{\code{data}}{A list with 65 MTS.}
#' \item{\code{classes}}{A numeric vector indicating the corresponding classes associated with the elements in \code{data}.}
#' }
#' @details Each element in \code{data} is a matrix formed by 400 rows (series length)
#' and 2 columns (dimensions). Series 1-15 were generated from a VAR(1) process
#' and series 16-30 were generated from a VMA(1) process. Series 31-45 were
#' generated from a QVAR(1) process and series 46-60 were generated from a different
#' QVAR(1) process. Finally, series 61-65 were generated from a VAR(1) model
#' different from the one associated with series 1-15. Note that series
#' 61-65 can be seen as anomalous elements in the dataset.
"SyntheticData2"
