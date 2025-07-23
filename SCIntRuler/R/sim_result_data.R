#' My Example Dataset
#'
#' An result example data with results from different functions.
#'
#' @format An result example data
#' \describe{
#'   \item{fullcluster}{A runable example of GetCluster, which is a list of clusters for each study.}
#'   \item{normCount}{A runable example of NormData, which is a list of normalized RNA expression matrixs for each study.}
#'   \item{distmat}{A runable example of FindNNDist, which is a list of distance matrixs for each study.}
#'   \item{testres}{A runable example of CalcuSCIR, which is a list of test results for each study.}
#' }
#' @return Simulation data to examplify the usage of the method.
#' @examples
#' # Load the data
#' data("sim_result")
#' @name sim_result
"sim_result"
