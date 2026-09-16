#' Run end-to-end MDFS
#'
#' @details
#' In case of FDR control it is recommended to use Benjamini-Hochberg-Yekutieli p-value adjustment
#' method (\code{"BY"} in \code{\link[stats]{p.adjust}}) due to unknown dependencies between tests.
#'
#' @param data input data where columns are variables and rows are observations (all numeric)
#' @param decision decision variable as a boolean vector of length equal to number of observations
#' @param n.contrast number of constrast variables (defaults to max of 1/10 of variables number and 30)
#' @param dimensions number of dimensions (a positive integer; on CUDA limited to 2--5 range)
#' @param divisions number of divisions (from 1 to 15)
#' @param discretizations number of discretizations
#' @param range discretization range (from 0.0 to 1.0; \code{NULL} selects probable optimal number)
#' @param pc.xi parameter xi used to compute pseudocounts (the default is recommended not to be changed)
#' @param p.adjust.method method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"} is recommended for FDR, see Details)
#' @param level statistical significance level
#' @param seed seed for PRNG used during discretizations (\code{NULL} for random)
#' @param use.CUDA whether to use CUDA acceleration (must be compiled with CUDA; NOTE: the CUDA version might provide a slightly lower sensitivity due to a lack of native support for \code{contrast_data})
#' @return A \code{\link{list}} with the following fields:
#'  \itemize{
#'    \item \code{contrast.indices} -- indices of variables chosen to build contrast variables
#'    \item \code{contrast.variables} -- built contrast variables
#'    \item \code{MIG.Result} -- result of ComputeMaxInfoGains
#'    \item \code{MDFS} -- result of ComputePValue (the MDFS object)
#'    \item \code{statistic} -- vector of statistic's values (IGs) for corresponding variables
#'    \item \code{p.value} -- vector of p-values for corresponding variables
#'    \item \code{adjusted.p.value} -- vector of adjusted p-values for corresponding variables
#'    \item \code{relevant.variables} -- vector of relevant variables indices
#'  }
#' @examples
#' \donttest{
#' MDFS(madelon$data, madelon$decision, dimensions = 2, divisions = 1,
#'      range = 0, seed = 0)
#' }
#' @importFrom stats p.adjust
#' @export
MDFS <- function(
  data,
  decision,
  n.contrast = max(ncol(data), 30),
  dimensions = 1,
  divisions = 1,
  discretizations = 1,
  range = NULL,
  pc.xi = 0.25,
  p.adjust.method = "holm",
  level = 0.05,
  seed = NULL,
  use.CUDA = FALSE
 ) {
 if(!is.null(seed)) {set.seed(seed)}
 if (n.contrast>0) {
  contrast <- GenContrastVariables(data, n.contrast)
  contrast.indices <- contrast$indices
  contrast_data <- contrast$contrast_data
  contrast.mask <- c(rep.int(F, ncol(data)), rep.int(T, ncol(contrast_data)))
 } else {
  contrast.mask <- contrast.indices <- contrast_data <- NULL
 }

 # FIXME: fix the CUDA version to support contrast_data
 if (use.CUDA) {
  MIG.Result <- ComputeMaxInfoGains(data, decision,
    contrast_data = NULL,
    dimensions = dimensions, divisions = divisions,
    discretizations = discretizations, range = range, pc.xi = pc.xi,
    seed = seed, return.tuples = !use.CUDA && dimensions > 1, use.CUDA = use.CUDA)
  MIG.Result_with_contrast <- ComputeMaxInfoGains(cbind(data, contrast_data), decision,
    contrast_data = NULL,
    dimensions = dimensions, divisions = divisions,
    discretizations = discretizations, range = range, pc.xi = pc.xi,
    seed = seed, return.tuples = !use.CUDA && dimensions > 1, use.CUDA = use.CUDA)
  attr(MIG.Result, "contrast_igs") <- MIG.Result_with_contrast$IG[contrast.mask]
 } else {
  MIG.Result <- ComputeMaxInfoGains(data, decision,
    contrast_data = contrast_data,
    dimensions = dimensions, divisions = divisions,
    discretizations = discretizations, range = range, pc.xi = pc.xi,
    seed = seed, return.tuples = !use.CUDA && dimensions > 1, use.CUDA = use.CUDA)
 }
 igs <- c(MIG.Result$IG, attr(MIG.Result, "contrast_igs"))

 fs <- ComputePValue(igs,
  dimensions = dimensions, divisions = divisions,
  contrast.mask = contrast.mask,
  one.dim.mode = ifelse (discretizations==1, "raw", ifelse(divisions*discretizations<12, "lin", "exp")))

 statistic <- MIG.Result$IG
 p.value <- fs$p.value[seq_len(ncol(data))]
 adjusted.p.value <- p.adjust(p.value, method=p.adjust.method)
 relevant.variables <- which(adjusted.p.value<level)

 return(list(
  contrast.indices = contrast.indices,
  contrast.variables = contrast_data,
  MIG.Result = MIG.Result,
  MDFS = fs,
  statistic = statistic,
  p.value = p.value,
  adjusted.p.value = adjusted.p.value,
  relevant.variables = relevant.variables
  )
 )
}
