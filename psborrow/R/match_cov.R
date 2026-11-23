#' Match
#'
#' @param dt  a list of \code{matrix}
#' @param match A vector of covariates name to match on
#' @return a list of \code{matrix} containing matched cohort information
#'
#'
#' @examples
#' # match internal and external trial data using different covariates
#' smp = set_n(ssC = 140, ssE = 275, ssExt = 100)
#' covset1 = set_cov(n_cat = 2, n_cont = 0, mu_int = 0, mu_ext = 0, var = 1)
#' covset2 = set_cov(n_cat = 0, n_cont = 1, mu_int = 62, mu_ext = 65, var = 11)
#' cObj = c(covset1, covset2)
#' sample_cov <-
#'   simu_cov(ssObj = smp, covObj = cObj, HR = 1, driftHR = 1.2, nsim = 2)
#'
#' # match on covariates 1 and 2
#' match_cov(dt = sample_cov, match = c("cov1", "cov2"))
#'
#' # match on all 3 covariates
#' match_cov(dt = sample_cov, match = c("cov1", "cov2", "cov3"))
#'
#' @export
#' @keywords constructor
match_cov <- function(dt, match) {
  if (missing(dt)) stop("Please provide a list of simulated data (dt).")
  if (missing(match)) {
    stop("Please provide covariates name to match on.")
  } else if (sum(match %notin% colnames(dt[[1]])[colnames(dt[[1]]) %notin% c("driftHR", "HR", "trt", "ext")]) > 0) {
    stop("Please make sure the trial data contains the correct variable names.")
  }
  match.formula <- paste("int ~", paste0(match, collapse=' + '))
  # print(match.formula)
  res_list <- lapply(seq(1, length(dt), by = 1), function(i){
    m_cov(dt[[i]], match.formula)
  }) #loop foreach

  res_list
}

