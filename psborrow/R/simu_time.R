#' Simulate time-to-events for multiple scenarios
#'
#' @param dt a list of \code{matrix} generated in \code{\link{simu_cov}} containing simulated covariates information
#' @param eventObj an object of class \code{.eventClass} generated in \code{\link{set_event}} including event information
#' @param clinInt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including internal trial information
#' @param clinExt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including external trial information
#' @param seed the seed of R‘s random number generator. Default is the first element of .Random.seed
#' @param path file name for saving the output including folder path
#' @return a list of \code{matrix} containing simulated time-to-events information
#'
#'
#' @examples
#' # simulate patient-level data without covariates
#' # simulate survival time following weibull distribution
#'
#' # simulate trial indicator and set hazard ratios
#' sample = set_n(ssC = 10, ssE = 20, ssExt = 40)
#' sample_hr <- simu_cov(ssObj = sample, HR = 1, driftHR=c(1,1.2), nsim = 10)
#'
#' # enrollment pattern, drop-out, analysis start time
#' c_int = set_clin(gamma = 2, e_itv = 10, etaC = 0.5,  CCOD = "fixed-first", CCOD_t = 64)
#' c_ext = c_int
#'
#' # simulate time-to-event with a weibull distribution
#' evt1 <- set_event(event = "weibull", shape = 0.8, lambdaC = 0.01)
#' simu_time(dt = sample_hr, eventObj = evt1, clinInt = c_int, clinExt = c_ext)
#'
#' # simulate time-to-event with an exponential distribution
#' evt2 <- set_event(event = "pwexp", t_itv = 1, lambdaC = c(0.1, 0.02))
#' simu_time(dt = sample_hr, eventObj = evt2, clinInt = c_int, clinExt = c_int)
#'
#'
#' @export
#' @keywords simulator
simu_time = function(dt, eventObj, clinInt, clinExt, seed, path){

  if (missing(dt)) stop("Please provide a list of simulated data (dt).")
  if (missing(eventObj)) stop("Please provide eventObj.")
  if (missing(clinInt)) stop("Please provide clinInt.")
  if (missing(clinExt)) stop("Please provide clinExt.")
  if (missing(seed)){
    ps_message("Simulating survival time... Set seed to ",.Random.seed[1])
    seed = .Random.seed[1]
  } else set.seed(seed)

  seed_list <- seq(seed, seed + length(dt), by = 1)

  res_list <- lapply(seq(1, length(dt), by = 1), function(i){
    seed_i = seed_list[i]
    # print(paste("seed =", seed_i))
    add_time(dt = dt[[i]], eventObj = eventObj, clinInt = clinInt, clinExt = clinExt, seed = seed_i)
  }) #loop foreach

  if (missing(path)) ps_message("Simulated time are not saved.") else {
    save(res_list, file = path)
    ps_message("Simulated time are saved as", path)
  }
  res_list
}

