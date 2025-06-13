#' Generic for simulating a path of a given risk model
#'
#' \code{simulate_path()} simulates a path of a given risk model until one of
#' the following conditions is met: (1) the process is ruined, (2)
#' \code{max_time_horizon} is achieved, (3) the elapsed time of the simulation
#' is greater than \code{max_simulation_time}.
#'
#' @param model an S4 object indicating a risk model (e.g.,
#' \linkS4class{CramerLundberg}).
#' @param max_time_horizon a length one numeric vector specifying the maximum
#' time horizon, until with the process will be simulated. Default: \code{Inf}.
#' @param max_simulation_time a length one numeric vector indicating the maximum
#' allowed time of simulation. The value should be specified in seconds.
#' Default: \code{Inf}.
#' @param seed an optional arbitrary length numeric vector specifying the seed.
#' If provided, the \code{.Random.seed} in \code{.GlobalEnv} is set to its
#' value.
#'
#' @return An S4 corresponding to \code{model} class object. For instance, for
#' \linkS4class{CramerLundberg}, the object of class
#' \linkS4class{PathCramerLundberg} is returned.
#'
#' @section Warning:
#' Setting both \code{max_time_horizon} and \code{max_simulation_time} to
#' \code{Inf} might be dangerous. In this case, the only stopping condition is a
#' ruin of the process, which might not happen.
#'
#' @examples
#' model <- CramerLundberg(initial_capital = 10,
#'                         premium_rate = 1,
#'                         claim_poisson_arrival_rate = 1,
#'                         claim_size_generator = rexp,
#'                         claim_size_parameters = list(rate = 1))
#'
#' path <- simulate_path(model = model, max_time_horizon = 10)
#'
#' @export
setGeneric(
  name = "simulate_path",
  def = function(model,
                 max_time_horizon = NULL,
                 max_simulation_time = NULL,
                 seed = NULL) {
    standardGeneric("simulate_path")
  },
  signature = c("model")

)
