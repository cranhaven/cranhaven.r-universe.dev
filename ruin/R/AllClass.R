#-------------------------------------------------------------------------------
# CramerLundberg classes


#' A formal S4 class CramerLundberg
#'
#' A formal S4 class representation of classical Cramer-Lundberg model.
#'
#' The model is defined as follows:
#' \deqn{X(t) = u + ct - \sum_{i=1}^{N(t)} Y_i,}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N(t)} is the Poisson process with
#' intensity \eqn{\lambda} (\code{claim_poisson_arrival_rate}), \eqn{Y_i} are
#' iid claim sizes (\code{claim_size_generator} and \code{claim_size_parameters}
#' ).
#'
#' Objects of class can be created only by using the constructor
#' \code{\link{CramerLundberg}}.
#'
#' @slot initial_capital a length one numeric non-negative vector specifying an
#' initial capital.
#' @slot premium_rate a length one numeric non-negative vector specifying a
#' premium rate.
#' @slot claim_poisson_arrival_rate a length one numeric positive vector
#' specifying the rate of the Poisson process of claims' arrivals.
#' @slot claim_size_generator a function indicating the random generator of
#' claims' sizes.
#' @slot claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes.
#'
#' @seealso \code{\link{CramerLundberg}}
#'
#' @references
#' Albrecher H., Asmussen A. \emph{Ruin Probabilities}. World Scientific, 2010.
setClass(
  Class = "CramerLundberg",
  slots = list(
    initial_capital = "numeric",
    premium_rate = "numeric",
    claim_poisson_arrival_rate = "numeric",
    claim_size_generator = "function",
    claim_size_parameters = "list"
  )
)


#' A formal S4 class PathCramerLundberg
#'
#' A formal S4 class to contain a simulated path of
#' \linkS4class{CramerLundberg} model.
#'
#' Objects of the class must not be created manually. Instead, objects of this
#' class are returned by \code{\link{simulate_path}}.
#'
#' @slot model an object of \linkS4class{CramerLundberg} class.
#' @slot path a numeric matrix of columns \code{"time"} and \code{"X"} defining
#' the simulated path represented by pairs time-value.
#' @slot claim_sizes a numeric vector of claims' sizes.
#' @slot claim_arrival_times a numeric vector of claims' interarrival times.
#' @slot time_horizon a numeric vector of the maximum time horizon achieved.
#' @slot is_ruined a logical vector indicating whether the process is ruined.
#' @slot elapsed_time a numeric vector of the elapsed simulation time in
#' seconds.
#' @slot max_time_horizon a numeric vector of the maximum time horizon allowed.
#' @slot max_simulation_time a numeric vector of the maximum simulation time
#' in seconds.
#' @slot seed a numeric vector indicating the seed used for simulation.
#'
#'
#' @seealso \linkS4class{CramerLundberg} (class definition),
#' \code{\link{CramerLundberg}} (constructor).
setClass(
  Class = "PathCramerLundberg",
  slots = list(
    model = "CramerLundberg",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)


#-------------------------------------------------------------------------------
# CramerLundbergCapitalInjections classes

#' A formal S4 class CramerLundbergCapitalInjections
#'
#' A formal S4 class representation of Cramer-Lundberg's extension that
#' includes capital injections.
#'
#' The model is defined as follows:
#' \deqn{X_(t) = u + ct + \sum_{k=1}^{N^{(+)}(t)} Y^{(+)}_k -
#' \sum_{i=1}^{N^{(-)}(t)} Y^{(-)}_i}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N^{(+)}(t)} is the Poisson process
#' of positive jumps (capital injections) with intensity \eqn{\lambda^{(+)}}
#' (\code{capital_injection_poisson_rate}), \eqn{Y^{(+)}_k} are
#' iid capital injections' sizes (\code{capital_injection_size_generator}
#' and \code{capital_injection_size_parameters}), \eqn{N^{(-)}(t)} is the
#' Poisson process of negative jumps (claims) with intensity \eqn{\lambda^{(-)}}
#' (\code{claim_poisson_arrival_rate}), \eqn{Y^{(-)}_i} are iid claim sizes
#' (\code{claim_size_generator} and \code{claim_size_parameters}).
#'
#' Objects of class can be created only by using the constructor
#' \code{\link{CramerLundbergCapitalInjections}}.
#'
#' @slot initial_capital a length one numeric non-negative vector specifying an
#' initial capital.
#' @slot premium_rate a length one numeric non-negative vector specifying a
#' premium rate.
#' @slot claim_poisson_arrival_rate a length one numeric positive vector
#' specifying the rate of the Poisson process of claims' arrivals.
#' @slot claim_size_generator a function indicating the random generator of
#' claims' sizes.
#' @slot claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes.
#' @slot capital_injection_poisson_rate a length one numeric positive vector
#' specifying the rate of the Poisson process of capital injections' arrivals.
#' @slot capital_injection_size_generator a function indicating the random
#' generator of capital injections' sizes.
#' @slot capital_injection_size_parameters a named list containing parameters
#' for the random generator of capital injections' sizes.
#'
#' @seealso \code{\link{CramerLundbergCapitalInjections}}
#'
#' @references
#' Breuera L., Badescu A. L. \emph{A generalised Gerber Shiu measure for
#' Markov-additive risk processes with phase-type claims and capital
#' injections}. Scandinavian Actuarial Journal, 2014(2): 93-115, 2014.
setClass(
  Class = "CramerLundbergCapitalInjections",
  contains = "CramerLundberg",
  slots = list(
    capital_injection_poisson_rate = "numeric",
    capital_injection_size_generator = "function",
    capital_injection_size_parameters = "list"
  )
)


#' A formal S4 class PathCramerLundbergCapitalInjections
#'
#' A formal S4 class to contain a simulated path of
#' \linkS4class{CramerLundbergCapitalInjections} model.
#'
#' Objects of the class must not be created manually. Instead, objects of this
#' class are returned by \code{\link{simulate_path}}.
#'
#' @slot model an object of \linkS4class{CramerLundbergCapitalInjections} class.
#' @slot path a numeric matrix of columns \code{"time"} and \code{"X"} defining
#' the simulated path represented by pairs time-value.
#' @slot claim_sizes a numeric vector of claims' sizes.
#' @slot claim_arrival_times a numeric vector of claims' interarrival times.
#' @slot capital_injection_sizes a numeric vector of capital injections' sizes.
#' @slot capital_injection_arrival_times a numeric vector of capital injections'
#' interarrival times.
#' @slot time_horizon a numeric vector of the maximum time horizon achieved.
#' @slot is_ruined a logical vector indicating whether the process is ruined.
#' @slot elapsed_time a numeric vector of the elapsed simulation time in
#' seconds.
#' @slot max_time_horizon a numeric vector of the maximum time horizon allowed.
#' @slot max_simulation_time a numeric vector of the maximum simulation time
#' in seconds.
#' @slot seed a numeric vector indicating the seed used for simulation.
#'
#'
#' @seealso \linkS4class{CramerLundbergCapitalInjections} (class definition),
#' \code{\link{CramerLundbergCapitalInjections}} (constructor).
setClass(
  Class = "PathCramerLundbergCapitalInjections",
  slots = list(
    model = "CramerLundbergCapitalInjections",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    capital_injection_sizes = "numeric",
    capital_injection_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)


#-------------------------------------------------------------------------------
# SparreAndersen classes


#' A formal S4 class SparreAndersen
#'
#' A formal S4 class representation of classical Sparre Andersen model.
#'
#' The model is defined as follows:
#' \deqn{X(t) = u + ct - \sum_{i=1}^{N(t)} Y_i,}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N(t)} is the renewal process defined
#' by distribution of interarrival times (\code{claim_interarrival_generator}
#' and \code{claim_interarrival_parameters}), \eqn{Y_i} are iid claim sizes
#' (\code{claim_size_generator} and \code{claim_size_parameters}).
#'
#' Objects of class can be created only by using the constructor
#' \code{\link{SparreAndersen}}.
#'
#' @slot initial_capital a length one numeric non-negative vector specifying an
#' initial capital.
#' @slot premium_rate a length one numeric non-negative vector specifying a
#' premium rate.
#' @slot claim_interarrival_generator a function indicating the random
#' generator of claims' interarrival times.
#' @slot claim_interarrival_parameters a named list containing parameters for
#' the random generator of claims' interarrival times.
#' @slot claim_size_generator a function indicating the random generator of
#' claims' sizes.
#' @slot claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes.
#'
#' @seealso \code{\link{SparreAndersen}}
#'
#' @references \itemize{
#' \item Andersen, E. Sparre. \emph{On the collective theory of risk in case of
#' contagion between claims}. Transactions of the XVth International Congress
#' of Actuaries, 2(6), 1957.
#' \item Thorin O. \emph{Some Comments on the Sparre Andersen Model in the Risk
#' Theory}. ASTIN Bulletin: The Journal of the IAA, 8(1):104-125, 1974.
#' }
setClass(
  Class = "SparreAndersen",
  slots = list(
    initial_capital = "numeric",
    premium_rate = "numeric",
    claim_interarrival_generator = "function",
    claim_interarrival_parameters = "list",
    claim_size_generator = "function",
    claim_size_parameters = "list"
  )
)


#' A formal S4 class PathSparreAndersen
#'
#' A formal S4 class to contain a simulated path of
#' \linkS4class{SparreAndersen} model.
#'
#' Objects of the class must not be created manually. Instead, objects of this
#' class are returned by \code{\link{simulate_path}}.
#'
#' @slot model an object of \linkS4class{CramerLundberg} class.
#' @slot path a numeric matrix of columns \code{"time"} and \code{"X"} defining
#' the simulated path represented by pairs time-value.
#' @slot claim_sizes a numeric vector of claims' sizes.
#' @slot claim_arrival_times a numeric vector of claims' interarrival times.
#' @slot time_horizon a numeric vector of the maximum time horizon achieved.
#' @slot is_ruined a logical vector indicating whether the process is ruined.
#' @slot elapsed_time a numeric vector of the elapsed simulation time in
#' seconds.
#' @slot max_time_horizon a numeric vector of the maximum time horizon allowed.
#' @slot max_simulation_time a numeric vector of the maximum simulation time
#' in seconds.
#' @slot seed a numeric vector indicating the seed used for simulation.
#'
#'
#' @seealso \linkS4class{SparreAndersen} (class definition),
#' \code{\link{SparreAndersen}} (constructor).
setClass(
  Class = "PathSparreAndersen",
  slots = list(
    model = "SparreAndersen",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)


#-------------------------------------------------------------------------------
# SparreAndersenCapitalInjections classes


#' A formal S4 class SparreAndersenCapitalInjections
#'
#' A formal S4 class representation of Sparre Andersen's extension that
#' includes capital injections.
#'
#' The model is defined as follows:
#' \deqn{X_(t) = u + ct + \sum_{k=1}^{N^{(+)}(t)} Y^{(+)}_k -
#' \sum_{i=1}^{N^{(-)}(t)} Y^{(-)}_i}
#' where \eqn{u} is the initial capital (\code{initial_capital}), \eqn{c} is the
#' premium rate (\code{premium_rate}), \eqn{N^{(+)}(t)} is the renewal process
#' of positive jumps (capital injections) defined by distribution of
#' interarrival times (\code{capital_injection_interarrival_generator} and
#' \code{capital_injection_interarrival_parameters}), \eqn{Y^{(+)}_k} are iid
#' capital injections' sizes (\code{capital_injection_size_generator}
#' and \code{capital_injection_size_parameters}), \eqn{N^{(-)}(t)} is the
#' renewal process of claims defined by distribution of interarrival times
#' (\code{claim_interarrival_generator} and
#' \code{claim_interarrival_parameters}), \eqn{Y^{(-)}_i} are iid claim sizes
#' (\code{claim_size_generator} and \code{claim_size_parameters}).
#'
#' Objects of class can be created only by using the constructor
#' \code{\link{SparreAndersenCapitalInjections}}.
#'
#' @slot initial_capital a length one numeric non-negative vector specifying an
#' initial capital.
#' @slot premium_rate a length one numeric non-negative vector specifying a
#' premium rate.
#' @slot claim_interarrival_generator a function indicating the random
#' generator of claims' interarrival times.
#' @slot claim_interarrival_parameters a named list containing parameters for
#' the random generator of claims' interarrival times.
#' @slot claim_size_generator a function indicating the random generator of
#' claims' sizes.
#' @slot claim_size_parameters a named list containing parameters for the
#' random generator of claims' sizes.
#' @slot capital_injection_interarrival_generator a function indicating
#' the random generator of capital injections' interarrival times.
#' @slot capital_injection_interarrival_parameters a named list containing
#' parameters for the random generator of capital injections' interarrival
#' times.
#' @slot capital_injection_size_generator a function indicating the random
#' generator of capital injections' sizes.
#' @slot capital_injection_size_parameters a named list containing parameters
#' for the random generator of capital injections' sizes.
#'
#' @seealso \code{\link{SparreAndersenCapitalInjections}}
#'
#' @references
#' Breuera L., Badescu A. L. \emph{A generalised Gerber Shiu measure for
#' Markov-additive risk processes with phase-type claims and capital
#' injections}. Scandinavian Actuarial Journal, 2014(2): 93-115, 2014.
setClass(
  Class = "SparreAndersenCapitalInjections",
  contains = "SparreAndersen",
  slots = list(
    capital_injection_interarrival_generator = "function",
    capital_injection_interarrival_parameters = "list",
    capital_injection_size_generator = "function",
    capital_injection_size_parameters = "list"
  )
)


#' A formal S4 class PathSparreAndersenCapitalInjections
#'
#' A formal S4 class to contain a simulated path of
#' \linkS4class{SparreAndersenCapitalInjections} model.
#'
#' Objects of the class must not be created manually. Instead, objects of this
#' class are returned by \code{\link{simulate_path}}.
#'
#' @slot model an object of \linkS4class{SparreAndersenCapitalInjections} class.
#' @slot path a numeric matrix of columns \code{"time"} and \code{"X"} defining
#' the simulated path represented by pairs time-value.
#' @slot claim_sizes a numeric vector of claims' sizes.
#' @slot claim_arrival_times a numeric vector of claims' interarrival times.
#' @slot capital_injection_sizes a numeric vector of capital injections' sizes.
#' @slot capital_injection_arrival_times a numeric vector of capital injections'
#' interarrival times.
#' @slot time_horizon a numeric vector of the maximum time horizon achieved.
#' @slot is_ruined a logical vector indicating whether the process is ruined.
#' @slot elapsed_time a numeric vector of the elapsed simulation time in
#' seconds.
#' @slot max_time_horizon a numeric vector of the maximum time horizon allowed.
#' @slot max_simulation_time a numeric vector of the maximum simulation time
#' in seconds.
#' @slot seed a numeric vector indicating the seed used for simulation.
#'
#'
#' @seealso \linkS4class{SparreAndersenCapitalInjections} (class definition),
#' \code{\link{SparreAndersenCapitalInjections}} (constructor).
setClass(
  Class = "PathSparreAndersenCapitalInjections",
  slots = list(
    model = "SparreAndersenCapitalInjections",
    path = "matrix",
    claim_sizes = "numeric",
    claim_arrival_times = "numeric",
    capital_injection_sizes = "numeric",
    capital_injection_arrival_times = "numeric",
    time_horizon = "numeric",
    is_ruined = "logical",
    elapsed_time = "numeric",
    max_time_horizon = "numeric",
    max_simulation_time = "numeric",
    seed = "integer"
  )
)
