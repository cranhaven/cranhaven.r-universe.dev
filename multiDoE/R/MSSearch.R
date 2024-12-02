#### MSSearch ####
#' Local search algorithm for high quality design generation
#'
#' @description The \code{MSSearch} function can be used to obtain an optimal
#' multi-stratum experimental design considering one or more optimality criteria,
#' up to a maximum of six criteria simultaneously. \cr
#' This function implements the procedure MS-Opt proposed by Sambo, Borrotti,
#' Mylona e Gilmour (2016) as an extension of the Coordinate-Exchange (CE)
#' algorithm for constructing approximately optimal designs. This innovative
#' procedure is able to handle all possible multi-stratum experimental structures
#' and, instead of minimizing a single objective function as in the original CE
#' algorithm, it seeks to minimize the following scalarization of the objective
#' functions for all considered criteria:
#' \deqn{f_W = \sum_{c \in C}{\alpha_c f_c(d; \eta)=\overline{\alpha} \cdot \overline{f}},}{%
#' f_W = \sum{c in C}(\alpha_c*f_c(d;\eta)) = \overline{\alpha} \overline{f},}
#' with
#' \deqn{\sum_{c \in C} \alpha_c = 1,}{\sum{c in C}(\alpha_c) = 1,}
#' where \eqn{C} is the set of criteria to be minimized, \eqn{f_c} is the
#' objective function for the \eqn{c} criterion and \eqn{\overline{\alpha}} is
#' the vector that controls the relative weights of the objective functions.
#'
#' @usage MSSearch(msopt, alpha, ...)
#'
#' @param msopt A list as returned by the \code{\link[multiDoE]{MSOpt}} function.
#' @param alpha A vector of weights, whose elements must sum to one.
#' \code{length(alpha)} must be equal to the number of criteria considered, that
#' is it must be equal to the length of the \code{criteria} element of \code{msopt}.
#' @param ... optional arguments (see \strong{Details}).
#'
#' @details \code{MSSearch} by default does not apply any normalization to the
#' individual objective functions \eqn{f_c} before the calculation of f_w is
#' performed. However, it is possible to subject the vector of objective functions
#' \eqn{\overline{f}} to the following transformation:
#' \deqn{\overline{f}_{norm} = \frac{\overline{f} - CritTR}{CritSC},}{%
#' \overline{f}_norm = (\overline{f} - CritTR) / CritSC,}
#' by specifying \eqn{CritTR} and \eqn{CritSC} vectors as additional parameters,
#' as described below.
#'
#' Additional arguments can be specified as follows:
#' \itemize{
#' \item \code{'Start', sol}: A string and a matrix, used in pair. They provide
#' a starting solution (or an initial design) to the algorithm. By default the
#' initial solution is randomly generated following the SampleDesign()
#' procedure described in Sambo, Borrotti, Mylona and Gilmour (2016).
#'
#' \item \code{'Restarts', r }: A string and an integer, used in pair. When
#' \code{r=1}, the default value, the procedure implemented in \code{MSSearch}
#' results in a local search algorithm that optimizes the objective function
#' \eqn{f_W} starting from one initial design in the design space. These
#' parameters allows to restart the algorithm \code{r} times. If no initial design
#' is passed a different starting solution is generated for each iteration, letting
#' the probability to find a global minimum be higher. \code{Mssearch} returns
#' the solution that minimizes \eqn{f_W} across all the \code{r} iterations.
#'
#' \item \code{'Normalize', c(CritTR, CritSC)}: A string and a vector, used in
#' pair. By specifying the \code{CritTR} and \code{CritSC} vectors, the user can
#' establish the normalization factors to be applied to each objective function
#' before evaluating \eqn{f_W}. \code{CritTR} and \code{CritSC} are vectors of
#' length equal to the number of criteria, whose default elements are 0 and 1
#' respectively.
#' }
#'
#' @return \code{MSSearch} returns a list, whose elements are:
#' \itemize{
#' \item{\code{optsol}: A design matrix. The best solution found.}
#' \item{\code{optscore}: A vector containing the criteria scores for
#' \code{optsol}.}
#' \item{\code{feval}: An integer representing the number of score function
#' evaluations (number of \eqn{f_W} evaluations over all iterations).}
#' \item{\code{trend}: A vector of length \code{r}. The \eqn{i}-th element is
#' the value that minimizes \eqn{f_W} for the \eqn{i}-th iteration.}
#' }
#'
#' @references
#' M. Borrotti and F. Sambo and K. Mylona and S. Gilmour. A multi-objective
#' coordinate-exchange two-phase local search algorithm for multi-stratum
#' experiments. Statistics & Computing, 2016.
#'
#' @examples
#' library(multiDoE)
#'
#' ## To check the number of digits to be printed.
#' backup_options <- options()
#' options(digits = 10)
#'
#' ## Definition of parameters for experimental setup
#' facts <- list(1, 2:5)
#' units <- list(21, 2)
#' level <- 3
#' etas <- list(1)
#' model2 <- "quadratic"
#'
#' ## Single-objective optimization
#' criteria_S <- c('I')
#' msopt_S <- MSOpt(facts, units, level, etas, criteria_S, model2)
#' \donttest{
#' mssearch_S <- MSSearch(msopt_S, alpha = 1, "Restarts", 100)
#' }
#'
#' ## Multi-objective optimization
#' criteria_M <- c('Id', 'Ds', 'As')
#' msopt_M <- MSOpt(facts, units, level, etas, criteria_M, model2)
#' \donttest{
#' mssearch_M <- MSSearch(msopt_M, alpha = c(1/2, 1/4, 1/4), "Restarts", 100)
#' }
#'
#' options(backup_options)
#' 
#' ## To reduce the computational cost of MSSearch function, you may reduce the number of restarts.
#'
#' @export
MSSearch <- function(msopt, alpha, ...) {
  varargin <- list(...)

  # default parameters
  restarts <- 1
  norms <- t(c(integer(msopt$ncrit), rep(1, msopt$ncrit)))
  random_start <- 1
  sol <- matrix(0, msopt$runs, msopt$nfacts)

  # optional parameters
  if (nargs() > 3) {
    for (i in seq(1, nargs() - 3, 2)) {
      switch (varargin[[i]],
              "Start" = {
                sol <- varargin[[i + 1]]
                random_start <- 0;
              },
              "Normalize" = {
                norms <- t(varargin[[i + 1]])
              },
              "Restarts" = {
                restarts = varargin[[i + 1]]
              }
      )
    }
  }

  CritTR <- norms[1:msopt$ncrit]
  CritSC <- norms[(msopt$ncrit + 1):length(norms)]

  # useful for working stratum by stratum
  totUnits <- t(numeric(msopt$ncrit))
  sizUnits <- totUnits

  for (s in 1:(msopt$nstrat-1)) {
    totUnits[s] <- prod(unlist(msopt$units[1:s]))
    sizUnits[s] <- prod(unlist(msopt$units[(s + 1):length(msopt$units)]))
  }
  totUnits[msopt$nstrat] <- prod(unlist(msopt$units[1:msopt$nstrat]))
  sizUnits[msopt$nstrat] <- 1

  optsc <- matrix(Inf, 1, msopt$ncrit)
  wopt <- Inf
  optsol <- matrix(0, msopt$runs, msopt$nfacts)
  feval <- 0
  trend <- numeric(restarts)

  for (t in 1:restarts) {
    if (random_start) {       # generate initial random solution

      # sample for each stratum
      for (s in 1:msopt$nstrat) {
        for (i in 1:totUnits[s]) {
          for (j in msopt$facts[[s]]) {
            sol[(sizUnits[s]*(i - 1) + 1):(sizUnits[s]*i), j] <-
              msopt$avlev[[j]][sample(1:msopt$levs[j], 1)]
          }
        }
      }
    } # if

    # score initial solution
    score <- Score(msopt, sol)
    score <- score[["score"]]
    wscore <- as.numeric(as.vector(alpha) %*% as.vector((score - CritTR) / CritSC))

    if (is.nan(wscore) | is.na(wscore)){
      wscore = Inf
    }
    feval <- feval + 1

    # Try to improve
    improvement <- 1
    sol2 <- sol

    while (improvement == 1) {
      improvement <- 0

      # Vary each factor in each stratum
      for (s in 1:msopt$nstrat) {
        for (i in 1: totUnits[s]) {
          for (j in msopt$facts[[s]]) {
            for (f in 1:length(msopt$avlev[[j]])) {

              if (sol[i * sizUnits[s], j] != msopt$avlev[[j]][f]) {
                sol2[(sizUnits[s]*(i - 1) + 1):(i * sizUnits[s]), j] <- msopt$avlev[[j]][f]
                score2 <- Score(msopt, sol2)
                score2 <- score2[["score"]]
                wscore2 <- as.numeric(as.vector(alpha) %*% as.vector((score2 - CritTR) / CritSC))
                feval <- feval + 1
                if ((!is.nan(wscore2)) && (!is.na(wscore2)) && (wscore2 < wscore)){
                  sol <- sol2
                  wscore <- wscore2
                  improvement <- 1
                } else {
                  sol2 <- sol
                }
              }
            }
          }
        }
      }
    } # while

    if (wscore < wopt) {
      optsol <- sol
      optsc <- Score(msopt, sol)
      wopt <- wscore
    }

    trend[t] <- wopt
  } # for t
  out <- list("optsol" = optsol, "optsc" = optsc, "feval" = feval, "trend" = trend)
  class(out) <- c("MSSearch", "list")
  return(out)
}



#### print.MSSearch ####
#' @export
#'
print.MSSearch <- function(x, ...) {
  cat("MSSearch object")
  return(invisible(NULL))
}

#### summary.MSSearch ####
#' @export
#'
summary.MSSearch <- function(object, ...) {
  cat("nfacts: ", object$nfacts, "\n")
  cat("For more info ?MSOpt")
  return(invisible(NULL))
}








