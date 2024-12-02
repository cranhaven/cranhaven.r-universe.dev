#' Multi-Stratum Two-Phase Local Search (MS-TPLS) Algorithm
#'
#' @description This function implements the \emph{Multi-Stratum Two-Phase Local
#' Search} (MS-TPLS) algorithm described in Borrotti, Sambo, Mylona and Gilmour
#' (2016). This algorithm is useful to obtain exact optimal multi-stratum
#' designs through a multi-criteria approach. When using runTPLS the user must
#' establish the search problem (structure of the experiment, number of trials,
#' optimization criteria, etc.) and the total number of iterations of MS-TPLS.
#' The resulting experimental designs can minimize up to six criteria simultaneously
#' from the following: "I", "Id", "D", "Ds", "A" and "As". \code{runTPLS} is able
#' to provide the set of solutions building the approximate Pareto front for
#' the specified optimization problem.
#'
#' @usage runTPLS(facts, units, criteria, model, iters, ...)
#'
#' @param facts A list of vectors representing the distribution of factors
#' across strata. Each item in the list represents a stratum and the first item
#' is the highest stratum of the multi-stratum structure of the experiment.
#' Within the vectors, experimental factors are indicated by progressive integer
#' from 1 (the first factor of the highest stratum) to the total number of
#' experimental factors (the last factor of the lowest stratum). Blocking
#' factors are differently denoted by empty vectors.
#'
#' @param units A list whose \eqn{i}-th element, \eqn{n_i}, is the number of
#' experimental units within each unit at the previous stratum (\eqn{i-1}). The
#' first item in the list, \eqn{n_1}, represents the number of experimental
#' units in the stratum \eqn{0}. The latter is defined as the entire experiment,
#' such that \eqn{n_0 = 1}{n_0 = 1}.
#'
#' @param criteria A list specifying the criteria to be optimized. It can
#' contain any combination of:
#' \itemize{
#'   \item{``I" : I-optimality}
#'   \item{``Id" : Id-optimality}
#'   \item{``D" : D-optimality}
#'   \item{``A" : Ds-optimality}
#'   \item{``Ds" : A-optimality}
#'   \item{``As" : As-optimality}
#' }
#' More detailed information on the available criteria is given in
#' \code{\link[multiDoE]{MSOpt}.}
#'
#' @param model A string which indicates the type of model, among ``main",
#' ``interaction" and ``quadratic".
#'
#' @param iters An integer indicating the number of iterations of the MS-TPLS
#' algorithm.
#'
#' @param ... optional arguments (see below).
#'
#' @details Additional arguments can be specified as follows:
#' \itemize{
#' \item \code{'Restarts', restarts}: A string and an integer, used in pair. \code{r}
#' defines the number of times the MS-Opt procedure is altogether called within
#' each iteration of the MS-TPLS algorithm. The default value is \code{r=100}.
#'
#' \item \code{'Levels', levels}: A string and a vector, used in pair. \code{levels}
#' is a vector containing the number of available levels for each experimental
#' factor in the argument \code{facts} (blocking factors are excluded). If all
#' experimental factors share the number of levels one integer is sufficient.
#'
#' \item \code{'Etas', etas}: A string and a list, used in pair. In \code{etas}
#' the user must specify the ratios of error variance between subsequent strata,
#' starting from the highest strata. It follows that \code{length(etas)} must be
#' equal to \code{length(facts)-1}.
#'
#' \item \code{'RestInit', restInit}: A string and an integer, used in pair. Through
#' these parameters, it is possible to determine how many of the \code{r} iterations
#' of MS-Opt should be used for each criterion in the first step of the MS-TPLS
#' algorithm (lines 3-6 of the pseudo-code of MS-TPLS, see Borrotti, Sambo, Mylona
#' and Gilmour (2017)). The default value is \code{restInit=50}. Let \eqn{n} be
#' the number of criteria under consideration. One can calculate accordingly as
#' \eqn{r - (n * restInit)} the number of times MS-Opt is called in the
#' second step (lines 7-11 of the pseudo-code of MS-TPLS) of each iteration of MS-TPLS.
#'
#' \item \code{'RngSeed', rngSeed}: A number indicating the seed for reproducibility.
#' Default is to leave the random number generator alone.}
#'
#' @return \code{runTPLS} returns a list, whose elements are:
#' \itemize{
#' \item \code{ar}: A list of length equal to \code{iters}. The \eqn{i}-th element
#' is a list whose elements are:
#'    \itemize{
#'    \item \code{nsols}: Number of designs produced during the \eqn{i}-th iteration.
#'    \item \code{dim}: The criteria space dimension.
#'    \item \code{scores}: A matrix of \code{nsols} rows and \code{dim} columns.
#'    Every row contains the value of the criteria for each solution of the
#'    \eqn{i}-th iteration.
#'    \item \code{solutions}: A list of length equal to \code{nsols} containing the
#'    design matrices produced during the \eqn{i}-th iteration. The values of the
#'    criteria corresponding at the first element of \code{solutions} are placed
#'    in the first row of the \code{scores} matrix and so on.
#'    }
#' \item \code{stats}: A list of length equal to \code{iters}. Every element is a
#' vector of size \eqn{r - (n * restInit) + 1}, where \eqn{n} is the number of the
#' considered criteria. The first element represents the number of function
#' evaluations during the first step of the MS-TPLS algorithm; the \eqn{i}-th
#' element (excluding the first one) is the sum of the number of evaluations for
#' the \eqn{i}-th scalarization and the maximum value in the \code{stats}.
#'
#' \item \code{megaAR}: A list whose elements are:
#'    \itemize{
#'    \item \code{nsols}: The number of the Pareto front solutions.
#'    \item \code{dim}: The criteria space dimension.
#'    \item \code{scores}: A matrix of \code{nsols} rows and \code{dim} columns.
#'     Every row contains the criteria values for each Pareto front design.
#'    \item \code{solutions}: A list of length equal to \code{nsols} containing
#'    the design matrices for the Pareto front designs. The values of the criteria
#'    corresponding at the first element of \code{solutions} are placed in the
#'    first row of the \code{scores} matrix and so on.
#'    }
#' }
#'
#'
#' @references
#' M. Borrotti and F. Sambo and K. Mylona and S. Gilmour. A multi-objective
#' coordinate-exchange two-phase local search algorithm for multi-stratum
#' experiments. Statistics & Computing, 2017.
#'
#'
#' @export

runTPLS <- function(facts, units, criteria, model, iters, ...) {

  varargin <- list(...)

  if ("RngSeed" %in% varargin == F) {
    varargin <- append(varargin, "RngSeed")
    varargin <- append(varargin, 0)
  }
  # optional parameters
  if (length(varargin) != 0) {
    for (i in seq(1, length(varargin), 2)) {
      switch(varargin[[i]],
             "Restarts" = {
               restarts = varargin[[i+1]]
             },
             "Levels" = {
               levels = varargin[[i+1]]
             },
             "Etas" = {
               etas = varargin[[i+1]]
             },
             "RngSeed" = {
               rngSeed = varargin[[i+1]]
             },
             "RestInit" = {
               restInit = varargin[[i+1]]
             }
             # "Interactive" = {
             #   interact = varargin[[i+1]]
             # }
      )
    }
  }

  ar <- vector(mode = "list", iters)
  stats <- vector(mode = "list", iters)

  for (i in 1:iters) {
    print(i)
    tpls <- TPLSearch(facts, units, criteria, model, varargin)
    ar[[i]] <- tpls$ar
    stats[[i]] <- tpls$stats
    varargin[which(varargin == "RngSeed") + 1][[1]] <- varargin[which(varargin == "RngSeed") + 1][[1]] + 1
  }

  lCrit <- length(criteria)
  megaAR <- Archive(lCrit, iters * (restarts - lCrit  * (restInit - 1)))

  for (i in 1:iters) {
    for (j in 1: ar[[i]][["nsols"]]) {
      megaAR <- Add(megaAR, ar[[i]][["solutions"]][[j]], ar[[i]][["scores"]][j, ])
    }
  }

  megaAR <- RemoveDuplicates(megaAR)
  megaAR <- RemoveDominated(megaAR)
  colnames(megaAR$scores) <- criteria
  out <- list("ar" = ar, "stats" = stats, "megaAR" = megaAR)
  class(out) <- c("runTPLS", "list")
  return(out)
}


