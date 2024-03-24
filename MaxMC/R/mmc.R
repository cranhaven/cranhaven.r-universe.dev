#' @title Maximized Monte Carlo
#' @docType package
#' @name MaxMC-package
#' @author Julien Neves, jmn252@cornell.edu (Maintainer)
#' @author Jean-Marie Dufour, jean-marie.dufour@mcgill.ca
#'
#' @description Functions that implement the Maximized Monte Carlo technique based on
#' Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters:
#' A general approach to finite sample inference and nonstandard asymptotics in econometrics.
#' \emph{Journal of Econometrics}, \bold{133(2)}, 443-447.
#'
#' The main functions of \pkg{MaxMC} are \code{\link{mmc}} and \code{\link{mc}}.
#'
#' @references Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters:
#' A general approach to finite sample inference and nonstandard asymptotics in econometrics.
#' \emph{Journal of Econometrics}, \bold{133(2)}, 443-447.
#'
#' @references Dufour, J.-M. and Khalaf L. (2003), Monte Carlo Test Methods in Econometrics.
#' in Badi H. Baltagi, ed., \emph{A Companion to Theoretical Econometrics}, Blackwell Publishing Ltd, 494-519.
#'
NA


#'
#' Find the Maximized Monte Carlo (MMC) p-value on a set of nuisance
#' parameters.
#'
#' The \code{dgp} function defined by the user is used to
#' generate new observations in order to compute the simulated
#'  statistics.
#'
#' Then \code{\link{pvalue}} is applied to the statistic and
#' its simulated values.\code{\link{pvalue}} computes the
#' p-value by ranking the statistic compared to its simulated
#' values. Ties in the ranking are broken according to a
#' uniform distribution.
#'
#' We allow for four types of p-value: \code{leq}, \code{geq},
#' \code{absolute} and \code{two-tailed}. For one-tailed test,
#' \code{leq} returns the proportion of simulated values smaller
#' than the statistic while \code{geq} returns the proportion of
#' simulated values greater than the statistic. For two-tailed
#' test with a symmetric statistic, one can use the
#' absolute value of the statistic and its simulated values to
#' retrieve a two-tailed test (i.e. type = \code{absolute}).
#' If the statistic is not symmetric, one can specify the p-value
#' type as \code{two-tailed} which is equivalent to twice the minimum
#' of \code{leq} and \code{geq}.
#'
#' Ties in the ranking are broken according to a uniform
#' distribution.
#'
#' Usually, to ensure that the MMC procedure is exact, \code{lower} and
#' \code{upper} must be set such that any theoretically possible
#' values for the nuisance parameters under the null are covered. This
#' can be computationally expansive.
#'
#' Alternatively, the consistent set estimate MMC method (CSEMMC)
#'  which is applicable when a consistent set estimator of the nuisance
#'  parameters is available can be used. If such set is available, by setting
#'  \code{lower} and \code{upper} accordingly, \code{mmc} will yield
#'  an asymptotically justified version of the MMC procedure.
#'
#'  One version of this procedure is the Two-stage constrained maximized
#'  Monte Carlo test, where first a confidence set of level
#'  \eqn{1-\alpha_1} for the nuisance parameters is obtained and then the MMC with
#'  confidence level \eqn{\alpha_2} is taken over this particular set.
#'  This procedure yields a conservative test with level
#'  \eqn{\alpha=\alpha_1+\alpha_2}. Note that we generally advise
#'  against using asymptotic Wald-type confidence intervals based on
#'  their poor performance. Instead, it is simply best to build
#'  confidence set using problem-specific tools.
#'
#'
#' @section Controls:
#'\subsection{Controls - \code{\link[GenSA]{GenSA}}}{
#' \describe{
#'   \item{maxit}{Integer. Maximum number of iterations of
#'   the algorithm. Defaults to
#'   1000.}
#'   \item{nb.stop.improvement}{Integer. The program will
#'   stop when there is no any improvement in
#'   nb.stop.improvement steps. Defaults to 25}
#'   \item{smooth}{Logical.TRUE when the objective function
#'   is smooth, or differentiable almost everywhere in the
#'   region of par, FALSE otherwise. Default value is TRUE.}
#'   \item{max.call}{Integer. Maximum number of call of the
#'   objective function. Default is set to 1e7.}
#'   \item{max.time}{Numeric. Maximum running time in seconds.}
#'   \item{temperature}{Numeric. Initial value for temperature.}
#'   \item{visiting.param}{Numeric. Parameter for visiting
#'   distribution.}
#'   \item{acceptance.param}{Numeric. Parameter for acceptance
#'    distribution.}
#'   \item{simple.function}{Logical. FALSE means that the
#'   objective function has only a few local minima. Default
#'   is FALSE which means that the objective function is
#'   complicated with many local minima.}
#'   }
#'}
#' \subsection{Controls - \code{\link[pso]{psoptim}}}{
#' \describe{
#'  \item{maxit}{The maximum number of iterations. Defaults to
#'   1000.}
#'  \item{maxf}{The maximum number of function evaluations
#'  (not considering any performed during numerical gradient
#'  computation). Defaults to Inf.}
#'  \item{reltol}{The tolerance for restarting. Once the
#'  maximal distance between the best particle and all other
#'   particles is less than reltol*d the algorithm restarts.
#'    Defaults to 0 which disables the check for restarting.}
#'  \item{s}{The swarm size. Defaults to
#'  floor(10+2*sqrt(length(par))) unless type is "SPSO2011"
#'  in which case the default is 40.}
#'  \item{k}{The exponent for calculating number of informants.
#'  Defaults to 3.}
#'  \item{p}{The average percentage of informants for each
#'  particle. A value of 1 implies that all particles are
#'  fully informed. Defaults to 1-(1-1/s)^k.}
#'  \item{w}{The exploitation constant. A vector of length 1
#'  or 2. If the length is two, the actual constant used is
#'  gradially changed from w[1] to w[2] as the number of
#'  iterations or function evaluations approach the limit
#'  provided. Defaults to 1/(2*log(2)).}
#'  \item{c.p}{The local exploration constant. Defaults to
#'  .5+log(2).}
#'  \item{c.g}{The global exploration constant. Defaults to
#'  .5+log(2).}
#'  \item{d}{The diameter of the search space. Defaults to the
#'  euclidean distance between upper and lower.}
#'  \item{v.max}{The maximal (euclidean) length of the velocity
#'  vector. Defaults to NA which disables clamping of the
#'  velocity. However, if specified the actual clamping of the
#'  length is v.max*d.}
#'  \item{rand.order}{Logical; if TRUE the particles are
#'  processed in random order. If vectorize is TRUE then the
#'  value of rand.order does not matter. Defaults to TRUE.}
#'  \item{max.restart}{The maximum number of restarts.
#'  Defaults to Inf.}
#'  \item{maxit.stagnate}{The maximum number of iterations
#'  without improvement. Defaults to 25}
#'  \item{vectorize}{Logical; if TRUE the particles are
#'  processed in a vectorized manner. This reduces the
#'  overhead associated with iterating over each particle and
#'  may be more time efficient for cheap function evaluations.
#'  Defaults to TRUE.}
#'  \item{type}{Character vector which describes which
#'  reference implementation of SPSO is followed. Can take
#'  the value of "SPSO2007" or "SPSO2011". Defaults to
#'  "SPSO2007".}
#'}
#'}
#' \subsection{Controls - \code{\link[GA]{GA}}}{
#' \describe{
#' \item{popSize}{the population size.}
#' \item{pcrossover}{the probability of crossover between
#' pairs of chromosomes. Typically this is a large value and
#' by default is set to 0.8.}
#' \item{pmutation}{the probability of mutation in a parent
#' chromosome. Usually mutation occurs with a small
#' probability, and by default is set to 0.1.}
#' \item{updatePop}{a logical defaulting to FALSE. If set at
#' TRUE the first attribute attached to the value returned by
#' the user-defined fitness function is used to update the
#' population. Be careful though, this is an experimental
#' feature!}
#' \item{postFitness}{a user-defined function which, if
#' provided, receives the current ga-class object as input,
#' performs post fitness-evaluation steps, then returns an
#' updated version of the object which is used to update the
#' GA search. Be careful though, this is an experimental
#' feature!}
#' \item{maxiter}{the maximum number of iterations to run
#' before the GA search is halted.}
#' \item{run}{the number of consecutive generations without
#' any improvement in the best fitness value before the GA
#' is stopped.}
#' \item{optim}{a logical defaulting to FALSE determining
#' whether or not a local search using general-purpose
#' optimisation algorithms should be used. See argument
#' optimArgs for further details and finer control.}
#' \item{optimArgs}{a list controlling the local search
#' algorithm with the following components:
#' \describe{
#' \item{method}{a string specifying the general-purpose
#' optimisation method to be used, by default is set to
#' "L-BFGS-B". Other possible methods are those reported
#' in \code{\link{optim}}.}
#' \item{poptim}{a value in the range [0,1] specifying the
#' probability of performing a local search at each
#' iteration of GA (default 0.1).}
#' \item{pressel}{a value in the range [0,1] specifying the
#' pressure selection (default 0.5). The local search is
#' started from a random solution selected with probability
#' proportional to fitness. High values of pressel tend to
#' select the solutions with the largest fitness, whereas
#' low values of pressel assign quasi-uniform probabilities
#' to any solution.}
#' \item{control}{a list of control parameters. See 'Details'
#' section in \code{\link{optim}}.}}
#' }
#' \item{keepBest}{a logical argument specifying if best
#' solutions at each iteration should be saved in a slot
#' called bestSol. See ga-class.}
#' \item{parallel}{a logical argument specifying if parallel
#' computing should be used (TRUE) or not (FALSE, default)
#' for evaluating the fitness function. This argument could
#' also be used to specify the number of cores to employ; by
#' default, this is taken from detectCores. Finally, the
#' functionality of parallelization depends on system OS:
#' on Windows only 'snow' type functionality is available,
#' while on Unix/Linux/Mac OSX both 'snow' and 'multicore'
#' (default) functionalities are available.}
#' }
#'}
#'
#'\subsection{Controls - \code{\link[NMOF]{gridSearch}}}{
#' \describe{
#' \item{n}{the number of levels. Default is 10.}
#' \item{printDetail}{print information on the number of
#' objective function evaluations}
#' \item{method}{can be loop (the default), multicore or
#' snow. See Details.}
#' \item{mc.control}{a list containing settings that will be
#' passed to mclapply if method is multicore. Must be a list
#' of named elements; see the documentation of mclapply in
#' parallel.}
#' \item{cl}{default is NULL. If method snow is used, this
#' must be a cluster object or an integer (the number of
#' cores).}
#' \item{keepNames}{logical: should the names of levels be
#' kept?}
#' \item{asList}{does fun expect a list? Default is FALSE}
#' }
#' }
#'
#'
#' @param y A vector or data frame.
#' @param statistic A function or a character string that specifies
#' how the statistic is computed. The function needs to input the
#' \code{y} and output a scalar.
#' @param dgp A function. The function inputs the first argument
#' \code{y} and a vector of nuisance parameters \code{v} and outputs a simulated \code{y}.
#' It should represent the data generating process under the null.
#' Default value is the function \code{sample(y, replace = TRUE)}, i.e. the
#' bootstrap resampling of \code{y}.
#' @param N An atomic vector. Number of replications of the test
#' statistic.
#' @param ... Other named arguments for statistic which are
#' passed unchanged each time it is called
#' @param est A vector with length of v. It is the starting
#' point of the algorithm. If \code{est} is a consistent estimate
#' of \code{v} then \code{mmc} will return both the MMC and Local Monte Carlo (LMC).
#' Default is NULL, in which case, default values will be generated automatically.
#' @param lower A vector with length of v. Lower bounds for
#' nuisance parameters under the null. See Details.
#' @param upper A vector with length of v. Upper bounds for
#' nuisance parameters under the null. See Details.
#' @param method A character string. Type of algorithm to be
#' used for global optimization. The four available methods
#' are simulated annealing (\code{\link{GenSA}}), particle swarm (\code{\link{pso}}),
#' genetic algorithm (\code{\link{GA}}), and grid search (\code{\link{gridSearch}})
#' Default is \code{GenSA},
#' @param control A list. Arguments to be used to control the
#' behavior of the algorithm chosen in \code{method}. See controls section for more details.
#' @param alpha An atomic vector. If \code{mmc} finds a
#' p-value over \code{alpha}, then the algorithm will stop. This is particularly
#' useful if we are only looking at testing a hypothesis at a particular level.
#' Default is NULL.
#' @param monitor A logical. If set to TRUE, then the p-values at every
#' iteration and the cumulative maximum p-value are plotted on a graphical device.
#' Default is FALSE.
#'
#' @inheritParams pvalue
#'
#' @return The returned value is an object of class \code{mmc}
#' containing the following components:
#'  \item{S0}{Observed value of \code{statistic}.}
#'  \item{pval}{Maximized Monte Carlo p-value of \code{statistic} under null.}
#'  \item{y}{Data specified in call.}
#'  \item{statistic}{\code{statistic} function specified in
#'  call.}
#'  \item{dgp}{\code{dgp} function specified in call.}
#'  \item{est}{\code{est} vector if specified in call.}
#'  \item{lower}{\code{lower} vector if specified in call.}
#'  \item{upper}{\code{upper} vector if specified in call.}
#'  \item{N}{Number of replications specified in call.}
#'  \item{type}{\code{type} of p-value specified in call.}
#'  \item{method}{\code{method} specified in call.}
#'  \item{call}{Original call to \code{mmc}.}
#'  \item{seed}{Value of \code{.Random.seed} at the start of
#'  \code{mmc} call.}
#'  \item{lmc}{If \code{par} is specified, it returns an
#'  object of class \code{mc} corresponding to the Local Monte
#'  Carlo test.}
#'  \item{opt_result}{An object returning the optimization
#'  results.}
#'  \item{rejection}{If \code{alpha} is specified, it returns
#'  a vector specifying whether the hypothesis was rejected at level \code{alpha}.}
#'
#'
#' @references Dufour, J.-M. (2006), Monte Carlo Tests with nuisance parameters:
#' A general approach to finite sample inference and nonstandard asymptotics in econometrics.
#' \emph{Journal of Econometrics}, \bold{133(2)}, 443-447.
#'
#' @references Dufour, J.-M. and Khalaf L. (2003), Monte Carlo Test Methods in Econometrics.
#' in Badi H. Baltagi, ed., \emph{A Companion to Theoretical Econometrics}, Blackwell Publishing Ltd, 494-519.
#'
#' @references Y. Xiang, S. Gubian. B. Suomela, J. Hoeng (2013). Generalized Simulated Annealing for
#' Efficient Global Optimization: the GenSA Package for R. \emph{The R Journal}, Volume
#' \bold{5/1}, June 2013.
#' URL \url{https://journal.r-project.org/}.
#'
#' @references Claus Bendtsen. (2012). pso: Particle Swarm Optimization. R package version 1.0.3.
#' \url{https://CRAN.R-project.org/package=pso}
#'
#' @references Luca Scrucca (2013). GA: A Package for Genetic Algorithms in R. \emph{Journal of
#' Statistical
#' Software}, \bold{53(4)}, 1-37. URL \url{http://www.jstatsoft.org/v53/i04/}.
#'
#' @references Luca Scrucca (2016). On some extensions to GA package: hybrid optimisation,
#' parallelisation and islands evolution. Submitted to \emph{R Journal}. Pre-print available at
#' arXiv URL \url{http://arxiv.org/abs/1605.01931}.
#'
#' @references Manfred Gilli (2011), Dietmar Maringer and Enrico Schumann. Numerical Methods and
#' Optimization
#' in Finance. \emph{Academic Press}.
#'
#'
#' @seealso \code{\link{mc}}, \code{\link{pvalue}}
#'
#' @example /inst/examples/mmc_example.R
#'
#' @export
#'
mmc <- function(y, statistic, ..., dgp = function(y, v) sample(y, replace = TRUE),
                est = NULL, lower, upper, N = 99,
                type = c("geq", "leq", "absolute", "two-tailed"),
                method = c("GenSA", "pso", "GA", "gridSearch"),
                control = list(), alpha = NULL, monitor = FALSE) {

    # Match type, method and extract exact call
    type <- match.arg(type)
    method <- match.arg(method)
    call <- match.call()

    if (missing(y)){
        stop("'y' is missing, no data to call")
    }
    if (missing(statistic)){
        stop("'statistic' is missing, no function to call")
    }
    if (missing(lower) || missing(upper)){
        stop("bounds for nuisance parameters are missing")
    }

    # Extract function from string of characters in statistic
    if (is.character(statistic)) {
        statistic <- get(statistic, mode = "function", envir = parent.frame())
    }
    if (!is.function(statistic)) {
        stop("'statistic' must be a function or a string
             naming a valid function")
    }

    # Test if alpha is in [0,1] and  alpha*(N+1) is an integer
    if (!is.null(alpha)) {
        alpha <- sort(alpha)
        if (!all(alpha > 0) && !all(alpha <= 1)) {
            stop("'alpha' must be a number between 0 and 1")
        } else if (!all(alpha * (N + 1) == round(alpha * (N + 1)))) {
            warning("'alpha'*(N+1) must be integer for mmc to
                    be an exact test")
        }
    }

    # Get seed
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        stats::runif(1)
    }
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

    # Compute the statistic
    if (is.null(est)){
        lmc <- NULL
        S0 <- statistic(y, ...)
    } else {
        # Compute the Local Monte Carlo if est is specified
        lmc <- mc(y = y, statistic = statistic, ..., dgp = function(y) dgp(y, est),
                  N = N, type = type)
        S0 <- lmc$S0
    }

    # Test if S0 is a number
    if (!is.numeric(S0)) {
        stop("'statistic' must return a number")
    }

    # Test if S0 is a vector of length 1
    if (length(S0)!=1 && !is.atomic(S0)) {
        stop("'statistic' must return an atomic vector of
             length = 1")
    }

    # Initialize iteration counter
    ite <- 0

    # P-value function to maximize
    max_fn <- function(v, ...) {
        temp <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        assign(".Random.seed", seed, envir = .GlobalEnv, inherits = FALSE)
        on.exit(assign(".Random.seed", temp, envir = .GlobalEnv, inherits = FALSE))

        # Simulate statistic
        S <- simulation_mmc(y, statistic, dgp, v, N, ...)

        # Calculate p-value
        pval <- pvalue(S0, S, type)

        # Update iteration counter
        ite <<- ite + 1
        opt_trace[ite,] <<- c(ite,pval,max(pval,opt_trace$pval, na.rm = TRUE))

        monitor_mmc(opt_trace, alpha = alpha, monitor = monitor)

        return(pval)
    }
    # Wrapper for algorithms that need function to minimize
    min_fn <- function(v, ...) {
        -max_fn(v, ...)
    }

    if (method == "GenSA") {
        # Set par
        par <- est
        # Get controls for global optimization
        if (!is.null(alpha)){
            control$threshold.stop <- -max(alpha)  - 1/(N + 1)
        }
        opt_control <- get_control(method, control)

        opt_trace <- as.data.frame(matrix(data = NA, opt_control$maxit, 3,
                                            dimnames = list(NULL,c("ite","pval","max"))))


        # Run simulated annealing on min_fn
        opt_result <- GenSA::GenSA(par = par, fn = min_fn, lower = lower,
                                   upper = upper, control = opt_control, ...)
    } else if (method == "pso") {
        # Set par to NA if par = NULL
        if (is.null(est)) {
            par <- rep(NA, length(lower))
        } else {
            par <- est
        }
        # Get controls for global optimization
        if (!is.null(alpha)){
            control$abstol <- -max(alpha) - 1/(N + 1)
        }
        opt_control <- get_control(method, control)


        opt_trace <- as.data.frame(matrix(data = NA, opt_control$maxit, 3,
                                            dimnames = list(NULL,c("ite","pval","max"))))

        # Run particle swarm optimization on min_fn
        opt_result <- pso::psoptim(par = par, fn = min_fn, gr = NULL, ...,
                                   lower = lower, upper = upper, control = opt_control)
    } else if (method == "GA") {
        # Set par
        par <- est
        # Get controls for global optimization
        if (!is.null(alpha)){
            control$maxFitness <- max(alpha) + 1/(N + 1)
        }
        # Get controls for global optimization
        opt_control <- get_control(method, control)

        opt_trace <- as.data.frame(matrix(data = NA, opt_control$maxiter, 3,
                                            dimnames = list(NULL,c("ite","pval","max"))))

        # Run Genetic Algorithm on max_fn
        opt_result <- GA::ga(type = "real-valued", max_fn, ..., min = lower, max = upper,
                             popSize = opt_control$popSize,
                             pcrossover = opt_control$pcrossover,
                             pmutation = opt_control$pmutation,
                             maxiter = opt_control$maxit, run = opt_control$run,
                             maxFitness = opt_control$maxFitness, names = opt_control$names,
                             suggestions = opt_control$suggestions,
                             optim = opt_control$optim,  optimArgs = opt_control$optimArgs,
                             keepBest = opt_control$keepBest, parallel = opt_control$parallel,
                             monitor = opt_control$monitor)
    } else if (method == "gridSearch") {
        # Get controls for global optimization
        opt_control <- get_control(method, control)

        opt_trace <- as.data.frame(matrix(data = NA, opt_control$n^length(lower), 3,
                                            dimnames = list(NULL,c("ite","pval","max"))))
        # Run Genetic Algorithm on max_fn
        opt_result <- NMOF::gridSearch(fun = min_fn, lower = lower, upper = upper,
                                       npar = opt_control$npar, n = opt_control$n,
                                       printDetail = opt_control$printDetail,
                                       method = opt_control$method,
                                       mc.control = opt_control$mc.control,
                                       cl = opt_control$cl, keepNames = opt_control$keepNames,
                                       asList = opt_control$asList)
        }

    # Return "mmc" object
    return_mmc(S0 = S0, y = y, statistic = statistic, dgp = dgp, est = est, lower = lower,
               upper = upper, N = N, type = type, method = method, alpha = alpha,
               control = control, call = call, seed = seed, lmc = lmc,
               opt_result = opt_result, opt_trace = opt_trace)
}
