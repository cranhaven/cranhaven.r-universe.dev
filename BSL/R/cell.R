#' Cell biology example
#'
#' @description This example estimates the probabilities of cell motility and
#'   cell proliferation for a discrete-time stochastic model of cell spreading.
#'   We provide the data and tuning parameters required to reproduce the results
#'   in \insertCite{An2019;textual}{BSL}.
#'
#' @param theta	   A vector of proposed model parameters,
#'   \eqn{P_m} and \eqn{P_p}.
#' @param Y        A \code{rows} \eqn{\times}
#'   \code{cols} \eqn{\times} \code{num_obs} array
#'   of the cell presences at times \code{1:num_obs} (not time 0).
#' @param Yinit    The initial matrix of cell presences of size \code{rows}
#'   \eqn{\times} \code{cols}.
#' @param rows     The number of rows in the lattice (rows in the cell location
#'   matrix).
#' @param cols     The number of columns in the lattice (columns in the cell
#'   location matrix).
#' @param sim_iters The number of discretisation steps to get to when an
#'   observation is actually taken. For example, if observations are taken every
#'   5 minutes but the discretisation level is 2.5 minutes, then
#'   \code{sim_iters} would be 2. Larger values of \code{sim_iters} lead to more
#'   ``accurate'' simulations from the model, but they also increase the
#'   simulation time.
#' @param num_obs  The total number of images taken after initialisation.
#'
#' @details Cell motility (movement) and proliferation (reproduction) cause
#'   tumors to spread and wounds to heal. If we can measure cell proliferation
#'   and cell motility under different situations, then we may be able to use
#'   this information to determine the efficacy of different medical treatments.
#'
#'   A common method for measuring in vitro cell movement and proliferation is
#'   the scratch assay. Cells form a layer on an assay and, once they are
#'   completely covering the assay, a scratch is made to separate the cells.
#'   Images of the cells are taken until the scratch has closed up and the cells
#'   are in contact again. Each image can be converted to a binary matrix by
#'   forming a lattice and recording the binary matrix (of size \code{rows}
#'   \eqn{\times} \code{cols}) of cell presences.
#'
#'   The model that we consider is a random walk model with parameters for the
#'   probability of cell movement
#'   (\eqn{P_m}) and the probability
#'   of cell proliferation
#'   (\eqn{P_p}) and it has no
#'   tractable likelihood function. We use the vague priors
#'   \eqn{P_m \sim U(0,1)}
#'   and \eqn{P_p \sim U(0,1)}.
#'
#'   We have a total of 145 summary statistics, which are made up of the Hamming
#'   distances between the binary matrices for each time point and the total
#'   number of cells at the final time.
#'
#'   Details about the types of cells that this model is suitable for and other
#'   information can be found in \insertCite{Price2018;textual}{BSL} and
#'   \insertCite{An2019;textual}{BSL}. \insertCite{Johnston2014;textual}{BSL}
#'   use a different ABC method and different summary statistics for a similar
#'   example.
#'
#' @section A simulated dataset:
#'
#'   An example ``observed'' dataset and the tuning parameters relevant to that
#'   example can be obtained using \code{data(cell)}. This ``observed'' data is
#'   a simulated dataset with \eqn{P_m = 0.35} and 
#'   \eqn{P_p = 0.001}. The lattice has 27 \code{rows} and 36
#'   \code{cols} and there are \code{num_obs = 144} observations after time 0
#'   (to mimic images being taken every 5 minutes for 12 hours). The simulation
#'   is based on there initially being 110 cells in the assay.
#'
#'   Further information about the specific choices of tuning parameters used in
#'   BSL and BSLasso can be found in An et al. (2019).
#'
#'   \itemize{
#'
#'   \item \code{data}:  The \code{rows}
#'   \eqn{\times} \code{cols}
#'   \eqn{\times} \code{num_obs} array of the cell
#'   presences at times 1:144.
#'
#'   \item \code{sim_args}: Values of \code{sim_args} relevant to this example.
#'
#'   \item \code{sum_args}: Values of \code{sum_args} relevant to this example,
#'   i.e. just the value of \code{Yinit}.
#'
#'   \item \code{start}: A vector of suitable initial values of the parameters
#'   for MCMC.
#'
#'   \item \code{cov}: The covariance matrix of a multivariate normal random
#'   walk proposal distribution used in the MCMC, in the form of a 2
#'   \eqn{\times} 2 matrix.
#'
#'   }
#'
#' @examples
#' \dontrun{
#' require(doParallel) # You can use a different package to set up the parallel backend
#'
#' # Loading the data for this example
#' data(cell)
#' model <- newModel(fnSim = cell_sim, fnSum = cell_sum, simArgs = cell$sim_args,
#'                   sumArgs = cell$sum_args, theta0 = cell$start, fnLogPrior = cell_prior,
#'                   thetaNames = expression(P[m], P[p]))
#' thetaExact <- c(0.35, 0.001)
#'
#' # Performing BSL (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultCellBSL <- bsl(cell$data, n = 5000, M = 10000, model = model, covRandWalk = cell$cov,
#'                      parallel = TRUE, verbose = 1L)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultCellBSL)
#' summary(resultCellBSL)
#' plot(resultCellBSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Performing uBSL (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultCelluBSL <- bsl(cell$data, n = 5000, M = 10000, model = model, covRandWalk = cell$cov,
#'                       method = "uBSL", parallel = TRUE, verbose = 1L)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultCelluBSL)
#' summary(resultCelluBSL)
#' plot(resultCelluBSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Performing tuning for BSLasso
#' ssy <- cell_sum(cell$data, cell$sum_args$Yinit)
#' lambda_all <- list(exp(seq(0.5,2.5,length.out=20)), exp(seq(0,2,length.out=20)),
#'                    exp(seq(-1,1,length.out=20)), exp(seq(-1,1,length.out=20)))
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' set.seed(100)
#' sp_cell <- selectPenalty(ssy, n = c(500, 1000, 1500, 2000), lambda_all, theta = thetaExact,
#'     M = 100, sigma = 1.5, model = model, method = "BSL", shrinkage = "glasso",
#'     parallelSim = TRUE, parallelMain = FALSE)
#' stopCluster(cl)
#' registerDoSEQ()
#' sp_cell
#' plot(sp_cell)
#'
#' # Performing BSLasso with a fixed penalty (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultCellBSLasso <- bsl(cell$data, n = 1500, M = 10000, model = model, covRandWalk = cell$cov, 
#'                          shrinkage = "glasso", penalty = 1.3, parallel = TRUE, verbose = 1L)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultCellBSLasso)
#' summary(resultCellBSLasso)
#' plot(resultCellBSLasso, thetaTrue = thetaExact, thin = 20)
#'
#' # Performing semiBSL (reduce the number of iterations M if desired)
#' # Opening up the parallel pools using doParallel
#' cl <- makeCluster(min(detectCores() - 1,2))
#' registerDoParallel(cl)
#' resultCellSemiBSL <- bsl(cell$data, n = 5000, M = 10000, model = model, covRandWalk = cell$cov, 
#'                          method = "semiBSL", parallel = TRUE, verbose = 1L)
#' stopCluster(cl)
#' registerDoSEQ()
#' show(resultCellSemiBSL)
#' summary(resultCellSemiBSL)
#' plot(resultCellSemiBSL, thetaTrue = thetaExact, thin = 20)
#'
#' # Plotting the results together for comparison
#' # plot using the R default plot function
#' oldpar <- par()
#' par(mar = c(5, 4, 1, 2), oma = c(0, 1, 2, 0))
#' combinePlotsBSL(list(resultCellBSL, resultCelluBSL, resultCellBSLasso, resultCellSemiBSL),
#'     which = 1, thetaTrue = thetaExact, thin = 20, label = c("bsl", "ubsl", "bslasso", "semiBSL"),
#'     col = 1:4, lty = 1:4, lwd = 1)
#' mtext("Approximate Univariate Posteriors", outer = TRUE, cex = 1.5)
#' par(mar = oldpar$mar, oma = oldpar$oma)
#'
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#' @author 								Ziwen An, Leah F. South and Christopher Drovandi
#' @name cell
#' @usage data(ma2)
NULL

#' @describeIn cell The function \code{cell_sim(theta, Yinit, rows, cols,
#'   sim_iters, num_obs)} simulates data from the model, using C++ in the
#'   backend.
#' @export
cell_sim <-function(theta, Yinit, rows, cols, sim_iters, num_obs) {
    Pm <- theta[1]
    Pp <- theta[2]
    Y <- simulate_cell(Yinit, rows, cols, Pm, Pp, sim_iters, num_obs)
    return(Y)
}

#' @describeIn cell The function \code{cell_sum(Y,sum_options)} calculates the
#'   summary statistics for this example.
#' @export
cell_sum <- function(Y, Yinit) {
    num_obs = dim(Y)[3]
    summ_stat = numeric(num_obs+1)
    
    # Hamming distances between cell locations across time
    summ_stat[1] = sum(abs(Yinit-Y[, , 1]))
    for (i in 2:num_obs) {
        summ_stat[i] = sum(abs(Y[, , i-1]-Y[, , i]))
    }
    
    # Total number of cells in the final time period
    summ_stat[num_obs + 1] = sum(Y[, , num_obs])
    
    return(summ_stat)
}

#' @describeIn cell The function \code{cell_prior(theta)} evaluates the log
#'   prior density at the parameter value
#'   \eqn{\theta}.
#' @export
cell_prior <- function(theta) {
    log(theta[1] > 0 & theta[1] < 1 & theta[2] > 0 & theta[2] < 1)
}
