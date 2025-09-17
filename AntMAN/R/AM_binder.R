#######################################################################################
###############
############### AntMAN Package
###############
###############
#######################################################################################


# #' Sequentially Allocated Latent Structure Optimisation
# #'
# #' Heuristic partitioning to minimise the expected Binder loss function 
# #' with respect to a given expected adjacency matrix. 
# #'
# #' @param fit an \code{\link{AM_mcmc_output}} object. 
# #' @param maxClusts Maximum number of clusters. 
# #' The actual number of clusters searched may be lower. 
# #' If set to 0L, the maximum is automatically limited by the number of items. 
# #' @param Const_Binder Relative penalty in the Binder loss function 
# #' for false-positives vis-a-vis false-negatives. 
# #' Must be a real number in the interval \[0, 1\]. 
# #' @param batchSize Number of permutations scanned per thread. 
# #' If set to 0L, the thread will continue to scan permutations until it times out 
# #' (in which case \code{timeLimit} cannot be 0L).
# #' @param nScans Number of scans for each permutation. 
# #' @param maxThreads Maximum number of threads to use. 
# #' If set to 0L (default), the maximum number of threads 
# #' will be determined by the runtime. 
# #' Set to 1L for no parallelisation. 
# #' The actual number of threads used may be lower than \code{maxThreads}.
# #' @param timeLimit Maximum computation time for each thread in milliseconds. 
# #' The actual computational time may be higher, 
# #' since the time limit is only checked at the end of each iteration. 
# #' If set to 0L, the thread will never time out 
# #' (in which case \code{batchSize} cannot be 0L).
# #' @return A list containing the following items:
# #' \itemize{
# #' \item \code{Labels} - the vector of partition labels.
# #' \item \code{BinderLoss} - the associated binder loss function.
# #' \item \code{NumClusts} - the number of clusters found.
# #' \item \code{NumPermutations} - the number of permutations actually scanned.
# #' \item \code{WallClockTime} - cumulative wall-clock time used by all threads in milliseconds.
# #' \item \code{TimeLimitReached} - whether the computation time limit was reached in any of the threads.
# #' \item \code{NumThreads} - actual number of threads used.
# #' }
# #'@export
# AM_binder <- function(fit, maxClusts=0L, Const_Binder = 0.5, batchSize = 1000L, nScans = 10L, maxThreads = 0L, timeLimit = 600000L) {

# 	eam  = AM_coclustering(fit)
		
# 	if (!is.validAdjacencyMatrix(eam)){
# 		stop(paste("eam must be a symmetric matrix with values between 0 and 1, ",
# 						"and 1s on the diagonal."))
# 	}
# 	if (!is.finiteInteger(maxClusts) | 
# 			maxClusts < 0) {
# 		stop("maxClusts must be a nonnegative integer. ")
# 	}
# 	if (!is.nonNegNumberLessThan1(Const_Binder)) {
# 		stop("Const_Binder must be a number between 0 and 1.")
# 	}
# 	if (!is.finiteInteger(batchSize) | 
# 			batchSize < 0) {
# 		stop("batchSize must be a nonnegative integer.")
# 	}
# 	if (!is.finiteInteger(nScans) | 
# 			nScans <= 0) {
# 		stop("nScans must be a positive integer.")
# 	}
# 	if (!is.finiteInteger(maxThreads) | 
# 			maxThreads < 0) {
# 		stop("maxThreads must be a nonnegative integer.")
# 	}
# 	if (!is.finiteInteger(timeLimit) | 
# 			timeLimit < 0) {
# 		stop("timeLimit must be a nonnegative integer.")
# 	}
# 	if (timeLimit == 0L & batchSize == 0L) {
# 		stop("batchSize and timeLimit cannot both be 0.")
# 	}
	
# 	temp = .salso(eam, maxClusts, Const_Binder, batchSize, nScans, maxThreads, timeLimit)
# 	temp[["Labels"]] <- as.integer(as.vector(temp[["Labels"]]))
# 	return (temp)
# }

# #' Compute the Binder loss function
# #' 
# #' Compute the Binder loss function of a partitioning
# #' with respect to an expected adjacency matrix.
# #' 
# #' @param eam Expected Adjacency Matrix, i.e., 
# #' the matrix whose entries \eqn{E_{ij}} is the posterior probability 
# #' that items \eqn{i} and \eqn{j} are together. 
# #' If the partitioning is already known, this is just the adjacency matrix.
# #' @param labels vector of partition labels. Must be integers. 
# #' @param Const_Binder Relative penalty in the Binder loss function 
# #' for false-positives vis-a-vis false-negatives. 
# #' Must be a real number in the interval \[0, 1\]. 
# #' @return The value of the Binder loss function of the given partition labels
# #' with respect to the given pairwise allocation matrix. 
# #'@export 
# AM_compute_binder_loss <- function(eam, labels, Const_Binder = 0.5){
#     if (!is.validAdjacencyMatrix(eam)){
#         stop(paste("eam must be a symmetric matrix with values between 0 and 1, ",
#                    "and 1s on the diagonal."))
#     }
#     if (!all(is.finiteInteger(labels))) {
#         stop("labels must be a vector of integers.")
#     }
#     if (length(labels) != ncol(eam)) {
#         stop("Incompatible size of eam and labels.")
#     }
#     return (.computeBinderLoss(eam, labels, Const_Binder))
# }



# #' Binder distance of two partitions
# #' 
# #' Compute the binder loss function of a partitioning
# #' with respect to the adjacency matrix of another partitioning.
# #' @param testLabels The vector of integer labels of the partitioning.
# #' @param refLabels The vector of partition labels to use to construct the adjacency matrix.
# #' @param Const_Binder Relative penalty in the Binder loss function 
# #' for false-positives vis-a-vis false-negatives. 
# #' Must be a real number in the interval \[0, 1\]. 
# #' @return The value of the Binder loss function of \code{testLabels} 
# #' with respect to the adjacency matrix of \code{refLabels}.
# #'@export
# AM_compute_binder_distance <- function(testLabels, refLabels, Const_Binder) {
#     if (!all(is.finiteInteger(testLabels))) {
#         stop("labels must be a vector of integers.")
#     }
#     if (!all(is.finiteInteger(refLabels))) {
#         stop("labels must be a vector of integers.")
#     }
#     if (!is.nonNegNumberLessThan1(Const_Binder)) {
#         stop("Const_Binder must be a number between 0 and 1.")
#     }
#     return(computeBinderLoss(computeAdjacencyMatrix(refLabels), testLabels, Const_Binder))
# }

# is.validAdjacencyMatrix <- function(p) {
#     return(all(is.nonNegNumberLessThan1(p)) & 
#              nrow(p) == ncol(p) & 
#              all(p == t(p)) & 
#              sum(diag(p)) == nrow(p))
# }

# is.finiteInteger <- function(x) {
#     return (is.integer(x) & is.finite(x))
# }

# is.nonNegNumberLessThan1 <- function(x) {
#     return (is.numeric(x) & is.finite(x) & x <= 1 & x >= 0)
# 	}




#' Sequentially Allocated Latent Structure Optimisation
#'
#' Heuristic partitioning to minimise the expected loss function 
#' with respect to a given expected adjacency matrix. This function is built upon R-package salso's implementation of the
#' \code{salso} function. See salso \insertCite{salso}{AntMAN} for more details.
#'  

#' @param eam a co-clustering/ clustering matrix. See salso for more information on which matrix to use.
#' @param loss the recommended loss functions to be used are the "binder" or "VI". However, other loss functions that are supported
#' can be found in the R-package salso's salso function. 
#' @param maxNClusters Maximum number of clusters to be considered. 
#' The actual number of clusters searched may be lower. Default is 0.
#' @param nRuns Number of runs to try. Default is 16.
#' @param maxZealousAttempts Maximum number of tries for zealous updates. Default is 10. See salso for more information.
#' The actual number of clusters searched may be lower. 
#' @param nRuns Number of runs to try.
#' @param maxZealousAttempts Maximum number of tries for zealous updates. See salso for more information.
#' @param probSequentialAllocation  The probability of using sequential allocation instead of random sampling via sample(1:K,ncol(x),TRUE), 
#' where K is maxNClusters. Default is 0.5. See salso for more information.
#' argument.
#' @param nCores Number of CPU cores to engage. Default is 0.

#'@source David B. Dahl and Devin J. Johnson and Peter Müller (2021). salso: Search Algorithms and Loss Functions for Bayesian Clustering. R package version 0.2.15.
#'@return A numeric vector describing the estimated partition. The integer values represent the cluster labels of each item respectively. 


#'@importFrom salso salso
#'@export
AM_salso = function(eam, loss, maxNClusters = 0, nRuns = 16, maxZealousAttempts = 10, probSequentialAllocation = 0.5, nCores = 0){
	return(salso(eam, loss, maxNClusters, nRuns, maxZealousAttempts, probSequentialAllocation, nCores))
}