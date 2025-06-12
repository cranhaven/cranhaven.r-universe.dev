#' @title Significant dimensions in principal coordinate analysis
#'
#' @description Function for determine the number of significant dimensions in principal coordinate analysis (PCoA).
#' 
#' @details At each iteration step a bootstrap sample is subjected to PCoA ordination, the scores are submitted 
#' to a procrustean adjustment, and the correlation between observed and bootstrap ordination scores 
#' is computed. It compares such correlations to the same parameter generated in a parallel bootstrapped
#' ordination of randomly permuted data. The number of axes in bootstrap or null PCoA with eigenvectors 
#' corresponding to positive eigenvalues may be smaller than the number of axes monitored, in this case, 
#' axes with values equal to 0 are created. The number of iterations with original values for each axis 
#' is shown in n.permut.bootstrap and n.permut.null. 
#'
#' The function scores.pcoasig re-scales the correlation values for \code{\link{biplot}} graphics.
#'
#' @encoding UTF-8
#' @importFrom picante randomizeMatrix
#' @importFrom vegan procrustes vegdist
#' @importFrom ape pcoa
#' @importFrom stats fitted cor
#' @aliases pcoa.sig print.pcoasig summary.pcoasig print.summarypcoasig
#' @param data Community data matrix.
#' @param method Method for dissimilarity index, as accepted by \code{\link{vegdist}} (Default method = "gower").
#' @param squareroot Logical argument (TRUE or FALSE) to specify if use square root of dissimilarity 
#' index (Default squareroot = FALSE).
#' @param axis Maximum number of ordination principal axes to be monitored (Default axis = 6).
#' @param n.start Initial sample size. If n.start = NULL 
#' initial sample size is equal to total sample size (Default n.start = NULL).
#' @param by Sampling unit is added at each sampling step (Default by = 1). 
#' @param iterations Number of permutations to assess significance (Default iterations = 1000).
#' @param parallel Number of parallel processes or a predefined socket cluster done with parallel package. Tip: use detectCores() (Default parallel = NULL).
#' @param object An object of class pcoasig.
#' @param x An object of class pcoasig.
#' @param choices Axes for re-scaling. Choices must have length equal to two (Default choices = c(1, 2)).
#' @param ... Other parameters for the respective functions.
#' @note \strong{Principal Component Analysis (PCA)}
#'
#' You can use the same function to determine the number of significant dimensions in principal component 
#' analysis (PCA). For this, standardize each variable for zero mean and uni variance (function decostand
#' and method standardize) and use euclidean distance as dissimilarity index.
#' 
#' \strong{Interpretation}
#' 
#' If the higher dimension is significant, then all lower dimensions will also be significant. 
#'
#' @return \item{value}{The eigenvalues, relative eigenvalues and cumulative relative eigenvalues..} 
#' \item{vectors}{The principal coordinates.} \item{correlations}{Correlations
#' between axis and original data.} \item{mean.cor.null}{Mean correlations, for axis, between null and reference
#' scores.} \item{mean.cor.bootstrap}{Mean correlations, for axis, between bootstrap and reference scores.}
#' \item{n.permut.bootstrap}{Number of iterations for each axis in bootstrap step.}
#' \item{n.permut.null}{Number of iterations for each axis in null step.} \item{probabilities}{Probabilities for each axis.}
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso \code{\link{pcoa}}, \code{\link{procrustes}} 
#' @references Pillar, V.D. (1999). The bootstrapped ordination reexamined. Journal of Vegetation Science 10, 895-902.
#' @keywords PCPS
#' @examples
#' 
#' \dontrun{
#' data(flona)
#' res<-pcoa.sig(flona$community, method = "bray", squareroot = TRUE, axis = 6, iterations = 100)
#' res
#' summary(res)$scores
#' }
#' 
#' @export
pcoa.sig<-function (data, method = "gower", squareroot = FALSE, axis = 6, n.start = NULL, by = 1, iterations = 1000, parallel = NULL){
	RES <- list(call = match.call())
	data <- as.matrix(data)
	colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "v.")
	rownames(data) <- rownames(data, do.NULL = FALSE, prefix = "u")
	n.row <- nrow(data)
	n.col <- ncol(data)
	if (is.null(n.start)) {
		n.start <- n.row
	}
	if (n.start > n.row) {
		stop("\n n.start must be lower than the number of sampling units\n")
	}
	seq.samp <- seq.int(from = n.start, to = n.row, by = by)
	if(!(((n.row - n.start)/by)%%1==0) & n.row!=n.start){
		seq.samp <- c(seq.samp, n.row)
	}
	table.row <- length(seq.samp)
	pco.ref <- wcmdscale.org(data, method = method, squareroot = squareroot, eig = TRUE, correlations = TRUE)
	vectors <- pco.ref$vectors[, 1:axis, drop = FALSE]
	if (axis > ncol(pco.ref$vectors)) {
		stop("\n axis must be lower than the number of axis with positive eigenvalues in reference ordination\n")
	}
	if (axis > n.start) {
		stop("\n n.start must be higher than the number of axis monitored\n")
	}
	mean.cor.null <- matrix(NA, nrow = table.row, ncol = axis, dimnames = list(paste("n.",seq.samp, sep = ""),colnames(vectors)))
	mean.cor.bootstrap <- matrix(NA, nrow = table.row, ncol = axis, dimnames = list(paste("n.",seq.samp, sep = ""),colnames(vectors)))
	probabilities <- matrix(NA, nrow = table.row, ncol = axis, dimnames = list(paste("n.",seq.samp, sep = ""),colnames(vectors)))
	n.permut <- matrix(NA, nrow = table.row, ncol = axis, dimnames = list(paste("n.",seq.samp, sep = ""),colnames(vectors)))
	n.randon <- matrix(NA, nrow = table.row, ncol = axis, dimnames = list(paste("n.",seq.samp, sep = ""),colnames(vectors)))
	ptest <- function(r, data, method, squareroot, axis, vectors){
		res <- list()
		matrix.1 <- matrix(1, nrow = 1, ncol = axis)
		matrix.2 <- matrix(1, nrow = 1, ncol = axis)
		matrix.permut <- matrix(NA, nrow = 1, ncol = axis)
		matrix.randon <- matrix(NA, nrow = 1, ncol = axis)
		n.row <- nrow(data)
		sam <- sample(1:n.row, r, replace = TRUE)
		permut <- data[sam, , drop = FALSE]
		vectors.permut <- wcmdscale.org(permut, method = method, squareroot = squareroot, eig = FALSE, correlations = FALSE, k = axis)$vectors
		if (ncol(vectors.permut) < axis) {
			n.number <- axis - ncol(vectors.permut)
			matrix.1[1,(axis-n.number+1):axis]<-0
			vectors.permut <- cbind(vectors.permut, matrix(0, nrow = nrow(vectors.permut), ncol = n.number))
		}
		for (l in 1:axis) {
			fit.procrustes <- stats::fitted(vegan::procrustes(vectors[sam, 1:l], vectors.permut[, 1:l], choices = 1:l), truemean = TRUE)
			matrix.permut[1, l] <- cor(vectors[sam, l], fit.procrustes[, l])
		}
		res$matrix.permut <- matrix.permut
		res$matrix.1 <- matrix.1
		randon <- t(picante::randomizeMatrix(t(data), null.model = "richness"))
		vectors.randon.ref <- wcmdscale.org(randon, method = method, squareroot = squareroot, eig = FALSE, correlations = FALSE, k = axis)$vectors
		sam.randon <- sample(1:n.row, r, replace = TRUE)
		permut.randon <- randon[sam.randon, ,drop = FALSE]
		vectors.permut.randon <- wcmdscale.org(permut.randon, method = method, squareroot = squareroot, eig = FALSE, correlations = FALSE, k = axis)$vectors
		if (ncol(vectors.permut.randon) < axis) {
			n.number.randon <- axis-ncol(vectors.permut.randon)
			matrix.2[1,(axis-n.number.randon+1):axis] <- 0
			vectors.permut.randon <- cbind(vectors.permut.randon, matrix(0, nrow = nrow(vectors.permut.randon), ncol = n.number.randon))
		}
		for (m in 1:axis) {
			fit.procrustes.randon <- stats::fitted(vegan::procrustes(vectors.randon.ref[sam.randon, 1:m], vectors.permut.randon[, 1:m], choices = 1:m), truemean = TRUE)
			matrix.randon[1, m] <- stats::cor(vectors.randon.ref[sam.randon, m], fit.procrustes.randon[, m])
		}
		res$matrix.2 <- matrix.2
		res$matrix.randon <- matrix.randon
	return(res)
	}
	newClusters <- FALSE
	if (is.numeric(parallel)) {
	  parallel <- parallel::makeCluster(parallel, type = "PSOCK")
	  newClusters <- TRUE
	}
	m <- 0
	for (r in seq.samp) {
		m <- m+1
		if(!inherits(parallel, "cluster")){
			res.temp<-vector("list",iterations)
			for(i in 1:iterations){
				res.temp[[i]] <- ptest(r, data = data, method = method, squareroot = squareroot, axis = axis, vectors = vectors)
			}
		} else {
			res.temp <- parallel::parRapply(parallel, matrix(r, iterations, 1), ptest, data = data, method = method, squareroot = squareroot, axis = axis, vectors = vectors)
		}
		matrix.1 <- t(sapply(seq_len(iterations), function(i) res.temp[[i]]$matrix.1))
		matrix.2 <- t(sapply(seq_len(iterations), function(i) res.temp[[i]]$matrix.2))
		matrix.permut <- t(sapply(seq_len(iterations), function(i) res.temp[[i]]$matrix.permut))
		matrix.randon <- t(sapply(seq_len(iterations), function(i) res.temp[[i]]$matrix.randon))
		mean.cor.null[m, ] <- colMeans(matrix.randon)
		mean.cor.bootstrap[m, ] <- colMeans(matrix.permut)
		probabilities[m, ] <- colSums(ifelse(matrix.randon >= matrix.permut, 1, 0))/iterations
		n.permut[m, ] <- colSums(matrix.1)
		n.randon[m, ] <- colSums(matrix.2)
	}
	if (newClusters) {
	  parallel::stopCluster(parallel)
	}
	RES$values <- pco.ref$values
	RES$vectors <- pco.ref$vectors
	RES$correlations <- pco.ref$correlations
	RES$mean.cor.null <- mean.cor.null
	RES$mean.cor.bootstrap <- mean.cor.bootstrap
	RES$probabilities <- probabilities
	RES$n.permut.bootstrap <- n.permut
	RES$n.permut.null <- n.randon
	class(RES) <- "pcoasig"
	return(RES)
}