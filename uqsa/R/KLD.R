density_fn <- function(ZETA, h=NULL) {
	# a very simplified KDE function
	if (missing(h) || is.null(h)) {
		stdv <- apply(ZETA,2,sd)
		iqr <- apply(ZETA,2,IQR)
		ssize <- NROW(ZETA)
		n <- NCOL(ZETA)
		h <- 0.9*pmin(stdv,iqr/1.34)*exp(-1/(2*n+1)*log(ssize)) # crude rule of thumb
	}
	return(
		function(xi) { # p_Y(X)
			sapply(
				seq(NROW(xi)),
				\(i) {
					mean(mvtnorm::dmvnorm(ZETA, mean = xi[i, ], sigma = diag(h)))
				}
			)
		}
	)
}


#' Kullback Leibler Divergence
#'
#' This function calculates the Kullback Leibler Divergence $D(P\|Q)$
#' value between two distributions $P$ and $Q$, represented by their
#' two samples, `X` and `Y`. Both samples will have their density
#' inferred. The intended use-case is to compare 2D and 3D densities,
#' e.g.: to find interesting pairs of parameters within a bigger
#' distribution.
#'
#' This estimate requires a method of density estimation, by default
#' we use the copula based methods [fitCopula] and [dCopulaPrior]
#' (which has a fairly high accuracy, but can be quite slow).
#'
#' Effects of setting `de` to
#' - `"ks"`: density is estimated via `ks::kde()`
#' - `"mvtnorm"`: density is estimated via manual kernel
#'    density estimation, using the multivariate
#'    Gaussian density `mvtnorm::dmvnorm`
#'
#' @param X sample from distribution P
#' @param Y sample from distribution Q
#' @param de density estimation mechanism (character scalar)
#' @return D, a scalar value, the Kullback Leibler Divergence
#' @export
KLD <- function(X,Y,de=c("copula","ks","mvtnorm")){
	de <- match.arg(de)
	if (de == "mvtnorm" && requireNamespace("mvtnorm",quietly=TRUE)) { # quick and dirty KDE
		P <- density_fn(X)
		Q <- density_fn(Y)
	} else if (de == "ks" && requireNamespace("ks",quietly=TRUE)) {
		P <- \(xi) ks::kde(X, eval.points = xi, density=TRUE, binned=FALSE)$estimate
		Q <- \(xi) ks::kde(Y, eval.points = xi, density=TRUE, binned=FALSE)$estimate
	} else {
		P <- dCopulaPrior(fitCopula(X)) # density estimate for X
		Q <- dCopulaPrior(fitCopula(Y)) # density estimate for Y
	}
	D <- mean(log(P(X))-log(Q(X))) # X ~ P
	return(D)
}

#' Multivariate Normal Distribution KLD
#'
#' Like the [KLD] function, this function calculates KLD values, but
#' for the specific case of multivariate normal distributions
#' \eqn{\\mathcal{N}_{A}}{N_A} and \eqn{\\mathcal{N}_{B}}{N_B}.  The two distributions are
#' specified using \eqn{\\mu}{mu} and \eqn{\\Sigma}{Sigma} values (mean and covariance).
#'
#' @param muA mean of distribution A
#' @param SigmaA Covariance of distribution A
#' @param muB mean of distribution B
#' @param SigmaB Covariance of distribution B
#' @return the KLD value D(A|B)
#' @export
exact_normal_kld <- function(muA,SigmaA,muB,SigmaB){
	tr <-\(M) sum(diag(M))
	k <- length(muA)
	log_det_A <- as.numeric(determinant(SigmaA, logarithm = TRUE)$modulus)
	log_det_B <- as.numeric(determinant(SigmaB, logarithm = TRUE)$modulus)
	D <- tr(solve(SigmaB,SigmaA)) - k + t(muB-muA) %*% solve(SigmaB,muB-muA) + log_det_B - log_det_A
	return(0.5*drop(D))
}
