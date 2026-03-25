#' EXPOKIT dmexpv wrapper function
#'
#' This function wraps the .C call to EXPOKIT for the dmexpv function.  Only the output probability
#' matrix is returned.
#'
#' @param n number of rows in Q matrix
#' @param m n-1
#' @param t the value to exponentiate the rate matrix by (often e.g. a time value)
#' @param v variable to store some results in; should have n elements (and perhaps start with 1)
#' @param w same length as v
#' @param tol tolerance for approximations; usually set to 0.01
#' @param anorm the norm of the Q matrix
#' @param lwsp length of workspace (wsp); for dmexpv, lwsp=n*(m+2)+5*(m+2)^2+ideg+1
#' @param wsp workspace to store some results in; should be a double with lwsp elements
#' @param liwsp length of integer workspace; for dmexpv, liwsp=m+2
#' @param iwsp integer workspace
#' @param itrace option, set to 0
#' @param iflag option, set to 0
#' @param ia i indices of Qmat nonzero values
#' @param ja j indices of Qmat nonzero values
#' @param a nonzero values of Qmat (ia, ja, a are columns of a COO-formatted Q matrix)
#' @param nz number of non-zeros in Qmat
#' @param res space for output probability matrix (n x n)
#'
#' EXPOKIT needs the input matrix to be transposed compared to normal.
#' COO format is required for EXPOKIT.
#' @return \code{tmpoutmat} the output matrix for the (first) input t-value
#' @seealso \code{\link{expokit_dmexpv_Qmat}}
#' @seealso \code{\link{expokit_wrapalldmexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' 
#' # DMEXPV and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
#' # DMEXPV is specifically designed for Markov chains and so may be slower, but more accurate.
#' 
#' # DMEXPV, single t-value
#' expokit_wrapalldmexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
#' expokit_wrapalldmexpv_tvals(Qmat=Qmat, tvals=2)
#' 
#' # This function runs a for-loop itself (sadly, we could not get mapply() to work
#' # on a function that calls dmexpv/dgexpv), returning a list of probability matrices.
#' 
#' # DMEXPV functions
#' list_of_P_matrices_dmexpv = expokit_wrapalldmexpv_tvals(Qmat=Qmat, 
#' tvals=tvals, transpose_needed=TRUE)
#' list_of_P_matrices_dmexpv
#' 
expokit_dmexpv_wrapper <- function(n, m, t, v, tol, anorm, wsp, lwsp, iwsp, liwsp, ia, ja, a, nz)
{
	ret <- .Call(R_dmexpv, 
		           as.integer(n), as.integer(m), as.double(t), 
		           as.double(v), as.double(tol), 
		           as.double(anorm), as.double(wsp), as.integer(lwsp), 
		           as.integer(iwsp), as.integer(liwsp), 
		           as.integer(ia), as.integer(ja), 
		           as.double(a), as.integer(nz))
		
	output_Pmat = matrix(ret$res, nrow=n, byrow=TRUE)
	
	return(output_Pmat)
}






#' EXPOKIT dmexpv wrapper function, return just output probs
#'
#' This function wraps the .C call to EXPOKIT for the dmexpv function.  Only the output probabilities
#' not the Pmat probability matrix, are returned.
#'
#' @param n number of rows in Q matrix
#' @param m n-1
#' @param t the value to exponentiate the rate matrix by (often e.g. a time value)
#' @param v variable to store some results in; should have n elements (and perhaps start with 1)
#' @param w same length as v
#' @param tol tolerance for approximations; usually set to 0.01
#' @param anorm the norm of the Q matrix
#' @param lwsp length of workspace (wsp); for dmexpv, lwsp=n*(m+2)+5*(m+2)^2+ideg+1
#' @param wsp workspace to store some results in; should be a double with lwsp elements
#' @param liwsp length of integer workspace; for dmexpv, liwsp=m+2
#' @param iwsp integer workspace
#' @param itrace option, set to 0
#' @param iflag option, set to 0
#' @param ia i indices of Qmat nonzero values
#' @param ja j indices of Qmat nonzero values
#' @param a nonzero values of Qmat (ia, ja, a are columns of a COO-formatted Q matrix)
#' @param nz number of non-zeros in Qmat
#'
#' EXPOKIT needs the input matrix to be transposed compared to normal.
#' COO format is required for EXPOKIT.
#' @return \code{w_output_probs} the output probabilities (= \code{myDMEXPV} variable \code{w}, or the fifth output
#' in the output from .Call("mydmexpv_", ...), given the (first) input t-value.
#' @seealso \code{\link{expokit_dmexpv_Qmat}}
#' @seealso \code{\link{expokit_wrapalldmexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' 
#' # DMEXPV and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
#' # DMEXPV is specifically designed for Markov chains and so may be slower, but more accurate.
#' 
#' # DMEXPV, single t-value
#' expokit_wrapalldmexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
#' expokit_wrapalldmexpv_tvals(Qmat=Qmat, tvals=2)
#' 
#' # This function runs a for-loop itself (sadly, we could not get mapply() to work
#' # on a function that calls dmexpv/dgexpv), returning a list of probability matrices.
#' 
#' # DMEXPV functions
#' list_of_P_matrices_dmexpv = expokit_wrapalldmexpv_tvals(Qmat=Qmat, 
#' tvals=tvals, transpose_needed=TRUE)
#' list_of_P_matrices_dmexpv
#' 
expokit_mydmexpv_wrapper <- function(n, m, t, v, tol, anorm, wsp, lwsp, iwsp, liwsp, itrace, iflag, ia, ja, a, nz)
{
	ret <- .Call(R_mydmexpv, 
		           as.integer(n), as.integer(m), as.double(t), 
		           as.double(v), as.double(tol), 
		           as.double(anorm), as.double(wsp), as.integer(lwsp), 
		           as.integer(iwsp), as.integer(liwsp), 
		           as.integer(ia), as.integer(ja), 
		           as.double(a), as.integer(nz))
	
	# 2017-10-23_fix
	#w_output_probs = matrix(ret$w, ncol=n, byrow=TRUE)
	w_output_probs = matrix(ret, ncol=n, byrow=TRUE)
	
	return(w_output_probs)
}





#' EXPOKIT dgexpv wrapper function, return just output probs
#'
#' This function wraps the .C call to EXPOKIT for the dgexpv function.  Only the output probabilities
#' not the Pmat probability matrix, are returned.
#'
#' @param n number of rows in Q matrix
#' @param m n-1
#' @param t the value to exponentiate the rate matrix by (often e.g. a time value)
#' @param v variable to store some results in; should have n elements (and perhaps start with 1)
#' @param w same length as v
#' @param tol tolerance for approximations; usually set to 0.01
#' @param anorm the norm of the Q matrix
#' @param lwsp length of workspace (wsp); for dgexpv, lwsp=n*(m+2)+5*(m+2)^2+ideg+1
#' @param wsp workspace to store some results in; should be a double with lwsp elements
#' @param liwsp length of integer workspace; for dgexpv, liwsp=m+2
#' @param iwsp integer workspace
#' @param itrace option, set to 0
#' @param iflag option, set to 0
#' @param ia i indices of Qmat nonzero values
#' @param ja j indices of Qmat nonzero values
#' @param a nonzero values of Qmat (ia, ja, a are columns of a COO-formatted Q matrix)
#' @param nz number of non-zeros in Qmat
#'
#' EXPOKIT needs the input matrix to be transposed compared to normal.
#' COO format is required for EXPOKIT.
#' @return \code{w_output_probs} the output probabilities (= \code{myDGEXPV} variable \code{w}, or the fifth output
#' in the output from .Call("mydgexpv_", ...), given the (first) input t-value.
#' @seealso \code{\link{expokit_dgexpv_Qmat}}
#' @seealso \code{\link{expokit_wrapalldgexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' 
#' # dgexpv and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
#' # dgexpv is specifically designed for Markov chains and so may be slower, but more accurate.
#' 
#' # dgexpv, single t-value
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=2)
#' 
#' # This function runs a for-loop itself (sadly, we could not get mapply() to work
#' # on a function that calls dgexpv/dgexpv), returning a list of probability matrices.
#' 
#' # dgexpv functions
#' list_of_P_matrices_dgexpv = expokit_wrapalldgexpv_tvals(Qmat=Qmat, 
#' tvals=tvals, transpose_needed=TRUE)
#' list_of_P_matrices_dgexpv
#' 
expokit_mydgexpv_wrapper <- function(n, m, t, v, tol, anorm, wsp, lwsp, iwsp, liwsp, ia, ja, a, nz)
{
	res2 = NULL
	
	# This must be mydgexpv_, not mydgexpv_ !!!!
	
	ret <- .Call(R_mydgexpv, 
		           as.integer(n), as.integer(m), as.double(t), 
		           as.double(v), as.double(tol), 
		           as.double(anorm), as.double(wsp), as.integer(lwsp), 
		           as.integer(iwsp), as.integer(liwsp), 
		           as.integer(ia), as.integer(ja), 
		           as.double(a), as.integer(nz))
		
	# 2017-10-23_fix
	#w_output_probs = matrix(ret$w, ncol=n, byrow=TRUE)
	w_output_probs = matrix(ret, ncol=n, byrow=TRUE)
	
	return(w_output_probs)
}




#' EXPOKIT dgexpv wrapper function
#'
#' This function wraps the .C call to EXPOKIT for the dgexpv function.  Only the output probability
#' matrix is returned.
#'
#' NOTE: DGEXPV vs. DMEXPV. According to the EXPOKIT documentation, DGEXPV should be
#' faster than DMEXPV, however DMEXPV runs an accuracy check appropriate for
#' Markov chains, which is not done in DGEXPV.
#' 
#' @param n number of rows in Q matrix
#' @param m n-1
#' @param timeval the value to exponentiate the rate matrix by (often e.g. a time value)
#' @param v variable to store some results in; should have n elements (and perhaps start with 1)
#' @param w same length as v
#' @param tol tolerance for approximations; usually set to 0.01
#' @param anorm the norm of the Q matrix
#' @param lwsp length of workspace (wsp); for dgexpv, lwsp=n*(m+2)+5*(m+2)^2+ideg+1
#' @param wsp workspace to store some results in; should be a double with lwsp elements
#' @param liwsp length of integer workspace; for dgexpv, liwsp=m+2
#' @param iwsp integer workspace
#' @param itrace option, set to 0
#' @param iflag option, set to 0
#' @param ia i indices of Qmat nonzero values
#' @param ja j indices of Qmat nonzero values
#' @param a nonzero values of Qmat (ia, ja, a are columns of a COO-formatted Q matrix)
#' @param nz number of non-zeros in Qmat
#' @param res space for output probability matrix (n x n)
#'
#' EXPOKIT needs the input matrix to be transposed compared to normal)
#' COO format is required for EXPOKIT.
#' @return \code{tmpoutmat} the output matrix for the (first) input t-value
#' @seealso \code{\link{expokit_dgexpv_Qmat}}
#' @seealso \code{\link{expokit_wrapalldgexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # Example building the inputs from scratch:
#'
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' timeval = tvals[2]
#'
#' 	ideg = as.integer(6)
#'	n=nrow(Qmat)
#'	m=n-1
#'	# t=as.numeric(2.1)
#'	
#'	# v should have as many elements as n; first element = 1 (?)
#'	v=double(n)
#'	v[1] = 1
#'	
#'	# w is the same length
#'	w = double(length=n)
#'	tol=as.numeric(0.01)
#'	
#'	# length of wsp
#'	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 4*(m+2)^2+ideg+1)
#'	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 5*(m+2)^2+ideg+1)
#'	lwsp = as.integer(n*(m+2)+5*(m+2)^2+ideg+1)
#'	
#'	#lwsp = 100
#'	wsp = double(length=lwsp)
#'	
#'	# length of iwsp
#'	liwsp = max(m+2, 7)
#'	iwsp = integer(length=liwsp)
#'	
#'	res = double(length=n*n)
#'	
#'	#matvec = matrix(data=Q, nrow=n, byrow=TRUE)
#'	matvec = Qmat
#'	tmatvec = t(matvec)
#'	rowSums(tmatvec)
#'	colSums(tmatvec)
#'	
#'	# type="O" is being used here, this is supposed to be the
#'	# default for norm(), although it throws an error if not
#'	# specified
#'	# 
#'	# From the help:
#'	# type - character string, specifying the type of matrix norm to be
#'	# computed. A character indicating the type of norm desired. 
#'	# 	"O", "o" or "1"
#'	# 		specifies the one norm, (maximum absolute column sum);
#'	anorm = as.numeric(norm(matvec, type="O"))
#'	#anorm = 1
#'	
#'	
#'	itrace = 0
#'	iflag = 0	
#'	
#'	
#'	#a = as.numeric(tmatvec)
#'	#a = as.numeric(matvec)
#'	tmpmat = tmatvec
#'	tmpmat_in_REXPOKIT_coo_fmt = mat2coo(tmpmat)
#'	ia = tmpmat_in_REXPOKIT_coo_fmt[,"ia"]
#'	ja = tmpmat_in_REXPOKIT_coo_fmt[,"ja"]
#'	a = tmpmat_in_REXPOKIT_coo_fmt[,"a"]
#' 
#'  # Number of non-zeros
#'  nz = nrow(Qmat) * ncol(Qmat)
#' 
#' # Run the wrapper function	
#' 
#' tmpoutmat = expokit_dgexpv_wrapper(n, m, timeval, v, w, tol, anorm, wsp, 
#' lwsp, iwsp, liwsp, itrace, iflag, ia, ja, a, nz, res)
#' 
#' print(tmpoutmat)
#'
expokit_dgexpv_wrapper <- function(n, m, timeval, v, w, tol, anorm, wsp, lwsp, iwsp, liwsp, itrace, iflag, ia, ja, a, nz, res)
{
	res2 = NULL
	
	res2 <- .C("wrapalldgexpv_", as.integer(n), as.integer(m), as.double(timeval), as.double(v), as.double(w), as.double(tol), as.double(anorm), as.double(wsp), as.integer(lwsp), as.integer(iwsp), as.integer(liwsp), as.integer(itrace), as.integer(iflag), as.integer(ia), as.integer(ja), as.double(a), as.integer(nz), as.double(res))
	
	tmpoutmat = matrix(res2[[18]], nrow=n, byrow=TRUE)
	
	return(tmpoutmat)
}



#' wrapper function for FORTRAN itscale5 (for FD's maxent)
#'
#' This function wraps a .C call to FORTRAN for the itscale5 function.
#' 
#' @param SXT is a Groups (rows) X Traits (columns) matrix
#' @param ngroups is an integer (nb: NJM's interpretation)
#' @param ntraits is an integer (nb: NJM's interpretation)
#' @param const is a vector of the constraint values (means, variances)
#' @param prior is the prior distribution
#' @param prob is the return vector of the maximum entropy
#' @param entropy is the maximum entropy probabilities
#' @param niter is the number of iterations required
#' @param tol is the convergence tolerance value; tolerance is mean square difference
#' @param denom are final moments
#'
#' The itscale5 function is in the "itscale5.f" FORTRAN file.  itscale5 is used by
#' the FD::maxent function.
#' 
#' The maxent function is used by BioGeoBEARS, merely to provide a simple method 
#' of putting flat or skewed probability distributions on the ordered categorical variable
#' "size of smaller daughter range"). 
#'
#' As the package FD has a number of other dependencies, some of which cause problems on 
#' some machines, I am just including maxent and itscale5 here, in order to avoid 
#' "dependency hell".
#'
#' I am putting it in rexpokit rather than BioGeoBEARS, to make rexpokit the only
#' package using FORTRAN code (which has a list of its own issues).
#' 
#' @return \code{res} A list of outputs
#' @seealso \code{\link{maxent}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # See maxent() function
#'
#' test=1
#' 
expokit_itscale5_wrapper <- function(SXT, ngroups, ntraits, const, prior, prob, entropy, niter, tol, denom)
{
	res2 = NULL
	
	res2 <- .C("itscale5_", as.double(SXT), as.integer(ngroups), as.integer(ntraits), as.double(const), as.double(prior), as.double(prob), as.double(entropy), as.integer(niter), as.double(tol), as.double(denom))
	
	#tmpoutmat = matrix(res2[[10]], nrow=n, byrow=TRUE)
	
	return(res2)
}



#' Run EXPOKIT's dgexpv on one or more t-values
#'
#' The function runs EXPOKIT's \code{dgexpv} function on a Q matrix and \emph{one or more} time values.  If \code{Qmat} is NULL (default), a default matrix is input.\cr
#'
#' NOTE: DGEXPV vs. DMEXPV. According to the EXPOKIT documentation, DGEXPV should be
#' faster than DMEXPV, however DMEXPV runs an accuracy check appropriate for
#' Markov chains, which is not done in DGEXPV.\cr
#' 
#' @param Qmat an input Q transition matrix
#' @param tvals one or more time values to exponentiate by (doesn't have to literally be a time value, obviously)
#' @param inputprobs_for_fast If NULL (default), the full probability matrix (Pmat) is returned. However, the full
#' speed of EXPOKIT on sparse matrices will be exploited if inputprobs_for_fast=c(starting probabilities). In this case
#' these starting probabilities are input to \code{myDMEXPV} directly, as \code{v}, and \code{w}, the output probabilities,
#' are returned.
#' @param transpose_needed If TRUE (default), matrix will be transposed (apparently
#' EXPOKIT needs the input matrix to be transposed compared to normal)
#' @param transform_to_coo_TF Should the matrix be tranposed to COO?  COO format is required
#' for EXPOKIT's sparse-matrix functions (like dmexpv and unlike the padm-related 
#' functions. Default TRUE; if FALSE, user must put a COO-formated matrix in \code{Qmat}. Supplying the
#' coo matrix is probably faster for repeated calculations on large matrices.
#' @param coo_n If a COO matrix is input, \code{coo_n} specified the order (# rows, equals # columns) of the matrix.
#' @param force_list_if_1_tval Default FALSE, but set to TRUE if you want a single matrix to be returned
#' inside a list
#' @param check_for_0_rows If TRUE or a numeric value, the input Qmat is checked for all-zero rows, since these will crash the FORTRAN wrapalldmexpv function. A small nonzero value set to check_for_0_rows or the default (0.0000000000001) is input to  off-diagonal cells in the row (and the diagonal value is normalized), which should fix the problem.
#' @return \code{tmpoutmat} the output matrix, if 1 t-value is input; \code{list_of_matrices_output},
#' if more than 1 t-value is input; to get a single output matrix in a list, set \code{force_list_if_1_tval=TRUE}
#' @seealso \code{\link{expokit_dgexpv_wrapper}}
#' @seealso \code{\link{expokit_dgexpv_Qmat}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com} and Drew Schmidt \email{schmidt@@math.utk.edu}
#' @examples # Example:
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' 
#' # Exponentiate each with EXPOKIT's dgexpv (should be fast for large sparse matrices)
#' for (t in tvals)
#' 	{
#' 	Pmat = expokit_dgexpv_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
#' 	cat("\n\nTime=", t, "\n", sep="")
#' 	print(Pmat)
#' 	}
#'
#' # DMEXPV and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
#' # DMEXPV is specifically designed for Markov chains and so may be slower, but more accurate.
#' 
#' # DMEXPV, single t-value
#' 
#' # DGEXPV, single t-value
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=2)
#' 
#' # These functions runs the for-loop itself (sadly, we could not get mapply() to work
#' # on a function that calls dmexpv/dgexpv), returning a list of probability matrices.
#' 
#' # DGEXPV functions
#' list_of_P_matrices_dgexpv = expokit_wrapalldgexpv_tvals(Qmat=Qmat, 
#' tvals=tvals, transpose_needed=TRUE)
#' list_of_P_matrices_dgexpv
#'
expokit_wrapalldgexpv_tvals <- function(Qmat=NULL, tvals=c(2.1), inputprobs_for_fast=NULL, transpose_needed=TRUE, transform_to_coo_TF=TRUE, coo_n=NULL, force_list_if_1_tval=FALSE, check_for_0_rows=TRUE)
{
	defaults = '
	Qmat=NULL
	t = 2.1
	inputprobs_for_fast=NULL
	transpose_needed=TRUE
	COO_needed=TRUE
	transform_to_coo_TF=TRUE
	coo_n=NULL
	force_list_if_1_tval=FALSE
	check_for_0_rows=FALSE
	check_for_0_rows=1e-15
	'
	
	# Check if Qmat is blank
	if (is.null(Qmat))
	{
		# Default Qmat
		warning("You supplied no matrix, so a default matrix is being used. Obviously you can't use this for anything real. YOU HAVE BEEN WARNED!!")
		
		Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	}
	
	if (is.null(tvals))
	{
		warning("You supplied no time values, so default time values are being used. Obviously you can't use this for anything real. YOU HAVE BEEN WARNED!!")
		tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
	}

	matvec = Qmat
	
	# Zero rows will crash the FORTRAN wrapalldgexpv function, and
	# therefore crash R.  This is annoying.
	if (is.null(inputprobs_for_fast))
	{ # Return the full Pmat (slow)
		#######################################################
		# Return the Pmat
		#######################################################
		
		# Zero rows will crash the FORTRAN wrapalldgexpv function, and
		# therefore crash R.  This is annoying.
		if (check_for_0_rows != FALSE)
		{
			# If not false, check_for_0_rows is either TRUE or numeric

			# Get T/F for rows with all zeros
			rows_w_all_zeros_TF = findrows_w_all_zeros(matvec)
			
			# If all FALSE, do nothing
			if (all(rows_w_all_zeros_TF == FALSE))
			{
				# Do nothing
				pass = 1
			} else {
				# indices of TRUE
				rows_allzero_indices = seq(1, length(rows_w_all_zeros_TF), 1)[rows_w_all_zeros_TF]

				# Here, you have to input a small value for each zero
				if (is.numeric(check_for_0_rows))
				{
					check_for_0_rows = check_for_0_rows
				} else {
					# 1e-15 appears to be the precision limit of the FORTRAN code
					check_for_0_rows = 1e-15
				}
				# Input the given value into all zeros
				newrowvals = rep(check_for_0_rows, ncol(matvec))
				matvec[rows_allzero_indices, ] = newrowvals
				diagonal_val = -1 * sum(newrowvals[-1])
				matvec[rows_allzero_indices, rows_allzero_indices] = diagonal_val
				
				warning(paste(sum(rows_w_all_zeros_TF), " rows of the Q matrix Qmat had all zeros. This will crash .Call('wrapalldgexpv_', ...)\nand therefore expokit_wrapalldgexpv_tvals() run with the inputprobs_for_fast=NULL option (producing a full Pmat),\nand therefore R.  Replacement value for 0:  check_for_0_rows=", check_for_0_rows, ".\n", sep=""))
			}
		} 
	}	

		
	# Count the number of NON-zeros (nz)
	# and input the matrix size
	if (transform_to_coo_TF == TRUE)
	{
		# COO format
		# http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29

		# number of non-zeros
		nz  = sum(matvec != 0)
		
		# These vectors have that length
		ia  = integer(length=nz)
		ja  = integer(length=nz)
		a   = double(length=nz)	
		n=nrow(matvec)
	} else {
		n = coo_n
		# (And make a regular matrix from COO)

		# number of non-zeros
		# Assumes that coo-formatted matrix columns are
		# ia, ja, a
		nz  = sum(matvec[,"a"] != 0)
	}
	
	

	#######################################################
	ideg = as.integer(6)
	#######################################################
	#n=nrow(Qmat)	# don't do this here, you might have a coo matrix
	m=n-1
	# t=as.numeric(2.1)
	
	# v should have as many elements as n; first element = 1 (?)
	if (is.null(inputprobs_for_fast))
	{
		# Input space-fillers, these get written over by wrapall
		v=double(n)
		v[1] = 1
		# Input the input probabilities, these get used directly by myDMEXPV/myDGEXPV
	} else {
		v = double(n)
		v = inputprobs_for_fast
	}
	
	# w is the same length
	w = double(length=n)
	tol=as.numeric(0.01)
	
	# length of wsp
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 4*(m+2)^2+ideg+1)
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 5*(m+2)^2+ideg+1)
	lwsp = as.integer(n*(m+2)+5*(m+2)^2+ideg+1)
	
	#lwsp = 100
	wsp = double(length=lwsp)
	
	# length of iwsp
	liwsp = max(m+2, 7)
	iwsp = integer(length=liwsp)
	

	if ((transform_to_coo_TF == TRUE) && (transpose_needed == TRUE))
	{
		tmatvec = t(matvec)
		#rowSums(tmatvec)
		#colSums(tmatvec)
	}


	
	# type="O" is being used here, this is supposed to be the
	# default for norm(), although it throws an error if not
	# specified
	# 
	# From the help:
	# type - character string, specifying the type of matrix norm to be
	# computed. A character indicating the type of norm desired. 
	# 	"O", "o" or "1"
	# 		specifies the one norm, (maximum absolute column sum);
	if ((exists("anorm") == FALSE) || is.null(anorm))
	{
		# Use the 1-norm or one-norm
		if (transform_to_coo_TF==FALSE && transpose_needed==FALSE)
		{
			tmpQmat1 = coo2mat(matvec, n=coo_n)
			tmpQmat2 = t(tmpQmat1)
			anorm = as.numeric(norm(tmpQmat2, type="O"))
		} else {
			anorm = as.numeric(norm(matvec, type="O"))
		}
	}	
	
	itrace = 0
	iflag = 0	
	
	
	#a = as.numeric(tmatvec)
	#a = as.numeric(matvec)
	if (transform_to_coo_TF == TRUE)
	{		
		tmpmat_in_REXPOKIT_coo_fmt = mat2coo(tmatvec)
	} else {
		tmpmat_in_REXPOKIT_coo_fmt = matvec
	}
	# Either way, store the rows/columns in the input variables for FORTRAN
	ia = tmpmat_in_REXPOKIT_coo_fmt[,"ia"]
	ja = tmpmat_in_REXPOKIT_coo_fmt[,"ja"]
	a = tmpmat_in_REXPOKIT_coo_fmt[,"a"]

	num_tvals = length(tvals)


	# Run the wrapper function	
	if (is.null(inputprobs_for_fast))
	{ # Return the full Pmat (slow)
		#######################################################
		# Return the Pmat
		#######################################################
		
		# Create the space for res (the returned Pmat)
		res = double(length=n*n)

	
		# If there is more than 1 t-value, or if the user desires a list even for a single
		# t-value, return a list
		if ((num_tvals > 1) || (force_list_if_1_tval==TRUE))
		{

			# Loop through the list of tvals, get the prob. matrix for each
			# sadly, mapply() etc. crash when tried on expokit_dgexpv_wrapper
			
			# Set up empty matrix
			NA_matrix = matrix(NA, nrow=n, ncol=n)
			
			# Set up list of empty matrices
			list_of_matrices_output = replicate(NA_matrix, n=num_tvals, simplify=FALSE)
			
			for (i in 1:num_tvals)
			{
				timeval = tvals[i]
				list_of_matrices_output[[i]] = expokit_dgexpv_wrapper(n, m, timeval, v, w, tol, anorm, wsp, lwsp, iwsp, liwsp, itrace, iflag, ia, ja, a, nz, res)
	
			} # end forloop
			
			return(list_of_matrices_output)
			
		} else {
			# If there is only 1 t value, just return 1 matrix
			#res2 <- .C("wrapalldgexpv_", as.integer(n), as.integer(m), as.double(t), as.double(v), as.double(w), as.double(tol), as.double(anorm), as.double(wsp), as.integer(lwsp), as.integer(iwsp), as.integer(liwsp), as.integer(itrace), as.integer(iflag), as.integer(ia), as.integer(ja), as.double(a), as.integer(nz), as.double(res))
			
			#tmpoutmat = matrix(res2[[18]], nrow=n, byrow=TRUE)
	
			timeval=tvals
			output_Pmat = expokit_dgexpv_wrapper(n, m, timeval, v, w, tol, anorm, wsp, lwsp, iwsp, liwsp, itrace, iflag, ia, ja, a, nz, res)		
			
			#print(tmpoutmat)
			return(output_Pmat)
		}
	} else {
		#######################################################
		# Return the output probabilities
		#######################################################
				
		# Be sure to input the input probabilities
		v = inputprobs_for_fast

		# If there is more than 1 t-value, or if the user desires a list even for a single
		# t-value, return a list
		if ((num_tvals > 1) || (force_list_if_1_tval==TRUE))
		{
			# Loop through the list of tvals, get the prob. matrix for each
			# sadly, mapply() etc. crash when tried on expokit_dgexpv_wrapper
			
			# Set up empty matrix
			list_of_outprobs_output = matrix(NA, nrow=num_tvals, ncol=n)

			for (i in 1:num_tvals)
			{
				t = tvals[i]
			
				list_of_outprobs_output[i,] = expokit_mydgexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)
	
			}
		} else {
			
			# If there is only 1 t value, just return 1 matrix
	    
	    w_output_probs <- expokit_mydgexpv_wrapper(n=n, m=m, t=tvals, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)
	    
	    # FIXME ???
			list_of_outprobs_output[1,] = w_output_probs
	
			return(w_output_probs)
		}

		return(list_of_matrices_output)
	}
}




#' Run EXPOKIT's dmexpv on one or more t-values
#'
#' The function runs EXPOKIT's \code{dmexpv} function on a Q matrix and \emph{one or more} time values.  If \code{Qmat} is NULL (default), a default matrix is input.
#'
#' @param Qmat an input Q transition matrix
#' @param tvals one or more time values to exponentiate by (doesn't have to literally be a time value, obviously)
#' @param inputprobs_for_fast If NULL (default), the full probability matrix (Pmat) is returned. However, the full
#' speed of EXPOKIT on sparse matrices will be exploited if inputprobs_for_fast=c(starting probabilities). In this case
#' these starting probabilities are input to \code{myDMEXPV} directly, as \code{v}, and \code{w}, the output probabilities,
#' are returned.
#' @param transpose_needed If TRUE (default), matrix will be transposed (apparently
#' EXPOKIT needs the input matrix to be transposed compared to normal)
#' @param transform_to_coo_TF Should the matrix be tranposed to COO?  COO format is required
#' for EXPOKIT's sparse-matrix functions (like dmexpv and unlike the padm-related 
#' functions. Default TRUE; if FALSE, user must put a COO-formated matrix in \code{Qmat}. Supplying the
#' coo matrix is probably faster for repeated calculations on large matrices.
#' @param coo_n If a COO matrix is input, \code{coo_n} specified the order (# rows, equals # columns) of the matrix.
#' @param force_list_if_1_tval Default FALSE, but set to TRUE if you want a single matrix to be returned
#' inside a list
#' @param check_for_0_rows If TRUE or a numeric value, the input Qmat is checked for all-zero rows, since these will crash the FORTRAN wrapalldmexpv function. A small nonzero value set to check_for_0_rows or the default (0.0000000000001) is input to  off-diagonal cells in the row (and the diagonal value is normalized), which should fix the problem.
#' @return \code{tmpoutmat} the output matrix, if 1 t-value is input; \code{list_of_matrices_output},
#' if more than 1 t-value is input; to get a single output matrix in a list, set \code{force_list_if_1_tval=TRUE}
#' @seealso \code{\link{expokit_dmexpv_wrapper}}
#' @seealso \code{\link{expokit_dmexpv_Qmat}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com} and Drew Schmidt \email{schmidt@@math.utk.edu}
#' @examples
#' # Make a square instantaneous rate matrix (Q matrix)
#' # This matrix is taken from Peter Foster's (2001) "The Idiot's Guide
#' # to the Zen of Likelihood in a Nutshell in Seven Days for Dummies,
#' # Unleashed" at:
#' # \url{http://www.bioinf.org/molsys/data/idiots.pdf}
#' #
#' # The Q matrix includes the stationary base freqencies, which Pmat 
#' # converges to as t becomes large.
#' Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Make a series of t values
#' tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
#' 
#' # DMEXPV and DGEXPV are designed for large, sparse Q matrices (sparse = lots of zeros).
#' # DMEXPV is specifically designed for Markov chains and so may be slower, but more accurate.
#' 
#' # DGEXPV, single t-value
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=tvals[1], transpose_needed=TRUE)
#' expokit_wrapalldgexpv_tvals(Qmat=Qmat, tvals=2)
#' 
#' # This function runs the for-loop itself (sadly, we could not get mapply() to work
#' # on a function that calls dmexpv/dgexpv), returning a list of probability matrices.
#' 
#' # DGEXPV functions
#' list_of_P_matrices_dgexpv = expokit_wrapalldgexpv_tvals(Qmat=Qmat, 
#' tvals=tvals, transpose_needed=TRUE)
#' list_of_P_matrices_dgexpv
#' 
expokit_wrapalldmexpv_tvals <- function(Qmat=NULL, tvals=c(2.1), inputprobs_for_fast=NULL, transpose_needed=TRUE, transform_to_coo_TF=TRUE, coo_n=NULL, force_list_if_1_tval=FALSE, check_for_0_rows=TRUE)
{
	defaults = '
	Qmat=NULL
	t = 2.1
	inputprobs_for_fast=NULL
	transpose_needed=TRUE
	COO_needed=TRUE
	transform_to_coo_TF=TRUE
	coo_n=NULL
	force_list_if_1_tval=FALSE
	check_for_0_rows=FALSE
	check_for_0_rows=1e-15
	'
	
	# Check if Qmat is blank
	if (is.null(Qmat))
	{
		# Default Qmat
		warning("You supplied no matrix, so a default matrix is being used. Obviously you can't use this for anything real. YOU HAVE BEEN WARNED!!")
		
		Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	}
	
	if (is.null(tvals))
	{
		warning("You supplied no time values, so default time values are being used. Obviously you can't use this for anything real. YOU HAVE BEEN WARNED!!")
		tvals = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2, 5, 14)
	}

	matvec = Qmat
	
	# Zero rows will crash the FORTRAN wrapalldmexpv function, and
	# therefore crash R.  This is annoying.
	if (is.null(inputprobs_for_fast))
	{ # Return the full Pmat (slow)
		#######################################################
		# Return the Pmat
		#######################################################
		
		# Zero rows will crash the FORTRAN wrapalldmexpv function, and
		# therefore crash R.  This is annoying.
		if (check_for_0_rows != FALSE)
		{
			# If not false, check_for_0_rows is either TRUE or numeric

			# Get T/F for rows with all zeros
			rows_w_all_zeros_TF = findrows_w_all_zeros(matvec)
			
			# If all FALSE, do nothing
			if (all(rows_w_all_zeros_TF == FALSE))
			{
				# Do nothing
				pass = 1
			} else {
				# indices of TRUE
				rows_allzero_indices = seq(1, length(rows_w_all_zeros_TF), 1)[rows_w_all_zeros_TF]

				# Here, you have to input a small value for each zero
				if (is.numeric(check_for_0_rows))
				{
					check_for_0_rows = check_for_0_rows
				} else {
					# 1e-15 appears to be the precision limit of the FORTRAN code
					check_for_0_rows = 1e-15
				}
				# Input the given value into all zeros
				newrowvals = rep(check_for_0_rows, ncol(matvec))
				matvec[rows_allzero_indices, ] = newrowvals
				diagonal_val = -1 * sum(newrowvals[-1])
				matvec[rows_allzero_indices, rows_allzero_indices] = diagonal_val
				
				warning(paste(sum(rows_w_all_zeros_TF), " rows of the Q matrix Qmat had all zeros. This will crash .Call('wrapalldmexpv_', ...)\nand therefore expokit_wrapalldmexpv_tvals() run with the inputprobs_for_fast=NULL option (producing a full Pmat),\nand therefore R.  Replacement value for 0:  check_for_0_rows=", check_for_0_rows, ".\n", sep=""))
			}
		}
	}	

		
	# Count the number of NON-zeros (nz)
	# and input the matrix size
	if (transform_to_coo_TF == TRUE)
	{
		# COO format
		# http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29

		# number of non-zeros
		nz  = sum(matvec != 0)
		
		# These vectors have that length
		ia  = integer(length=nz)
		ja  = integer(length=nz)
		a   = double(length=nz)	
		n=nrow(matvec)
	} else {
		n = coo_n
		# (And make a regular matrix from COO)

		# number of non-zeros
		# Assumes that coo-formatted matrix columns are
		# ia, ja, a
		nz  = sum(matvec[,"a"] != 0)
	}
	
	

	#######################################################
	ideg = as.integer(6)
	#######################################################
	#n=nrow(Qmat)	# don't do this here, you might have a coo matrix
	m=n-1
	# t=as.numeric(2.1)
	
	# v should have as many elements as n; first element = 1 (?)
	if (is.null(inputprobs_for_fast))
	{
		# Input space-fillers, these get written over by wrapall
		v=double(n)
		v[1] = 1
		# Input the input probabilities, these get used directly by myDMEXPV/myDGEXPV
	} else {
		v = double(n)
		v = inputprobs_for_fast
	}
	
	tol=as.numeric(0.01)
	
	# length of wsp
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 4*(m+2)^2+ideg+1)
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 5*(m+2)^2+ideg+1)
	lwsp = as.integer(n*(m+2)+5*(m+2)^2+ideg+1)
	
	#lwsp = 100
	wsp = double(length=lwsp)
	
	# length of iwsp
	liwsp = max(m+2, 7)
	iwsp = integer(length=liwsp)
	

	if ((transform_to_coo_TF == TRUE) && (transpose_needed == TRUE))
	{
		tmatvec = t(matvec)
		#rowSums(tmatvec)
		#colSums(tmatvec)
	}


	
	# type="O" is being used here, this is supposed to be the
	# default for norm(), although it throws an error if not
	# specified
	# 
	# From the help:
	# type - character string, specifying the type of matrix norm to be
	# computed. A character indicating the type of norm desired. 
	# 	"O", "o" or "1"
	# 		specifies the one norm, (maximum absolute column sum);
	if ((exists("anorm") == FALSE) || is.null(anorm))
	{
		# Use the 1-norm or one-norm
		if (transform_to_coo_TF==FALSE && transpose_needed==FALSE)
		{
			tmpQmat1 = coo2mat(matvec, n=coo_n)
			tmpQmat2 = t(tmpQmat1)
			anorm = as.numeric(norm(tmpQmat2, type="O"))
		} else {
			anorm = as.numeric(norm(matvec, type="O"))
		}
	}	
	
	itrace = 0
	iflag = 0	
	
	
	#a = as.numeric(tmatvec)
	#a = as.numeric(matvec)
	if (transform_to_coo_TF == TRUE)
	{		
		tmpmat_in_REXPOKIT_coo_fmt = mat2coo(tmatvec)
	} else {
		tmpmat_in_REXPOKIT_coo_fmt = matvec
	}
	# Either way, store the rows/columns in the input variables for FORTRAN
	ia = tmpmat_in_REXPOKIT_coo_fmt[,"ia"]
	ja = tmpmat_in_REXPOKIT_coo_fmt[,"ja"]
	a = tmpmat_in_REXPOKIT_coo_fmt[,"a"]

	num_tvals = length(tvals)


	# Run the wrapper function	
	if (is.null(inputprobs_for_fast))
	{ # Return the full Pmat (slow)
		#######################################################
		# Return the Pmat
		#######################################################
		
		# Create the space for res (the returned Pmat)
		res = double(length=n*n)

	
		# If there is more than 1 t-value, or if the user desires a list even for a single
		# t-value, return a list
		if ((num_tvals > 1) || (force_list_if_1_tval==TRUE))
		{

			# Loop through the list of tvals, get the prob. matrix for each
			# sadly, mapply() etc. crash when tried on expokit_dmexpv_wrapper
			
			# Set up empty matrix
			NA_matrix = matrix(NA, nrow=n, ncol=n)
			
			# Set up list of empty matrices
			list_of_matrices_output = replicate(NA_matrix, n=num_tvals, simplify=FALSE)
			
			for (i in 1:num_tvals)
				{
				t = tvals[i]
				list_of_matrices_output[[i]] = expokit_dmexpv_wrapper(n=n, m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)
	
				} # end forloop
			
			return(list_of_matrices_output)
			
		} else {
			# If there is only 1 t value, just return 1 matrix
			#res2 <- .C("wrapalldmexpv_", as.integer(n), as.integer(m), as.double(t), as.double(v), as.double(w), as.double(tol), as.double(anorm), as.double(wsp), as.integer(lwsp), as.integer(iwsp), as.integer(liwsp), as.integer(itrace), as.integer(iflag), as.integer(ia), as.integer(ja), as.double(a), as.integer(nz), as.double(res))
			
			#tmpoutmat = matrix(res2[[18]], nrow=n, byrow=TRUE)
	
			t=tvals
			output_Pmat = expokit_dmexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)		
			
			#print(tmpoutmat)
			return(output_Pmat)
		}
	} else {
		#######################################################
		# Return the output probabilities
		#######################################################
				
		# Be sure to input the input probabilities
		v = inputprobs_for_fast

		# If there is more than 1 t-value, or if the user desires a list even for a single
		# t-value, return a list
		if ((num_tvals > 1) || (force_list_if_1_tval==TRUE))
		{
			# Loop through the list of tvals, get the prob. matrix for each
			# sadly, mapply() etc. crash when tried on expokit_dmexpv_wrapper
			
			# Set up empty matrix
			list_of_outprobs_output = matrix(NA, nrow=num_tvals, ncol=n)

			for (i in 1:num_tvals)
			{
				t = tvals[i]
			
				# This must be mydmexpv_, not myDMEXPV_ !!!!

				w_output_probs <- expokit_mydmexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, itrace=itrace, iflag=iflag, ia=ia, ja=ja, a=a, nz=nz)
				
				list_of_outprobs_output[i,] = w_output_probs
	
			}
		} else {
			
			# If there is only 1 t value, just return 1 matrix
			#res2 <- .C("wrapalldmexpv_", as.integer(n), as.integer(m), as.double(t), as.double(v), as.double(w), as.double(tol), as.double(anorm), as.double(wsp), as.integer(lwsp), as.integer(iwsp), as.integer(liwsp), as.integer(itrace), as.integer(iflag), as.integer(ia), as.integer(ja), as.double(a), as.integer(nz), as.double(res))
			
			#tmpoutmat = matrix(res2[[18]], nrow=n, byrow=TRUE)
	
			t=tvals

			# This must be mydmexpv_, not myDMEXPV_ !!!!

			w_output_probs <- expokit_mydmexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, itrace=itrace, iflag=iflag, ia=ia, ja=ja, a=a, nz=nz)
      
			list_of_outprobs_output[1,] = w_output_probs
	
			return(w_output_probs)
		}

		return(list_of_matrices_output)
	}
}






rexpokit_as_coo <- function(x)
{
  if (!is.double(x))
    storage.mode(x) <- "double"
  
  ret <- .Call(R_rexpokit_as_coo, x)
  colnames(ret) <- c("ia", "ja", "a")
  
  return(ret)
}





