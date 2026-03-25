#' @include fermat.R
#' @include rexpokit-package.R
 
#sourcedir = '/Dropbox/_njm/'
#source3 = '_genericR_v1.R'
#source(paste(sourcedir, source3, sep=""))
#roxygenize()

# Re-source this R code after editing, without reinstalling from scratch:
# sourcedir = "/Dropbox/_njm/__packages/rexpokit_setup/"
# source8 = 'rexpokit_v1.R'
# source(paste(sourcedir, source8, sep=""))



# Original source:
# 
#sourcedir = "/Dropbox/_njm/"
#source8 = '_matrix_utils_v1.R'
#source(paste(sourcedir, source8, sep=""))

# for e.g. calc_loglike
# sourcedir = '/Dropbox/_njm/'
# source3 = '_R_tree_functions_v1.R'
# source(paste(sourcedir, source3, sep=""))


#######################################################
# EXPOKIT-RELATED FUNCTIONS
#######################################################

#' EXPOKIT dgpadm matrix exponentiation on Q matrix
#'
#' This function exponentiates a matrix via the EXPOKIT padm function
#' (designed for small dense matrices) and wrapper function 
#' \code{wrapalldgpadm_} around dmexpv.\cr
#'
#' From EXPOKIT:\cr
#'
#' \code{*     Computes exp(t*H), the matrix exponential of a general matrix in }\cr
#' \code{*     full, using the irreducible rational Pade approximation to the   }\cr
#' \code{*     exponential function exp(x) = r(x) = (+/-)( I + 2*(q(x)/p(x)) ), }\cr
#' \code{*     combined with scaling-and-squaring.                              }\cr
#'
#' If \code{Qmat} is NULL (default), a default matrix is input.\cr
#'
#' @param Qmat an input Q transition matrix
#' @param t one or more time values to exponentiate by
#' @param transpose_needed If TRUE (default), matrix will be transposed (apparently
#' EXPOKIT needs the input matrix to be transposed compared to normal)
#' @return \code{tmpoutmat} the output matrix. \code{wrapalldmexpv_} produces
#' additional output relating to accuracy of the output matrix etc.; these can be
#' obtained by a direct call of wrapalldmexpv_.
#' @seealso \code{\link{mat2coo}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com} and Drew Schmidt \email{schmidt@@math.utk.edu}
#' @examples	# Example:
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
#' # Exponentiate each with EXPOKIT's dgpadm (good for small dense matrices)
#' for (t in tvals)
#' 	{
#' 	Pmat = expokit_dgpadm_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
#' 	cat("\n\nTime=", t, "\n", sep="")
#' 	print(Pmat)
#' 	}
#' 
expokit_dgpadm_Qmat <- function(Qmat=NULL, t=2.1, transpose_needed=TRUE)
{
	defaults = '
	Qmat=NULL
	t = 2.1
	transpose_needed=TRUE
	'
	
	# Check if Qmat is blank
	if (is.null(Qmat))
	{
		# Default Qmat
		warning("expokit_dgpadm_Qmat() was provided a Qmat with value NULL.  Example Qmat provided instead")
		Qmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	}
	# Check if t is blank
	if (is.null(t))
	{
		# Default Qmat
		stop("\nSTOP ERROR: expokit_dgpadm_Qmat() was provided a t (time or times list) with value NULL.  \n")
	}
	
	# FOR DGPADM
	# ideg = 
	# "(input) the degree of the diagonal Pade to be used.
	# a value of 6 is generally satisfactory."
	ideg = as.integer(6)
  
	# Order (numrows/numcols) of the matrix
	# "(input) order of H."
	m = as.integer(nrow(Qmat))

	# output matrix
	res = double(length=m*m)

	# Prepare input matrix
	matvec = Qmat
	if (transpose_needed == TRUE)
	{
		tmatvec = t(matvec)
		H = as.numeric(tmatvec)
	} else {
		H = as.numeric(matvec)
	}
	
	# "H(ldh,m)  : (input) argument matrix."
	# (ldh = numrows and m is numcols, or something)
	ldh = m
	
	if (!is.double(H))
	  storage.mode(H) <- "double"
	
	res <- .Call(R_dgpadm, as.integer(ideg), as.integer(m), as.double(t), H, as.integer(ldh))
	
	output = res$wsp
	ind <- res$ind
	output_Pmat_is = seq(ind, ind+m*m-1, by=1)
	output_Pmat = output[output_Pmat_is]
	output_Pmat = matrix(output_Pmat, nrow=m, byrow=TRUE)
	
	return(output_Pmat)
}







#' EXPOKIT dmexpv matrix exponentiation on Q matrix
#'
#' This function converts a matrix to COO format and exponentiates
#' it via the EXPOKIT dmexpv function (designed for sparse matrices)
#' and wrapper functions \code{wrapalldmexpv_} around dmexpv.
#'
#' From EXPOKIT:\cr
#' \code{*     The method used is based on Krylov subspace projection}\cr
#' \code{*     techniques and the matrix under consideration interacts only}\cr
#' \code{*     via the external routine 'matvec' performing the matrix-vector} \cr
#' \code{*     product (matrix-free method).}\cr
#' \code{*}\cr
#' \code{*     This is a customised version for Markov Chains. This means that a}\cr
#' \code{*     check is done within this code to ensure that the resulting vector} \cr
#' \code{*     w is a probability vector, i.e., w must have all its components }\cr
#' \code{*     in [0,1], with sum equal to 1. This check is done at some expense}\cr
#' \code{*     and the user may try DGEXPV which is cheaper since it ignores }\cr
#' \code{*     probability constraints.}\cr
#'
#' COO (coordinated list) format is a compressed format that is\cr
#' required for EXPOKIT's sparse-matrix functions (like dmexpv and\cr
#' unlike EXPOKIT's padm-related functions.\cr
#'
#' COO (coordinated list) format is described here:\cr
#'
#' \url{http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr
#'
#' If \code{Qmat} is NULL (default), a default matrix is input.\cr
#'
#' @param Qmat an input Q transition matrix
#' @param t one or more time values to exponentiate by
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
#' @param anorm \code{dmexpv} requires an initial guess at the norm of the matrix. Using the
#' @param check_for_0_rows If TRUE or a numeric value, the input Qmat is checked for all-zero rows, since these will crash the FORTRAN wrapalldmexpv function. A small nonzero value set to check_for_0_rows or the default (0.0000000000001) is input to  off-diagonal cells in the row (and the diagonal value is normalized), which should fix the problem.
#' R function \code{\link{norm}} might get slow with large matrices. If so, the user
#' can input a guess manually (\code{Lagrange} seems to just use 1 or 0, if I
#' recall correctly).
#' @return \code{tmpoutmat} the output matrix. \code{wrapalldmexpv_} produces
#' additional output relating to accuracy of the output matrix etc.; these can be
#' by a direct call of dmexpv.
#' @seealso \code{\link{mat2coo}}
#' @seealso \code{\link{expokit_dmexpv_wrapper}}
#' @seealso \code{\link{expokit_wrapalldmexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com} and Drew Schmidt \email{schmidt@@math.utk.edu}
#' @examples	# Example:
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
#' # Exponentiate each with EXPOKIT's dmexpv (should be fast for large sparse matrices)
#' for (t in tvals)
#' 	{
#' 	Pmat = expokit_dmexpv_Qmat(Qmat=Qmat, t=t, transpose_needed=TRUE)
#' 	cat("\n\nTime=", t, "\n", sep="")
#' 	print(Pmat)
#' 	}
#' 
expokit_dmexpv_Qmat <- function(Qmat=NULL, t=2.1, inputprobs_for_fast=NULL, transpose_needed=TRUE, transform_to_coo_TF=TRUE, coo_n=NULL, anorm=NULL, check_for_0_rows=TRUE)
{
	defaults = '
	Qmat=NULL
	t = 2.1
	inputprobs_for_fast=NULL
	transpose_needed=TRUE
	transform_to_coo_TF=TRUE
	coo_n=NULL
	anorm=NULL
	check_for_0_rows=FALSE
	check_for_0_rows=1e-15
	'
	
	matvec = Qmat
	
	# Check if Qmat is blank
	if (is.null(matvec))
	{
		# Default Qmat
		warning("expokit_dmexpv_Qmat() was provided a Qmat with value NULL.  Example Qmat provided instead")
		matvec = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	}
	# Check if t is blank
	if (is.null(t))
	{
		# Default Qmat
		stop("\nSTOP ERROR: expokit_dmexpv_Qmat() was provided a t (time or times list) with value NULL.  \n")
	}


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

	# ideg = degree of polynomial, 6 is usually considered sufficient
	ideg = as.integer(6)
	#n=nrow(Qmat)	# don't do here, possibly coo-formatted
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
	tol=as.numeric(0.01)
	
	# lwsp = length of wsp
	# wsp = workspace to hold various variables, cells of the matrix, etc.
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 4*(m+2)^2+ideg+1)
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 5*(m+2)^2+ideg+1)

	# 2017-10-23_NJM_edit -- produces NAs when result is
	# n=21700
	# m=21699
	# ideg=6
	# n*(m+2)+5*(m+2)^2+ideg+1
	# [1] 2825578712
	#lwsp = as.integer(n*(m+2)+5*(m+2)^2+ideg+1)
	lwsp = as.double(n*(m+2)+5*(m+2)^2+ideg+1)
	
	#lwsp = 100
	wsp = double(length=lwsp)
	
	# length of iwsp
	liwsp = max(m+2, 7)
	iwsp = integer(length=liwsp)
	
	#matvec = matrix(data=Q, nrow=n, byrow=TRUE)
	matvec = matvec
	
	# Don't transform if already coo
	if ((transform_to_coo_TF == TRUE) && (transpose_needed == TRUE))
	{
		tmatvec = t(matvec)
	} else {
		tmatvec = matvec
	}
	#rowSums(tmatvec)
	#colSums(tmatvec)
	
	# This might (?) get slow with large matrices -- doesn't seem to
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
	
	# Make the input COO matrix
	# COO = coordinate list format, useful for sparse matrices with lots of zeros:
	# http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29
	# ia = rownum in the matrix
	# ja = colnum in the matrix
	# a  = value of that cell
	
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

	
	# Return the full Pmat (slow)
	if (is.null(inputprobs_for_fast))
	{
		# However, this may be an inefficient use of the dmexpv sparse matrix capabilities (Hansen)
		# Try mydmexpv_ to just get the ancestral probabilities (w, res2[[5]])
		output_Pmat = expokit_dmexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)
		
		return(output_Pmat)
	} else {
	  # Instead of returning the full Pmat (slow), just return the output probabilities (fast)
		
		# Be sure to input the input probabilities
		v = inputprobs_for_fast
		
		# w, list item #5, contains the output probabilities
		w_output_probs = expokit_mydmexpv_wrapper(n=n, m=m, t=t, v=v, tol=tol, anorm=anorm, wsp=wsp, lwsp=lwsp, iwsp=iwsp, liwsp=liwsp, ia=ia, ja=ja, a=a, nz=nz)
		
		return(w_output_probs)
	}
}




#######################################################
# 
# NOTE: DGEXPV section.  Same code as dmexpv, but EXPOKIT's DGEXPV should be
# faster than DMEXPV, however DMEXPV runs an accuracy check appropriate for
# Markov chains, which is not done in DGEXPV.
#
#######################################################




#' EXPOKIT dgexpv matrix exponentiation on Q matrix
#'
#' This function converts a matrix to COO format and exponentiates
#' it via the EXPOKIT dgexpv function (designed for sparse matrices)
#' and wrapper functions \code{wrapalldgexpv_} around dgexpv.\cr
#'
#'
#' NOTE: DGEXPV vs. DMEXPV. According to the EXPOKIT documentation, DGEXPV should be
#' faster than DMEXPV, however DMEXPV runs an accuracy check appropriate for
#' Markov chains, which is not done in DGEXPV.\cr
#'
#' From EXPOKIT:\cr
#'
#' \code{*     The method used is based on Krylov subspace projection}\cr
#' \code{*     techniques and the matrix under consideration interacts only}\cr
#' \code{*     via the external routine 'matvec' performing the matrix-vector} \cr
#' \code{*     product (matrix-free method).}\cr
#' \code{*}\cr
#' \code{*     This [DMEXPV, not DGEXPV -- NJM] is a customised version for Markov Chains. This means that a}\cr
#' \code{*     check is done within this code to ensure that the resulting vector }\cr
#' \code{*     w is a probability vector, i.e., w must have all its components }\cr
#' \code{*     in [0,1], with sum equal to 1. This check is done at some expense}\cr
#' \code{*     and the user may try DGEXPV which is cheaper since it ignores }\cr
#' \code{*     probability constraints.}\cr
#'
#' I (NJM) have not noticed a difference between the outputs of these two functions, but it might
#' occur with large matrices.
#'
#' COO (coordinated list) format is a compressed format that is
#' required for EXPOKIT's sparse-matrix functions (like dgexpv and
#' unlike EXPOKIT's padm-related functions. COO format is described here:\cr
#'
#' \url{http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr
#'
#' If \code{Qmat} is NULL (default), a default matrix is input.\cr
#'
#' @param Qmat an input Q transition matrix
#' @param t a time value to exponentiate by
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
#' @param anorm \code{dgexpv} requires an initial guess at the norm of the matrix. Using the
#' R function \code{\link{norm}} might get slow with large matrices. If so, the user
#' can input a guess manually (\code{Lagrange} seems to just use 1 or 0, if I
#' recall correctly).
#' @param check_for_0_rows If TRUE or a numeric value, the input Qmat is checked for all-zero rows, since these will crash the FORTRAN wrapalldmexpv function. A small nonzero value set to check_for_0_rows or the default (0.0000000000001) is input to  off-diagonal cells in the row (and the diagonal value is normalized), which should fix the problem.
#' @return \code{tmpoutmat} the output matrix. \code{wrapalldgexpv_} produces
#' additional output relating to accuracy of the output matrix etc.; these can be
#' by a direct call of dgexpv.
#' @seealso \code{\link{mat2coo}}
#' @seealso \code{\link{expokit_dgexpv_wrapper}}
#' @seealso \code{\link{expokit_wrapalldgexpv_tvals}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com} and Drew Schmidt \email{schmidt@@math.utk.edu}
#' @examples 	# Example:
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
expokit_dgexpv_Qmat <- function(Qmat=NULL, t=2.1, inputprobs_for_fast=NULL, transpose_needed=TRUE, transform_to_coo_TF=TRUE, coo_n=NULL, anorm=NULL, check_for_0_rows=TRUE)
{
	defaults = '
	Qmat=NULL
	t = 2.1
	inputprobs_for_fast=NULL
	transpose_needed=TRUE
	transform_to_coo_TF=TRUE
	coo_n=NULL
	anorm=NULL
	check_for_0_rows=FALSE
	check_for_0_rows=1e-15
	'
	
	matvec = Qmat
	
	# Check if Qmat is blank
	if (is.null(matvec))
	{
		# Default Qmat
		warning("expokit_dgexpv_Qmat() was provided a Qmat with value NULL.  Example Qmat provided instead")
		matvec = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168,  0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	}
	# Check if t is blank
	if (is.null(t))
	{
		# Default Qmat
		stop("\nSTOP ERROR: expokit_dgexpv_Qmat() was provided a t (time or times list) with value NULL.  \n")
	}


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

	# ideg = degree of polynomial, 6 is usually considered sufficient
	ideg = as.integer(6)
	#n=nrow(Qmat)	# don't do here, possibly coo-formatted
	m=n-1
	# t=as.numeric(2.1)
	
	# v should have as many elements as n; first element = 1 (?)
	if (is.null(inputprobs_for_fast))
	{
		# Input space-fillers, these get written over by wrapall
		v=double(n)
		v[1] = 1
		# Input the input probabilities, these get used directly by myDGEXPV/myDGEXPV
	} else {
		v = double(n)
		v = inputprobs_for_fast
	}
	
	# w is the same length
	w = double(length=n)
	tol=as.numeric(0.01)
	
	# lwsp = length of wsp
	# wsp = workspace to hold various variables, cells of the matrix, etc.
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 4*(m+2)^2+ideg+1)
	#lwsp = as.integer(n*(m+1)+n+(m+2)^2 + 5*(m+2)^2+ideg+1)
	lwsp = as.integer(n*(m+2)+5*(m+2)^2+ideg+1)
	
	#lwsp = 100
	wsp = double(length=lwsp)
	
	# length of iwsp
	liwsp = max(m+2, 7)
	iwsp = integer(length=liwsp)
	
	#matvec = matrix(data=Q, nrow=n, byrow=TRUE)
	matvec = matvec
	
	# Don't transform if already coo
	if ((transform_to_coo_TF == TRUE) && (transpose_needed == TRUE))
	{
		tmatvec = t(matvec)
	} else {
		tmatvec = matvec
	}
	#rowSums(tmatvec)
	#colSums(tmatvec)
	
	# This might (?) get slow with large matrices -- doesn't seem to
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
	
	# The itrace flag, if set to 1, results in dgexpv printing some details of
	# the function's run to screen.
	itrace = 0
	iflag = 0	
	
	# Make the input COO matrix
	# COO = coordinate list format, useful for sparse matrices with lots of zeros:
	# http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29
	# ia = rownum in the matrix
	# ja = colnum in the matrix
	# a  = value of that cell
	
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

	# Run the wrapper function	
	if (is.null(inputprobs_for_fast))
	{
		######################################
		# Return the full Pmat (slow)
		######################################
		
		ret <- .Call(R_dgexpv, 
		           as.integer(n), as.integer(m), as.double(t), 
		           as.double(v), as.double(tol), 
		           as.double(anorm), as.double(wsp), as.integer(lwsp), 
		           as.integer(iwsp), as.integer(liwsp), 
		           as.integer(ia), as.integer(ja), 
		           as.double(a), as.integer(nz))
	
		# wrapalldgexpv_ returns all kinds of stuff, list item 18 is the P matrix
		# However, this may be an inefficient use of the dgexpv sparse matrix capabilities (Hansen)
		# Try mydgexpv_ to just get the ancestral probabilities (w, res2[[5]])
		output_Pmat = matrix(ret$res, nrow=n, byrow=TRUE)
		
		return(output_Pmat)
	} else {
		######################################
		# Instead of returning the full Pmat (slow), just return the output probabilities (fast)
		######################################
		
		# Be sure to input the input probabilities
		v = inputprobs_for_fast
		
		ret <- .Call(R_dgexpv, 
		           as.integer(n), as.integer(m), as.double(t), 
		           as.double(v), as.double(tol), 
		           as.double(anorm), as.double(wsp), as.integer(lwsp), 
		           as.integer(iwsp), as.integer(liwsp), 
		           as.integer(ia), as.integer(ja), 
		           as.double(a), as.integer(nz))
		
		# w, list item #5, contains the output probabilities

		# 2017-10-23_fix
		#w_output_probs = matrix(ret$w, ncol=n, byrow=TRUE)
		w_output_probs = ret
		
		return(w_output_probs)
	}
}


