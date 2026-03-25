#' Check if a row is all zeros
#'
#' Q matrices with all-zero rows will crash .Call(wrapalldmexpv_, ...) and .Call(wrapalldgexpv_, ...),
#' and therefore will crash expokit_wrapalldmexpv_tvals() and expokit_wrapalldgexpv_tvals() when 
#' these are set (the default) to return the full P matrix.  These functions work fine with
#' zero rows if \code{inputprobs_for_fast} is supplied, meaning that only the output probabilities
#' of each state are returned.
#'
#' @param tmprow A row of a Q transition matrix
#' @return \code{TRUE} if tmprow is all zeros, FALSE if not.
#' @seealso \code{\link{expokit_wrapalldmexpv_tvals}}
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
row_allzero_TF <- function(tmprow)
{
	return(all(tmprow == 0))
}



#' Check if a Q matrix has rows with all zeros
#'
#' Q matrices with all-zero rows will crash .Call(wrapalldmexpv_, ...) and .Call(wrapalldgexpv_, ...),
#' and therefore will crash expokit_wrapalldmexpv_tvals() and expokit_wrapalldgexpv_tvals() when 
#' these are set (the default) to return the full P matrix.  These functions work fine with
#' zero rows if \code{inputprobs_for_fast} is supplied, meaning that only the output probabilities
#' of each state are returned.
#'
#' @param matvec Q transition matrix
#' @return A list of TRUE/FALSE, as long as the number of rows. \code{TRUE}=the is all zeros, \code{FALSE}=the row has nonzero values.
#' @seealso \code{\link{expokit_wrapalldmexpv_tvals}}
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
findrows_w_all_zeros <- function(matvec)
{
	return(apply(X=matvec, MARGIN=1, FUN=row_allzero_TF))
}





#' Convert matrix to COO format using SparseM function
#'
#' Converts a matrix to COO format using the SparseM function, presumably this
#' is faster than using a for-loop.\cr
#'
#' \code{EXPOKIT}'s \code{dmexp}-type functions deal with sparse matrices.
#' These have a lot of zeros, and thus can be compressed
#' into COO (coordinated list) format, which is described here:\cr
#'
#' \url{http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr
#'
#' In \code{EXPOKIT} and its wrapper functions, a COO-formated matrix is input as
#' 3 vectors (first two integer, the third double):\cr
#'
#' ia = row number\cr
#' ja = column number\cr
#' a = value of that cell in the matrix (skipping 0 cells)\cr
#' 
#' @param tmpmat A square matrix
#' @return tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia}, \code{ja}, and \code{a} 
#' @seealso \code{\link{mat2coo_forloop}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # Example use:
#' @examples
#' # Make a Q matrix
#' tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Convert to coo format
#' tmpmat_in_REXPOKIT_coo_fmt = mat2coo(tmpmat)
#' tmpmat_in_REXPOKIT_coo_fmt
#' 
mat2coo <- function(tmpmat)
{
	defaults = '
	tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
	'
	
	numrows = nrow(tmpmat)
	numcols = ncol(tmpmat)

	if (numrows != numcols)
	{
		stop("ERROR! mat2coo(tmpmat) says that in tmpmat, nrows != ncols")
		return(NA)
	}

	numcells = numrows ^2
	
	# require(sfsmisc)
	# xy.grid
	
	x = 1:numrows
	y = 1:numrows

	# Seems to be slow when numrow > 1000
	# as.vector(tmpmat) appends col1vals, then col2vals, etc., so ji = xy
	# cells_ij = expand.grid(x, y)
	# tmpa = as.vector(tmpmat)
	# 
	# # Remove 0s
	# TF = tmpa != 0
	# ia = cells_ij[,1][TF]	
	# ja = cells_ij[,2][TF]	
	# a = tmpa[TF]

	#require(SparseM)	# required for the as.matrix.coo function
	
	# This produces a matrix in coo format
	# (this is an S4 object)
	tmpmat_in_REXPOKIT_coo_fmt <- rexpokit_as_coo(tmpmat)
	
	return(tmpmat_in_REXPOKIT_coo_fmt)
}



#' Convert a SparseM COO matrix to a plain matrix
#'
#' Converts a SparseM COO-formatted matrix (an S4 object) to a plain matrix, with \cr
#' column #1 = ia = i index\cr
#' column #2 = ja = j index\cr
#' column #3 = a = nonzero values of the matrix\cr
#'
#' Background: COO (coordinated list) format, is described here:\cr
#'
#' \url{http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr
#'
#' In \code{EXPOKIT} and its wrapper functions, a COO-formated matrix is input as
#' 3 vectors (first two integer, the third double):\cr
#'
#' ia = row number\cr
#' ja = column number\cr
#' a = value of that cell in the matrix (skipping 0 cells)\cr
#' 
#' @param tmpmat_in_SparseMcoo_fmt A square matrix S4 object derived from SparseM's as.matrix.coo
#' @return tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia}, \code{ja}, and \code{a} 
#' @seealso \code{\link{mat2coo_forloop}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # Example use:
#' # Make a Q matrix
#' tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Covert to SparseM coo format
#' tmpmat_in_SparseMcoo_fmt = SparseM::as.matrix.coo(tmpmat)
#' 
#' # Convert to REXPOKIT coo format
#' tmpmat_in_REXPOKIT_coo_fmt = SparseM_coo_to_REXPOKIT_coo(tmpmat_in_SparseMcoo_fmt)
#' tmpmat_in_REXPOKIT_coo_fmt
#' 
SparseM_coo_to_REXPOKIT_coo <- function(tmpmat_in_SparseMcoo_fmt)
{
	tmpcoo = tmpmat_in_SparseMcoo_fmt
	
	# We just need the 3 columns: i index, j index, and nonzero values
	tmpmat_in_REXPOKIT_coo_fmt = cbind(tmpcoo@ia, tmpcoo@ja, tmpcoo@ra)
	
	# Apply appropriate column names
	colnames(tmpmat_in_REXPOKIT_coo_fmt) = c("ia", "ja", "a")
	
	return(tmpmat_in_REXPOKIT_coo_fmt)
}




#' Convert a COO-formated matrix to standard square format
#'
#' \code{EXPOKIT}'s \code{dmexp}-type functions deal with sparse matrices.
#' These have a lot of zeros, and thus can be compressed
#' into COO (coordinated list) format, which is described here:\cr
#'
#' \url{http://en.wikipedia.org/wiki/Sparse_matrix#Coordinate_list_.28COO.29}\cr
#'
#' In \code{EXPOKIT} and its wrapper functions, a COO-formated matrix is input as
#' 3 vectors (first two integer, the third double):\cr
#'
#' ia = row number\cr
#' ja = column number\cr
#' a = value of that cell in the matrix (skipping 0 cells)\cr
#'
#' This function takes a 3-column matrix or data.frame (basically \code{cbind(ia, ja, a)})
#' and the order of the matrix, \code{n} (n = the order of the matrix, i.e. number of
#' rows/columns) and converts back to standard square format.\cr
#'
#' @param coomat a 3-column matrix or data.frame (basically \code{cbind(ia, ja, a)})
#' @param n the order of the matrix
#' @param transpose_needed If TRUE (default), matrix will be transposed (apparently
#' EXPOKIT needs the input matrix to be transposed compared to normal)
#' @return outmat
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # Example use:
#' ia = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4)
#' ja = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4)
#' a  = c(-1.218, 0.126, 0.168, 0.126, 0.504, -0.882, 0.504, 
#' 0.672, 0.336, 0.252, -1.050, 0.252, 0.378, 0.504, 0.378, -1.050)
#' coomat = cbind(ia, ja, a)
#' print(coomat)
#' n = 4
#' Qmat = coo2mat(coomat, n)
#' print(Qmat)
coo2mat <- function(coomat, n=max(max(coomat[,1]), max(coomat[,2])), transpose_needed=FALSE)
{
	defaults='
	
	'
	
	# Make an empty matrix of 0s
	outmat = matrix(double(length=n*n), nrow=n)
	
	# go through each row of coomat
	ia = coomat[,1]
	ja = coomat[,2]
	a = coomat[,3]
	
	if (transpose_needed == FALSE)
	{
		for (k in 1:length(ia))
		{
			#cat(ia[k], ja[k], a[k], "\n")
			outmat[ia[k], ja[k]] = a[k]
		}
	} else {
		for (k in 1:length(ia))
		{
			#cat(ia[k], ja[k], a[k], "\n")
			outmat[ja[k], ia[k]] = a[k]
		}		
	}
	
	return(outmat)
}



#' Convert matrix to COO format using nested for-loops
#'
#' Converts a matrix to COO format. This version of the function uses
#' for-loops, which is presumably less efficient than \code{\link{mat2coo}}.
#'
#' @param tmpmat A square matrix
#' @return tmpmat_in_REXPOKIT_coo_fmt A \code{cbind} of \code{ia}, \code{ja}, and \code{a} 
#' @seealso \code{\link{mat2coo}}
#' @export
#' @author Nicholas J. Matzke \email{nickmatzke.ncse@@gmail.com}
#' @examples # Example use:
#' # Make a Q matrix
#' tmpmat = matrix(c(-1.218, 0.504, 0.336, 0.378, 0.126, -0.882, 0.252, 0.504, 0.168, 
#' 0.504, -1.05, 0.378, 0.126, 0.672, 0.252, -1.05), nrow=4, byrow=TRUE)
#' 
#' # Convert to REXPOKIT coo format
#' tmpmat_in_REXPOKIT_coo_fmt = mat2coo_forloop(tmpmat)
#' tmpmat_in_REXPOKIT_coo_fmt
#' 
mat2coo_forloop <- function(tmpmat)
{
	# Number of non-zeros
	nz = sum(tmpmat != 0)
	
	# Blank columns for COO matrix
	ia = integer(length=nz)
	ja = integer(length=nz)
	a = integer(length=nz)
	
	count = 0
	for (i in 1:nrow(tmpmat))
	{
		for (j in 1:ncol(tmpmat))
		{
			if (tmpmat[i,j] != 0)
			{
				count = count+1
				ia[count] = i
				ja[count] = j
				a[count] = tmpmat[i,j]
			}
		} 
	}
	tmpmat_in_REXPOKIT_coo_fmt = cbind(ia, ja, a)
	return(tmpmat_in_REXPOKIT_coo_fmt)
}


