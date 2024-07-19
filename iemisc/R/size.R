#' Size of R objects (GNU Octave/MATLAB compatible)
#'
#' Provides the dimensions of R objects in a manner compatible with
#' GNU Octave/MATLAB. This function is the same as \code{\link[pracma]{size}}, except this
#' \code{size} can find the size of character vectors too. Some documentation from
#' \code{\link[pracma]{size}}.
#'
#'
#' @param x An R object (array, vector, or matrix)
#' @param k integer specifying a particular dimension
#'
#' @return "Return the number of rows and columns of the object x as a numeric
#'   vector. If given a second argument, \code{size} will return the size of the
#'   corresponding dimension." Source: Eaton.
#'
#'
#' @source
#' pracma size function definition - R package pracma created and maintained by Hans Werner Borchers. See \code{\link[pracma]{size}}.
#'
#'
#' @references
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Pages 47-48.
#'
#'
#'
#' @author Hans Werner Borchers (pracma size), Irucka Embry
#'
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @seealso \code{\link[base]{dim}}, \code{\link[pracma]{size}}
#'
#'
#' @examples
#' 
#' # Example from GNU Octave ndims function reference
#' 
#' size(matlab::ones(4, 1, 2, 1))
#' 
#'
#'
#'
#' @export
# Source 1 begins
size <- function (x, k) {

    if (length(x) == 0) {

        sz <- 0

    } else if (is.vector(x) & is.vector(x, mode = "character") == FALSE) {

    sz <- c(1, length(x))

	} else if (is.vector(x) & is.vector(x, mode = "character")) {

	sz <- c(1, nchar(x))

    } else if (all(is.array(x) & !is.matrix(x))) {

    ifelse(x[length(x)] == 1, sz <- dim(x)[1:length(dim(x))-1L], sz <- dim(x))
    
    } else if (is.matrix(x)) {
    
    sz <- dim(x)

    } else sz <- NULL
    

    if (!missing(k)) {

    if (k > length(sz)+1L) {

    sz <- 1

    } else if (k == 1) {
    
    sz <- nrow(x)
    
    } else if (k == 2) {
        
    ifelse(is.array(x), sz <- length(x) / nrow(x), sz <- ncol(x))
    
    } else if (k > 2) {

    sz <- sz[k]
    }
    
    else stop("Requested dimension 'k' is out of range.")

    }

    return(sz)
# Source 1 ends
    }



    
    
    

#' Length of R objects (GNU Octave/MATLAB compatible)
#'
#' Obtain the length of R objects [arrays, matrices, and vectors (including
#' lists)] in a manner compatible with GNU Octave/MATLAB. Some documentation
#' from \code{\link[base]{length}}.
#'
#'
#' @param x An R object (array, matrix, vector)
#'
#' @return Return the length of the object x as an integer. "The length is 0
#'   for empty objects, 1 for scalars (in R, a \code{vector} of \code{length} 1), and
#'   the number of elements (in R, the \code{length}) for vectors. For matrix objects,
#'   the length is the number of rows or columns, whichever is greater (this
#'   odd definition is used for compatibility with MATLAB)." Source: Eaton.
#'
#' @references
#' \enumerate{
#'    \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/inspection_length.html}.
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 47.
#' }
#'
#'
#' @author Irucka Embry, Samit Basu (FreeMat)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @seealso \code{\link[base]{length}}, \code{\link[base]{lengths}}, \code{\link[pracma]{size}}, \code{\link{size}}
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' import::from(matlab, ones)
#' 
#' # Example from pracma isempty
#' 
#' object1 <- matrix(0, 1, 0)
#' 
#' length_octave(object1)
#'
#'
#'
#' @importFrom sjmisc is_empty
#'
#' @export
length_octave <- function (x) {

if (is_empty(x))

  0

else if (length(is.vector(x)) == 1)

  1

else if (is.vector(x))

  length(x)

else (is.matrix(x))

  max(size(x))

  }


  
  
  
  

#' Number of elements (GNU Octave/MATLAB compatible)
#'
#' Obtain the number of elements of R objects [arrays, matrices, and vectors
#' (including lists)] in a manner compatible with GNU Octave/MATLAB. Some
#' documentation from \code{\link[base]{length}}.
#'
#'
#' @param x An R object (array, matrix, vector)
#' @param ... R objects (indices idx1, idx2, ...)
#'
#' @return "Return the number of elements in the R object x. Optionally, if
#'   indices idx1, idx2, ... are supplied, return the number of elements that
#'   would result from the indexing a(idx1, idx2, ...)." Source: Eaton page 41.
#'
#'
#' @source
#' \enumerate{
#'    \item r - Add a Column to a Dataframe From a List of Values - Stack Overflow answered by Matthew Plourde on Jun 21 2012. See \url{https://stackoverflow.com/questions/11130037/add-a-column-to-a-dataframe-from-a-list-of-values/11130178}.
#'    \item r - Why does is.vector() return TRUE for list? - Stack Overflow answered by Andrie on May 17 2011. See \url{https://stackoverflow.com/questions/6032772/why-does-is-vector-return-true-for-list/6032909}.
#' }
#'
#'
#' @references
#' \enumerate{
#'    \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/inspection_numel.html}.
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Pages 46-47.
#' }
#'
#'
#' @author Irucka Embry, Samit Basu (FreeMat)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#'
#' @seealso \code{\link[matlab]{numel}}, \code{\link[pracma]{numel}}, \code{\link{size}}, \code{\link[base]{length}}, \code{\link{length_octave}}
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' import::from(matlab, ones)
#' 
#' xx <- list(1:26, 1:10)
#' 
#' numel(xx)
#'
#' 
#'
#'
#'
#' @export
numel <- function (x, ...) {

if (nargs() == 1) {

  lens <- prod(size(x))

} else {

  varargin <- list(...)

  len <- 1

  lens <- vector("list", numel(varargin))
# Source 1 and 2 / pre-allocate the list since it is being used in a for loop

for (k in 1:numel(varargin)) {

  lens[[k]] <- len * size(varargin[[k]])
}

  lens <- prod(unlist(lens))

}

return(lens)

}












#' Number of dimensions in an Array (GNU Octave/MATLAB compatible)
#'
#' Obtain the number of dimensions of an array [arrays, matrices, and vectors]
#' in a manner compatible with GNU Octave/MATLAB.
#'
#'
#' @param x An array (array, matrix, vector)
#'
#' @return "Return the number of dimensions of a. For any array, the result
#'   will always be greater than or equal to 2. Trailing singleton dimensions
#'   are not counted." Source: Eaton page 46.
#'
#'
#'
#' @author Irucka Embry, Samit Basu (FreeMat)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @references
#' \enumerate{
#'    \item Samit Basu (2002-2006). FreeMat v4.0, \url{https://freemat.sourceforge.net/help/inspection_ndims.html}.
#'    \item John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 46.
#' }
#'
#' @seealso \code{\link{size}}
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples from GNU Octave ndims
#' 
#' b <- matlab::ones(c(4, 1, 2, 1))
#' 
#' ndims(b)
#'
#' 
#'
#'
#'
#' @export
ndims <- function (x) {

n <- length_octave(size(x))

return(n)
}
















#' Row Vector (GNU Octave/MATLAB compatible)
#'
#' Test for row vector that is compatible with GNU Octave/MATLAB.
#'
#'
#' @param x An array (array, matrix, vector)
#'
#' @return "Return true if x is a row vector. A row vector is a 2-D array for
#'   which size (x) returns [1, N] with non-negative N." Source: Eaton page 68.
#'
#'
#'
#' @author Irucka Embry, Rik Wehbring (GNU Octave), Colin B. Macdonald (OctSymPy)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @references
#'
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 68.
#'
#' @seealso \code{\link{iscolumn}}
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples
#' 
#' xx <- ramify::mat("1, 2"); xx
#' 
#' isrow(xx)
#' 
#' 
#' 
#'
#'
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#' @export
isrow <- function(x) {

assert_that(!any(all(qtest(x, "N+(,)")) == FALSE), msg = "Either x is NA, NaN, Inf, -Inf, empty, or a string. Or, there is not at least 1 argument. Please try again.")
# only process with finite values with a length greater than 1 and provide an error message if the check fails

sz <- size(x)

r <- (ndims(x) == 2 && (sz[1] == 1))

return(r)
}







#' Column Vector (GNU Octave/MATLAB compatible)
#'
#' Test for column vector that is compatible with GNU Octave/MATLAB.
#'
#'
#' @param x An array (array, matrix, vector)
#'
#' @return "Return true if x is a column vector. A column vector is a 2-D array
#'   for which size (x) returns [N, 1] with non-negative N." Source: Eaton page
#'   68.
#'
#'
#'
#' @author Irucka Embry, Rik Wehbring (GNU Octave), Colin B. Macdonald (OctSymPy)
#'
#'
#' @encoding UTF-8
#'
#'
#'
#' @references
#'
#' John W. Eaton, David Bateman, Søren Hauberg, and Rik Wehbring (November 2022). \emph{GNU Octave: A high-level interactive language for numerical computations: Edition 7 for Octave version 7.3.0}. \url{https://docs.octave.org/octave.pdf}. Page 68.
#'
#' @seealso \code{\link{isrow}}
#'
#'
#' @examples
#' 
#' library(iemisc)
#'
#' # Examples
#' 
#' xxx <- ramify::mat("1, 2"); xxx
#' 
#' iscolumn(xxx)
#' 
#' 
#' 
#'
#'
#'
#'
#'
#' @importFrom checkmate qtest
#' @importFrom assertthat assert_that
#'
#' @export
iscolumn <- function(x) {

assert_that(!any(all(qtest(x, "N+(,)")) == FALSE), msg = "Either x is NA, NaN, Inf, -Inf, empty, or a string. Or, there is not at least 1 argument. Please try again.")
# only process with finite values with a length greater than 1 and provide an error message if the check fails

sz <- size(x)

r <- (ndims(x) == 2 && (sz[2] == 1))

return(r)
}
