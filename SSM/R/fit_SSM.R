#' Fit a smooth supersaturated model
#'
#' \code{fit.ssm} fits a smooth supersaturated model to given data.  By default
#' the model smooths over \eqn{[-1, 1]^d} and uses a basis of Legendre
#' polynomials. Many model parameters such as basis size and smoothing
#' criterion can be user-defined. Optionally, sensitivity indices and a
#' metamodel error estimating Gaussian process can be computed.
#'
#' Returns an SSM object containing the fitted smooth
#' supersaturated model.  Minimal arguments required are \code{design} and
#' \code{response}.  This will result in a model using Legendre polynomials
#' and smoothing over \eqn{[-1, 1]^d}. All other arguments will be assigned
#' automatically if not specified.
#'
#' If the model is unable to be fit due to numerical instability a warning will
#' be given and the returned SSM object will have a zero vector of appropriate
#' length in the \code{theta} slot.  The \code{basis_size} parameter is often
#' useful in this situation.  Try reducing the basis_size until a model fit is
#' successful.  Ideally the basis size should be as large as possible without
#' causing instability in the predictions (see the example below).
#'
#' If \code{SA} is TRUE then sensitivty analysis will be performed on the model.
#' Sobol indices for main effects, Total indices, and Total interaction indices
#' for second order interactions will be computed and held in the slots
#' \code{main_sobol}, \code{total_sobol} and \code{total_int} respectively. If
#' the number of factors is < 11 then Sobol indices will be computed for all
#' order interactions and stored in \code{int_sobol}.  Default behaviour is to
#' assume each input is uniformly distributed over \eqn{[-1, 1]}.  If \code{P}
#' has been used-defined then the polynomials defined by \code{P} are assumed to
#' be an orthonormal system with respect to some measure on the reals.  See
#' \code{\link{update.sensitivity}} for more details.
#'
#' If \code{GP} is TRUE (default behaviour is false due to computational cost)
#' then a the metamodel error is estimated using a zero-mean Gaussian process
#' with a constant trend.  Scaling and length parameters are estimated using
#' maximum likelihood methods using the Leave-One-Out model errors and stored
#' in the \code{sigma} and \code{r} slots respectively.  Model predictions
#' using \code{\link{predict.SSM}} will then include credible intervals. The
#' distance between points is defined by the \code{distance_type} argument and
#' is set to the standard Euclidean distance by default.  See
#' \code{\link{new.distance}} for other options although they are experimental
#' and subject to erratic behaviour. The default correlation function used is
#' the square exponential although this can be changed to a Matern 3/2
#' function by setting the \code{type} argument to \code{"matern32"}.
#'
#' If \code{validation} is TRUE then the Leave-One_Out error at each design
#' point will be computed and stored in the \code{residuals} slot, and the LOO
#' RMSE computed and stored in the \code{LOO_RMSE} slot.  Note that if
#' \code{GP} is TRUE then these values will be computed regardless of the
#' value of \code{validation} as they are required to fit the metamodel error
#' estimate GP.
#'
#' @param design A matrix containing the design.  Each design point is a row in
#'   the matrix. Accepts a vector for a design in one variable.  If the default
#'   options for \code{P} and \code{K} are used then it is recommended that the
#'   design is transformed to lay within \eqn{[-1, 1]^d}.  The function
#'   \code{\link{transform11}} is useful in this regard.
#' @param response A vector containing the responses at design points.  The
#'   length must correspond with the number of rows in \code{design}.
#' @param ssm (optional) A pre-existing SSM class object.  If this argument is
#'   supplied then \code{basis}, \code{basis_size}, \code{K}, and \code{P} will
#'   be carried over rather than re-computed.  This is useful for simulation
#'   studies where the model structure remains the same and only the design and
#'   responses change.
#' @param basis (optional) A matrix where each row is an exponent vector of a
#'   monomial.  This is used in conjunction with \code{P} to construct the model
#'   basis.  If not supplied, a hierarchical basis will be used.
#' @param basis_size (optional) A number.  Specifies the desired number of
#'   polynomials in the model basis.  If not supplied, \code{basis_size} is set
#'   to \eqn{20 * d + n}.
#' @param K (optional) A semi-positive definite matrix specifying the weighting
#'   criterion of basis terms.  If not supplied, default behaviour is to use the
#'   Frobenius norm of the term Hessian integrated over \eqn{[-1, 1]^d} with
#'   respect to a basis of Legendre polynomials.
#' @param P (optional) A matrix defining the polynomials used to construct the
#'   model basis.  Each column corresponds to one polynomial. If not supplied, a
#'   Legendre polynomial basis is used.
#' @param design_model_matrix (optional) Specify a design model matrix.  If
#'   provided the new model will be fit to the basis and design implied by the
#'   design model matrix regardless of the values of \code{basis}, \code{P} and
#'   \code{design}.
#' @param SA (optional) Logical. If TRUE then Sobol indices, Total indices and
#'   Total interaction indices will be computed.
#' @param GP (optional) Logical. If TRUE then a GP metamodel error estimate will
#'   be computed.
#' @param type (optional) Character. One of "exp" or "matern32".  Specifies the
#'   correlation function used to compute the GP covariance matrix.  Irrelevant
#'   if \code{GP} is FALSE. For further details see
#'   \code{\link{compute.covariance}}.
#' @param validation (optional) Logical.  If TRUE then the Leave-One-Out errors
#'   are computed for each design point and the standardised root mean square
#'   error calculated.  The rmse is standardised against the variance of the
#'   ybar estimator. If \code{GP} is TRUE then these will be calculated
#'   regardless of the value of \code{validation}.
#' @param exclude (optional) A list of vectors of integers. These indicate terms
#'   in the listed variables should be omitted from the model. \emph{e.g.}
#'   \code{exclude = list(1)} removes all terms dependent on the first variable
#'   only. \code{exclude = list(1, c(1, 2))} removes terms in the first variable
#'   only and interactions between the first and second variables only. To
#'   remove a variable and all of its higher order interactions, it is better to
#'   remove the appropriate column from the design otherwise the algorithm will
#'   generate a lot of basis vectors that will be excluded, wasting computation.
#' @param distance_type (optional) Character. Selects the distance function used
#'   for the GP metamodel error estimate correlation function. One of
#'   "distance", "line", "product", "area", "proddiff", or "smoothdiff".  Uses
#'   "distance", the standard Euclidean distance between points by default.  For
#'   further details see \code{\link{new.distance}}.  Not needed if \code{GP} is
#'   FALSE.
#' @return An SSM object.
#' @export
#' @seealso \code{\link{predict.SSM}} for model predictions for SSM, and
#'   \code{\link{plot.SSM}} for plotting main effects of SSM.
#'   \code{\link{transform11}} is useful for transforming data to
#'   \eqn{[-1, 1]^d}.
#'
#' @examples
#' # A simple one factor example
#' X <- seq(-1,1,0.5) # design
#' Y <- c(0,1,0,0.5,0) # response
#' s <- fit.ssm(X,Y)
#' s
#' plot(s)
#' predict(s,0.3)
#'
#' # used defined basis sizes
#'
#' # A model that is too large to fit
#' \dontrun{
#' s <- fit.ssm(X, Y, basis_size=80)
#' }
#' # A large model that can be fit but is unstable
#' s <- fit.ssm(X, Y, basis_size=70)
#' plot(s)
#' # A model larger than default that is not unstable
#' s <- fit.ssm(X, Y, basis_size=40)
#' plot(s)
#'
#' # with metamodel error estimate
#'
#' s <- fit.ssm(X, Y, GP=TRUE)
#' plot(s)
#' predict(s,0.3)
#'
#' # Sensitivity analysis and main effect plotting
#'
#' # A design of 20 points over [-1, 1]^d
#' X <- matrix(runif(20, -1, 1), ncol = 2)
#' Y <- runif(10)
#' s <- fit.ssm(X, Y, SA = TRUE)
#' s
#' sensitivity.plot(s)
#' plot(s)
fit.ssm <- function(design, response, ssm, basis, basis_size, K, P,
                    design_model_matrix, SA = FALSE, GP=FALSE, type = "exp",
                    validation = FALSE, exclude=list(),
                    distance_type = "distance"){

  # Check validity of input and correct

  if(!is.vector(design) && !is.matrix(design) && !is.data.frame(design))
    stop("The design is required to be a vector (for one independent variable),
          a matrix or a data frame.")
  if(is.vector(design)) design <- matrix(design, ncol = 1)
  design <- as.matrix(design)
  if(missing(response)) stop("A response vector is required.")
  if(length(response) != nrow(design))
    stop("There are not the same number of responses as design points.")

  # Set up SSM object
  s <- SSM(design = design, response = response)
  s@dimension <- ncol(design)
  s@design_size <- length(response)
  s@distance_type <- distance_type

  # if ssm argument is provided, copy relevant slots
  if(!missing(ssm)){
    s@basis_size <- ssm@basis_size
    s@basis      <- ssm@basis
    s@K          <- ssm@K
    s@P          <- ssm@P
    s@include    <- ssm@include
  } else {
  # otherwise check arguments or compute as necessary
    if(!missing(basis)){
      s@basis <- basis
      s@basis_size <- nrow(s@basis)
    } else {
      if(!missing(basis_size)){
        s@basis_size <- basis_size
      } else {
        s@basis_size  <- 20 * s@dimension + s@design_size
      }
      basis <- degl(s@dimension, s@basis_size, exclude)
      s@basis   <- basis$basis
      s@include <- basis$include
    }

    if(!missing(K)){
      s@K <- K
    } else {
      s@K <- if(s@dimension == 1){
        construct.K.1d(s@basis)
      } else {
        construct.K(s@basis)
      }
    }

    if(!missing(P)){
      s@P <- P
      s@legendre <- FALSE
    } else {
      s@P <- if(s@dimension == 1){
        construct.P.1d(s@basis)
      } else {
        construct.P(s@basis)
      }
    }
  }

  if(!missing(design_model_matrix)){
    s@design_model_matrix <- design_model_matrix
  } else {
    s@design_model_matrix <- construct.dmm(s@basis, design, s@P)
  }

  # fit model
  s@theta <- rep(0, nrow(s@basis))
  theta <- find.theta(response,
                      s@K[s@include, s@include],
                      s@design_model_matrix[ , s@include])

  # check fit
  if (length(theta)<1) {
    warning(paste("Unable to fit model due to numeric instability.\n",
                  "Try reducing the basis size, which is currently ",
                  s@basis_size, sep=""),
            call. = FALSE, immediate = TRUE)
    return(s)
  } else {
    s@theta[s@include] <- theta
    s@fail = FALSE
  }

  # run sensitivity analysis and compute sobol indices
  if (SA) s <- update.sensitivity(s)

  # generate Gaussian process metamodel error if GP = TRUE
  if (GP){
    s@distance <- new.distance(ssm = s, type = s@distance_type)
    s@type <- type
    s <- estimate.GP(s)
  }

  # compute LOO residuals if validation = TRUE and they are not done already
  if (validation){
    if (length(s@residuals) < 1) s <- compute.residuals(s)
    sdybar <- sqrt(sum((mean(response) - response)^2) / length(response))
    s@LOO_RMSE <- sqrt(sum(s@residuals^2) / s@design_size) /
                  sdybar
  }

  return(s)
}






#' Construct the design model matrix
#'
#' Constructs the design model matrix corresponding to the given design and
#' basis. Used by \code{fit.ssm}.
#'
#' The argument \code{basis} defines a monomial basis and the change of basis
#' matrix \code{P} is used to convert this to a polynomial basis.  The function
#' returns the design model matrix corresponding to this polynomial basis and
#' the design given by \code{design}.  Note that if \code{P} is the
#' appropriately sized identity matrix then the model basis a monomial one.
#'
#' @param basis An \eqn{N x d} matrix where each row is an exponent
#'  vector.
#' @param design An \eqn{n x d} matrix where each row is a design point.
#' @param P An \eqn{N x N} change of basis matrix from a monomial basis to
#'  a polynomial basis.  Note that each column corresponds to a polynomial term.
#' @keywords internal
#' @return The \eqn{n x N} design model matrix.
construct.dmm <- function(basis, design, P){
  X<-matrix(0, ncol = nrow(basis), nrow = nrow(design))
  if(ncol(basis) == 1){
    for (i in 1:nrow(basis)){
      X[ , i] <- apply(design, 1, "^", basis[i, ])
    }
  }
  if(ncol(basis) > 1){
    for (i in 1:nrow(basis)){
      X[ , i] <- apply(apply(design, 1, "^", basis[i, ]), 2, prod)
    }
  }
  if(missing(P)) return(X)
  return(X %*% P)
}

#' Generate all desired exponent vectors of a given degree.
#'
#' \code{comb} recursively generates all the desired exponent vectors of a given
#' degree and is called by \code{\link{degl}} to generate the matrix put into
#' the \code{basis} slot of the SSM object.
#'
#' This function is called by \code{\link{degl}} during the process of
#' constructing the objects related to the basis of a smooth supersaturated
#' model.  It constructs the first \code{N} exponent vectors of degree \code{d},
#' excluding those which are non-zero only in the columns specified by vectors
#' listed in \code{exclude}.  It operates recursively.
#' @param d A number. Controls the number of recursions before stopping.
#' @param deg A number. The desired degree of resulting exponent vectors.
#' @param N (optional) A number. Sets the number of exponent vectors that will
#'   be generated.  If not supplied, all candidate vectors will be generated.
#' @param vec A vector.  Stores the current state of the generated vector during
#'   recursion.
#' @param start (optional) Logical. A flag used to identify the initial call environment.
#' @param parent (optional) An environment. Stores the environment name of the initializing
#'   function call.
#' @param exclude (optional) A list of integer vectors which is used to exclude the
#'   generation of undesired exponent vectors.  \emph{e.g.} If 1 is a vector in
#'   the list then any generated vector which is non-zero in the first column
#'   and zero everywhere else will be thrown away.
#' @keywords internal
#' @return A matrix of exponent vectors
comb <- function(d, deg, N = choose(d + deg - 1, deg), vec, start = TRUE,
                 parent, exclude = list()){

  # start by initializing objects on initial call
  if (start){

    # number of vectors N to construct is either N or the number of possible
    # combinations, whichever is smaller
    N <- min(N, choose(d + deg -1, deg))

    # L is the matrix that will contain the generated vectors
    L <- matrix(nrow = choose(d + deg - 1, deg), ncol = d)

    # count is a count of the number of included generated vectors
    count <- 1

    # vec is a d dimensional vector of zeroes
    vec <- rep(0, d)

    # include is a vector of indices indicating which matrix rows are included
    # in the model basis
    include <- rep(NA, N)

    # record parent frame info to pass down recursion chain
    parent <- sys.frame(sys.nframe())

    # flag to mark when enough vectors have been generated and the matrix can be
    # output
    finish <- FALSE
  } else {

    # if this is not the inital call but is inside the recursion, get the
    # current matrix of vectors and count from the initial environment
    L <- get("L", envir = parent)
    count <- get("count", envir = parent)
    include <- get("include", envir = parent)
  }


  # if the recursion has completed, check the generated vector is not to be
  # excluded and assign the generated vector to the correct row of the matrix L
  # in the initial environment and add one to the count
  if (deg == 0){
    L[count, ] <- vec
    assign("L", L, envir = parent)
    assign("count", count + 1, envir=parent)
    if (length(exclude)>0){
      for (i in 1:length(exclude)) {
        if (all(vec[exclude[[i]]] > 0) && all(vec [-exclude[[i]]] == 0)){
          return()
        }
      }
    }
    include[match(TRUE, is.na(include))] <- count
    assign("include", include, envir=parent)
    if(sum(!is.na(include)) == N) assign("finish", TRUE, envir = parent)
    return()
  }

  # recursive section: this adds one to an element of vec and passes it into
  # comb again but with one less degree
  for (i in max(which(vec != 0), 1):length(vec)){
    if (get("finish", envir=parent) && !start) return()
    if (get("finish", envir=parent))
      return(list(basis=L[!is.na(L[, 1]), ],
                  include = include[!is.na(include)]))
    addvec <- rep(0, d)
    addvec[i] <- 1
    comb(d = d, N = N, vec = vec + addvec, deg = deg - 1, start = FALSE, parent = parent, exclude = exclude)
  }

  ind <- is.na(L[,1])
  # identify the rows of NA to remove when less than N rows are generated
  return(list(basis=L[!ind,], include = include[!is.na(include)]))
}

#' Construct matrix of exponent vectors.
#'
#' This function is called by \code{\link{fit.ssm}} when it needs to construct
#' the matrix of exponent vectors stored in the \code{basis} slot.
#'
#' The algorithm works by repeated calls to \code{\link{comb}} to generate all
#' possible exponent vectors of a given degree until \code{N} vectors have been
#' generated.  Any generated vector is checked to make sure that it's non-zero
#' entries do not match a vector provided in \code{exclude} before being added
#' to the output matrix.
#'
#' @param d A number. This determines the number of columns in the
#'   output matrix
#' @param N A number. This determines the number of rows in the output matrix.
#' @param exclude (optional) A list of integer vectors.  If this argument is
#'   provided, the generated matrix will not contain any rows which are only
#'   non-zero in the columns specified by any of the vectors in the list.
#' @keywords internal
degl <- function(d,N, exclude = list()){
  ## degl(d, N) generates a matrix of N hierarchical exponent vectors in d variables
  ## the optional argument exclude allows you to include a list of terms to exclude,
  ##  identified by integers.  For example exclude = list(1) will not include any
  ##  terms only in x_1.  exclude = list(1, c(2,3)) will exclude terms in x_1 and
  ##  in x_2x_3.
  include <- 1
  L <- matrix(0, nrow = 1, ncol = d)
  count <- 1
  deg <- 1
  while(count < N){
    n <- min(choose(d + deg - 1, deg), N - count)
    new <- comb(d, deg, N = n, exclude = exclude)
    if(length(new$include)==0) stop("All variables and interactions have been excluded")
    include <- c(include, new$include + nrow(L))
    n <- nrow(new$basis)
    L <- rbind(L, new$basis)
    count <- count + length(new$include)
    deg <- deg + 1
  }
  return(list(basis = L, include = include))
}


#' Construct the K matrix for a given multivariate basis.
#'
#' This function constructs the K matrix for a given multivariate basis assuming
#' the basis is a Legendre polynomial basis and the smoothing criterion is the
#' Frobenius norm of the Hessian integrated over \eqn{[-1, 1]^d}.
#' @param basis A matrix.  Rows of the matrix are taken as the exponent vectors
#'   of the leading terms of a Legendre polynomial basis.
#'
#' @return A matrix where each entry is \eqn{<f, g>} with \deqn{<f, g> = \int_X
#'  \sum_{i,j}\frac{d^2f}{dx_idx_j}\frac{d^2g}{dx_idx_j} dx,} with \eqn{f, g}
#'  being the Legendre polynomials described by the appropriate exponent vectors.
#' @keywords internal
construct.K<-function(basis) {
  ## Input - basis
  ## Output - the K matrix for the respective basis
  ## of Legendre multivariate polynomials
  n<-nrow(basis)
  K<-matrix(0,nrow=n,ncol=n)
  for(i in 1:n){
    K[i,i:n]<-sapply(i:n,get.K.element,i,basis)
    K[i:n,i]<-K[i,i:n]
  }
  return(K)
}

#' Construct the K matrix for a given univariate basis.
#'
#' This function constructs the K matrix for a given multivariate basis assuming
#' the basis is a Legendre polynomial basis and the smoothing criterion is the
#' Frobenius norm of the Hessian integrated over \eqn{[-1, 1]}.
#' @param basis A matrix.  Rows of the matrix are taken as the degree of the
#'   Legendre polynomial.
#' @keywords internal
#' @return A matrix where each entry is \eqn{<f, g>} with \deqn{<f, g> = \int_X
#'  \frac{d^2f}{dx^2}\frac{d^2g}{dx^2} dx,} with \eqn{f, g} being the
#'  Legendre polynomials described by the appropriate exponent vectors.
construct.K.1d <- function (basis) {
  # Uses an explicit function for computing each entry
  basis <- as.numeric(basis)
  L2 <- function (x, y) {
    m <- min (x, y)
    n <- max (x, y)
    if (m %% 2 != n %% 2) return (0)
    m * (m - 1) * (m + 1) * (m + 2) * (3 * n^2 + 3 * n + 6 - m^2 - m) / 24
  }
  L2 <- Vectorize (L2)
  K <- outer(basis,basis,L2)
  return (K)
}

#' Compute entry of K matrix.
#'
#' Called by \code{\link{construct.K}} to compute entries of the K matrix.
#'
#' @param n A number. Identifies the first basis element in the inner product.
#' @param m A number. Identifies the second basis element in the inner product.
#' @param basis A matrix. Rows are taken as exponent vectors.
#' @keywords internal
#' @return The inner product of the \eqn{n, m}-th basis elements as defined for
#'   \code{\link{construct.K}}.
get.K.element<-function(n, m, basis){
  ## This function is given the indexes of two terms in basis and basis itself
  ## as input.
  ## It returns 0 if more than 3 exponents differ
  ## It returns g(p,q) if p and q are the only exponents that differ
  ## It returns sum[g(p,k)] if p is the only term that differs

  ## For large dimensional datasets, this may be slightly faster if
  ## we check each exponent in turn and return 0 if we find more than
  ## two differ
  d<-ncol(basis)
  if(n == m){
    pr <- 2 / (basis[n, ] * 2 + 1)
    tot <- 0
    for (j in 1:d){
      N <- basis[n, j]
      for(i in j:d){
        if (i == j) tot <- tot + (N * (N-1) * (N+1) * (N+2) *
                                    (2 * N^2 + 2 * N + 6) * prod(pr[-i])) / 24
        if (i != j) {
          M <- basis[n, i]
          tot <- tot + 2 * M * N * (M + 1) * (N + 1) * prod(pr[-c(i, j)])
        }
      }
    }
    return(tot)
  }

  ## create sequence differ for different exponents
  w <- basis[n, ] == basis[m, ]
  differ <- which(!w)
  ## if length differ>2 then return 0
  if (length(differ) > 2) return(0)
  ## create sequence same for matching exponents
  same <- which(w)
  ## create product pr for same terms
  pr <- 2 / (basis[n, same] * 2 + 1)
  ## if length differ=2 retrn g(differ)*prod(pr)
  if (length(differ) == 2) {
    if ((basis[n, differ[1]] %% 2 == 0) != (basis[m, differ[1]] %% 2 == 0))
      return(0)
    if ((basis[n, differ[2]] %% 2 == 0) != (basis[m, differ[2]] %% 2 == 0))
      return(0)
    N <- min(basis[n, differ[1]], basis[m, differ[1]])
    M <- min(basis[n, differ[2]], basis[m, differ[2]])
    return(M * N * (M + 1) * (N + 1) * prod(pr))
  }
  ## return sum[g(p,k)]*pr
  if ((basis[n, differ] %% 2 == 0) != (basis[m, differ] %% 2 == 0)) return(0)
  N <- min(basis[n, differ], basis[m, differ])
  M <- max(basis[n, differ], basis[m, differ])
  tot<-(N * (N - 1) * (N + 1) * (N + 2) * (3 * M^2 + 3 * M + 6 - N^2 - N) *
          prod(pr)) / 24
  for (i in 1:length(same)) {
    M <- basis[n, same[i]]
    tot <- tot + M * N * (M + 1) * (N + 1) * prod(pr[-i])
  }
  return(tot)
}

## Legendre_P_1d creates the P matrix for 1 dimensional Legendre polynomials
##	using explicit representation

#' Construct the change of basis matrix from univariate monomials to Legendre
#' polynomials.
#'
#' Let \eqn{f(x)} be the vector of monomials implied by the matrix \code{basis}.
#' Let \eqn{P} be the matrix generated by \code{Legendre_P_1d}.  Then
#' \eqn{g(x)=P^Tf(x)} is the vector of Legendre polynomials with leading terms
#' corresponding to \code{basis}. \emph{e.g.} the matrix \eqn{(0 1 2)^T} implies
#' \eqn{f(x)^T = (1 x x^2)}. Then \eqn{g(x)^T = (L_0, L_1, L_2)}.
#'
#' @param basis A matrix. Rows are taken as the degree of the associated
#'   monomial.
#' @keywords internal
#' @return A matrix which functions as a change of basis from monomials to
#'   Legendre.
construct.P.1d <- function(basis){
  N <- max(basis) + 1
  P <- diag(0, N)
  for (i in 1:N){
    for (j in i:N){
      P[i, j] <- 2^(j - 1) * choose(j - 1, i - 1) *
        choose((j + i - 3) / 2, j - 1)
    }
  }
  return(P)
}

#' Construct the change of basis matrix from multivariate monomials to Legendre
#' polynomials.
#'
#' Let \eqn{f(x)} be the vector of monomials implied by the matrix \code{basis}.
#' Let \eqn{P} be the matrix generated by \code{Legendre_P}.  Then
#' \eqn{g(x)=P^Tf(x)} is the vector of Legendre polynomials with leading terms
#' corresponding to \code{basis}.
#'
#' @param basis A matrix. Rows are taken as the degree of the associated
#'   monomial.
#' @keywords internal
#' @return A matrix which functions as a change of basis from monomials to
#'   Legendre.
construct.P <- function(basis){
  n <- nrow(basis)
  d <- ncol(basis)
  Legendre_P <- construct.P.1d(basis)
  polynomial_system <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    m <- Legendre_P[basis[i, ] + 1, ] #this holds the multipliers for each exponent
    for (j in 1:n){
      polynomial_system[i, j] <- prod(m[basis[j, ] * d + 1:d])
    }
  }
  return(polynomial_system)
}



## find_theta: given the response, K and design model matrix, finds theta

#' Compute the SSM vector of parameters.
#'
#' This function is called by \code{\link{fit.ssm}} to compute the coefficient
#' vector that interpolates the data and minimises the smoothness of the
#' resulting model.
#' @param response An length \eqn{n} vector. The observed responses.
#' @param K A semi-positive definite \eqn{N x N} matrix that defines the smoothing
#'   criterion.
#' @param design_model The \eqn{n x N} design model matrix.
#' @param tol (optional) The model fitting requires the inversion of a large
#'   matrix and if the model basis is too large there can be numerical issues.
#'   This argument is passed on to \code{\link{solve}} so models can be fit
#'   despite these issues.
#' @keywords internal
#' @return A vector of parameters of length \eqn{N} if the model is fit
#'  successfully. \code{NA} is returned should \code{\link{solve}} not invert
#'  the required matrix.
find.theta <- function(response, K, design_model, tol = .Machine$double.eps){
  n <- nrow(design_model)
  N <- ncol(design_model)
  C <- rbind(cbind(diag(0, n), design_model), cbind(-t(design_model), K))
  flag <- tryCatch(solve(C, c(response, rep(0, N)), tol = tol),
                   error = function(e) NA)
  return(flag[-(1:n)])
}

#' Compute the Leave-One-Out error at all design points.
#'
#' This function repeatedly fits the SSM to all but one held out point in turn
#' and computes the prediction error at the held out point.  These errors are
#' collated into a vector indicating the Leave-One-Out error at each design
#' point.
#'
#' @param ssm An SSM object that has valid entries for the slots
#'   \code{response}, \code{K}, and \code{design_model_matrix}.
#' @keywords internal
#' @return A vector of Leave-One_Out errors.
compute.residuals <- function (ssm){
  # this computes the LOO error at each design point found by fitting the SSM
  # to the other points and finding the difference between the observation and
  # model at the design point.
  n <- length(ssm@response)
  e <- c()
  for (i in 1:n){
    theta <- find.theta(ssm@response[-i], ssm@K, ssm@design_model_matrix[-i,])
    e[i] <- ssm@design_model_matrix[i,] %*% theta - ssm@response[i]
  }
  ssm@residuals <- e
  return(ssm)
}


#' Transform a design to [-1, 1]^d
#'
#' This function transforms a design (supplied as a matrix) into the space
#' [-1, 1]^d.  This has numerical and computational advantages when using smooth
#' supersaturated models and is assumed by the default \code{\link{fit.ssm}}
#' behaviour.
#'
#' @param design A matrix where each row is a design point.
#'
#' @return A matrix where each column contains values in \eqn{[-1, 1]^d}.
#' @export
#' @examples
#' X <- transform11(quakes[, 1:4])
#' apply(X, 2, range)
transform11 <- function(design){
  if(is.vector(design)){
    r <- range(design)
    return (2 * (design - r[1]) / diff(r) - 1)
  }
  r <- apply(design, 2, range)
  m <- r[1, ]
  d <- apply(r, 2, diff)
  t(2 * (t(design) - m) / d - 1)
}
