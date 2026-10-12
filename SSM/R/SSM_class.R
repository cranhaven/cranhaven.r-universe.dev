#' SSM: A package for fitting smooth supersaturated models (SSM).
#'
#' The SSM package provides an S4 class to represent smooth supersaturated
#' models, along with functions for fitting SSM to data, computing Sobol indices
#' for the SSM and estimating metamodel error with a Gaussian process.
#'
#' @aliases NULL
#'
#' @section SSM functions:
#' There are three important functions in the package.
#'
#' \code{fit.ssm} returns an SSM object that fits an SSM to the data and, by
#' default, computes the Sobol indices, and Total interaction indices of the
#' model.  Optionally, the metamodel error can be estimated using a Gaussian
#' process.  The fitted SSM smooths over \eqn{[-1, 1]^d} and uses a hierarchical
#' basis of 20*d+n Legendre polynomials by default but the function is highly
#' customisable.
#'
#' \code{predict.SSM} returns the model prediction at a point, including a
#' credible interval if a metamodel error GP has been fit.
#'
#' \code{plot.SSM} plots the main effects of the SSM.
#'
#' @docType package
#' @name SSM
NULL

#' @import methods utils graphics stats
NULL

#' An S4 class to represent a smooth supersaturated model
#'
#' @slot dimension A number indicating the number of variables in the design.
#' @slot design A matrix with rows indicating the design point and columns
#' indicating the variable.
#' @slot design_size A number indicating the number of design points.
#' @slot response A \code{design_size} length vector of responses.
#' @slot theta A vector containing the fitted model coefficients.
#' @slot basis A matrix with each row being the exponent vector of a polynomial
#' term.
#' @slot basis_size A number indicating the number of basis terms used in the
#' model.  This may be different from \code{nrow(basis)} if terms are excluded.
#' @slot include A vector containing the row numbers of the basis polynomials
#' used in the model.  This is used when interactions or variables are being
#' excluded from the model.
#' @slot K A semi-positive definite matrix that defines the smoothing criteria.
#' @slot P A matrix that defines the polynomial basis in terms of a monomial
#' basis.
#' @slot design_model_matrix A matrix.
#' @slot variances A vector of length \code{basis_size} containing the term
#' variances.
#' @slot total_variance A length one vector containing the total variance.
#' @slot main_sobol A \code{dimension} length vector containing the Sobol
#' index for each variable.
#' @slot main_ind A logical matrix indicating whether each term is included
#' in the main effect corresponding to the column.
#' @slot total_sobol A \code{dimension} length vector containing the Total
#' sensitivity index for each variable.
#' @slot total_ind A logical matrix indicating whether each term is included in
#' the Total sensitivity index corresponding to the column.
#' @slot int_sobol A vector containing the Sobol index for interactions.
#' @slot int_factors A list of length the same as \code{int_sobol} indicating
#' which interaction corresponds with each entry in \code{int_sobol}.
#' @slot total_int A vector containing the Total interaction indices of all
#' second order interactions.
#' @slot total_int_factors A matrix where each row indicates the variables
#' associated with the corresponding interaction in \code{total_int}.
#' @slot distance A matrix containing the distances used for computing the
#' covariance matrix of the GP metamodel error estimate.
#' @slot distance_type A character defining the distance type used for computing
#' \code{distance}.  Can be one of "distance", "line", "product", "area",
#' "proddiff", or "smoothdiff".
#' @slot type A character, either "exp", "matern32", that selects the
#' correlation function used for the GP metamodel error estimate.
#' @slot covariance A positive definite matrix.  The covariance matrix of the GP
#' metamodel error estimate prior to scaling by \code{sigma}.
#' @slot residuals A \code{design_size} length vector containing the
#' Leave-One-Out errors of the model at each design point.
#' @slot sigma A number indicating the scaling factor for \code{covariance}.
#' @slot r A number indicating the length factor for the correlation function.
#' @slot local_smoothness A \code{design_size} length vector containing the
#' model smoothness at each design point.
#' @slot LOO_RMSE A number. The Leave-One-Out root mean square error.
#' @slot legendre logical. Indicates whether the default Legendre polynomial
#' basis is being used.
#' @slot fail logical. Indicates whether the model fit was successful.
#' @export
SSM <- setClass(
  # Set the name for the class
  "SSM",

  # Define the slots
  slots = c(

    # model fitting objects
    dimension    = "numeric", # number of variables
    design       = "matrix",  # rows are design points
    design_size  = "numeric", # number of design points (n)
    response     = "vector",  # vector of observations
    theta        = "vector",  # coefficients of fitted model
    basis        = "matrix",  # rows are exponent vectors (for Legendre)
    basis_size   = "numeric", # number of basis polynomials (N)
    include      = "numeric", # the indices of the basis vectors used in the
                              # fitted model (allows for removal of effects and
                              # interactions)

    K                    = "matrix", # K matrix required for model fit
    P                    = "matrix", # matrix describing basis polynomials
    design_model_matrix  = "matrix", # design model matrix

    # sensitivity analysis objects
    variances         = "numeric", # FANOVA variances for polynomial terms
    total_variance    = "numeric", # var(Y)
    main_sobol        = "numeric", # Sobol indices for main effects
    main_ind          = "matrix",  # describes which basis polynomials are
                                   # associated with each Sobol index
    total_sobol       = "numeric", # Total indices for main effects
    total_ind         = "matrix",  # describes which basis polynomials are
                                   # associated with each total index
    int_sobol         = "numeric", # Sobol indices for all order interactions
                                   # (not computed when number of factors is
                                   # large)
    int_factors       = "list",    # describes which factors relate to each
                                   # interaction Sobol index
    total_int         = "numeric", # Total interaction indices
    total_int_factors = "matrix",  # describes which factors relate to each
                                   # total interaction index

    # gaussian process metamodel error objects
    distance         = "matrix",    # distance matrix of points used to compute
                                    # covariance matrix
    distance_type    = "character", # denotes the method chosen to determine
                                    # 'distance' between points
    type             = "character", # denotes covariance type.  Either "exp" or
                                    # matern32".
    covariance       = "matrix",    # covariance matrix before scaling by sigma
    residuals        = "numeric",   # LOO error at each design point
    sigma            = "numeric",   # parameter of metamodel error GP
    r                = "numeric",   # parameter of metamodel error GP
    local_smoothness = "numeric",   # vector of smoothness at each design point

    # model validation
    LOO_RMSE = "numeric",     # Leave-One-Out RMSE

    # flags
    legendre     = "logical", # this flag indicates whether the default
                             # Legendre polynomials are being used. The
                             # included sensitivity analysis methods only work
                             # with Legendre polynomials.
    fail         = "logical" # this flag can be used to identify failed fits in
                             # error checking
  ),

  # default values
  prototype = list(fail=TRUE, legendre=TRUE, type = "exp")
)
