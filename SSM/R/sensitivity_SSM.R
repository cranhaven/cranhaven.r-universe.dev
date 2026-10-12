
################################################
## SENSITIVITY ANALYSIS functions


#' Update an SSM object with the term variances and Sobol indices
#'
#' This function computes the term variances, Sobol indices, Total indices and
#' Total interaction indices of a given SSM class object.
#'
#' This function has two modes.  If the \code{legendre} slot in the SSM object
#' is TRUE (\emph{i.e.} the default \code{P} matrix has been used to fit the
#' SSM) then all the sensitivity indices are computed assuming inputs are
#' uniformly distributed over \eqn{[-1, 1]^d}.  If \code{legendre} is FALSE
#' (\emph{i.e.} a user-defined \code{P} has been supplied) then the polynomials
#' defined by \code{P} are orthonormal with respect to some probability measure
#' and the sensitivity indices are computed assuming that measure defines the
#' distribution of the inputs.
#'
#' The returned SSM object has term variances held in the \code{variances} slot,
#' ordered to match the exponent vectors in the \code{basis} slot.  The Sobol
#' indices are placed in \code{main_sobol} and the Total indices in
#' \code{total_sobol}.  The Total interaction indices are placed in the
#' \code{total_int} slot with identifying labels stored in
#' \code{total_int_factors}.  If there are less than eleven variables then
#' interaction indices are computed for all order interactions and stored in
#' \code{int_sobol} with identifying labels in \code{int_factors}.
#' @param ssm An SSM object.
#' @keywords internal
#' @return An SSM object.
update.sensitivity <- function (ssm){

  if(ssm@legendre){
    cat("\nComputing Sobol indices assuming inputs are uniformly distributed
        over [-1,1]^d using Legendre polynomials.\n")
    if (ssm@dimension == 1) {
      norms <- as.numeric(2 / (2 * ssm@basis + 1))
    } else {
      norms <- apply(2 / (2 * ssm@basis + 1), 1, prod)
    }
    ssm@variances <- ssm@theta^2 * norms
  } else {
    cat("\nComputing Sobol indices using user-defined polynomial basis.  This
        method assumes P defines an orthonormal system with respect to the
        probability measure defining the input distribution.\n")
    ssm@variances <- ssm@theta^2
  }
  ssm@total_variance <- sum(ssm@variances[-1])

  if (ssm@dimension == 1){
    ssm@main_sobol <- 1
    ssm@total_sobol <- 1
    ssm@total_int <- 1
  } else {
    cat("\nComputing main effects.\n")
    ssm <- compute.main.effects(ssm)
    cat("\nComputing total effects.\n")
    ssm <- compute.total.effects(ssm)
    cat("\nComputing total interaction indices.\n")
    ssm <- compute.interactions(ssm)
  }

  return (ssm)
}



#' Identify main effect terms
#'
#' This is used by the sensitivity computation functions to identify which basis
#' terms are associated with each main effect.
#' @param i A number indicating the main effect of interest.
#' @param basis A matrix.  A matrix where each row is an exponent vector of a
#'   monomial basis.
#' @return A logical vector.  Each term corresponds with the associated row of
#' \code{basis}.
#' @keywords internal
identify.main.effect.terms <- function(i, basis)
  apply(basis, 1, sum) == basis[ , i]

#' Compute main effects
#'
#' This function computes the Sobol indices of the model variables, also known
#' as the main effects.  It identifies the relevant rows of the SSM \code{basis}
#' slot and sums the term variances. Called by \code{\link{update.sensitivity}}.
#'
#' @param ssm An SSM object.
#'
#' @return An SSM object.  This is the \code{ssm} input with the
#' relevant slots updated.
#' @keywords internal
compute.main.effects <- function(ssm){
  ssm@main_ind      <- sapply(1:ssm@dimension, identify.main.effect.terms,
                              basis = ssm@basis)
  ssm@main_ind[1, ] <- FALSE
  ssm@main_sobol      <- apply(ssm@main_ind * ssm@variances, 2, sum) /
                           ssm@total_variance
  return(ssm)
}

#' Identify total effect terms
#'
#' This is used by the sensitivity computation functions to identify which basis
#' terms are associated with each total effect.
#' @param i A number indicating the total effect of interest.
#' @param basis A matrix.  A matrix where each row is an exponent vector of a
#'   monomial basis.
#' @keywords internal
#' @return A logical vector.  Each term corresponds with the associated row of
#' \code{basis}.
identify.total.effect.terms <- function(i, basis)
  basis[ , i] > 0

#' Compute Total effects
#'
#' This function computes the Total indices of the model variables.  It
#' identifies the relevant rows of the SSM \code{basis} slot and sums the term
#' variances. Called by \code{\link{update.sensitivity}}.
#'
#' @param ssm An SSM object.
#' @keywords internal
#' @return An SSM object.  This is the \code{ssm} input with the
#' relevant slots updated.
compute.total.effects <- function(ssm){
  ssm@total_ind <- sapply(1:ssm@dimension, identify.total.effect.terms,
                          basis = ssm@basis)
  ssm@total_sobol <- apply(ssm@total_ind * ssm@variances, 2, sum) /
                       ssm@total_variance
  return(ssm)
}


#' Compute Total interaction indices and Sobol indices for higher order
#' interactions.
#'
#' This function computes the Total interaction indices of all second order
#' interactions present in the model.  If \eqn{d < 11} then it also computes the
#' Sobol indices of all interactions (second order and higher) in the model. It
#' identifies the relevant rows of the SSM \code{basis} slot for each
#' interaction and sums the term variances. Called by
#' \code{\link{update.sensitivity}}.
#'
#' @param ssm An SSM object.
#' @keywords internal
#' @return An SSM object.  This is the \code{ssm} input with the
#' relevant slots updated.
compute.interactions <- function(ssm){

  if(ssm@dimension < 11){
    cat("\nComputing Sobol indices for all order interactions.\n")
    # initialize list to hold factor indexes
    ssm@int_factors <- vector("list",
                              length = 2^ssm@dimension - ssm@dimension - 1)

    counter = 1
    for (i in 2:ssm@dimension){     # cycle through all order interactions
      # compute indices of all ith order interactions as matrix
      int_matrix <- combn(ssm@dimension, i)
      # fix format when only one interaction, i.e. the order when all variables
      # are interacting
      if (i == ssm@dimension) int_matrix <- matrix(int_matrix, ncol = 1)
      # This loop updates the relevant ssm list with the indices
      for (j in 1:ncol(int_matrix)){
        ssm@int_factors[[counter]] <- int_matrix[ ,j]
        counter = counter + 1
      }
    }
    # compute sobol indices
    ssm@int_sobol <- unlist(lapply(ssm@int_factors,
                                   compute.specific.interaction, ssm = ssm))
  }

  ## compute total interaction indices
  cat("Computing total interaction indices.\n")
  ssm@total_int_factors <- t(combn(ssm@dimension,2))
  ssm@total_int <- apply(ssm@total_int_factors, 1,
                         compute.specific.total.interaction, ssm = ssm)
  return(ssm)
}

#' Compute Total interaction variance
#'
#' This computes the Total interaction index for a given interaction. The
#' relevant term variances are identified and summed and the resulting
#' variance normalized and returned.
#' @param factors A vector of numbers identifying the interaction of interest.
#'   \emph{e.g.} The input \eqn{(1, 3, 4)} indicates that the interaction
#'   between the first, third and fourth factors is the one of interest.
#' @param ssm An SSM object.
#' @keywords internal
#' @return A number.  The Total interaction index of the requested interaction.
compute.specific.total.interaction <- function(factors, ssm){
  ind <- apply(ssm@basis[ , factors] > 0, 1, prod)
  var <- sum(ind * ssm@variances) / ssm@total_variance
  return(var)
}

#' Compute the Sobol index for a given interaction.
#'
#' This computes the Sobol index for a given interaction. The
#' relevant term variances are identified and summed and the resulting
#' variance normalized and returned.
#' @param factors A vector of numbers of length at least two, identifying the
#'   interaction of interest. \emph{e.g.} The input \eqn{(1, 3, 4)} indicates
#'   that the interaction between the first, third and fourth factors is the one
#'   of interest.
#' @param ssm An SSM object.
#' @keywords internal
#' @return A number. The Sobol index of the requested interaction.
compute.specific.interaction <- function (factors, ssm){
  ind <- apply(ssm@basis[ , factors] > 0 ,1, prod)
  if(length(factors) != ssm@dimension){
    not_factors <- setdiff(1:ssm@dimension, factors)
    not_ind <- apply(as.matrix(ssm@basis[ , not_factors], ncol=length(not_factors)) == 0, 1, prod)
    ind <- ind * not_ind
  }
  var <- sum(ind*ssm@variances) / ssm@total_variance
  return(var)
}

#' Plot the sensitivity indices of a smooth supersaturated model.
#'
#' \code{sensitivity.plot} visualises the sensitivity indices of a given smooth
#' supersaturated model using \code{barplot}.  If the \code{SA} flag was not set
#' to TRUE when \code{\link{fit.ssm}} was run to fit the model, then
#' \code{\link{update.sensitivity}} should be used to compute the model
#' variances.  If not, this function will return an error message.
#'
#' There are four classes of plot available:
#' \itemize{
#'   \item{\code{"sobol"}}{ Produces a barplot of all Sobol indices. If there
#'   are more than 10 factors then Sobol indices will not have been computed for
#'   interactions and only the Sobol indices for main effects will be plotted.
#'   Main effects are in red, interactions are in grey.}
#'   \item{\code{"main_sobol"}}{ Produces a barplot of Sobol indices for main
#'   effects only.}
#'   \item{\code{"main_total"}}{ Produces a barplot of Total indices for main
#'   effects only. This is the default behaviour.}
#'   \item{\code{"total"}}{ Produces a barplot of Total indices for main effects
#'   and all second order interactions. Main effects are in red, interactions
#'   are in grey.}
#' }
#'
#' Note that variables and interactions are not labelled in the plots since
#' there can be too many bars to label clearly.
#' @param ssm An SSM object.  Must have relevant sensitivity indices
#'   in the appropriate slots, either from setting \code{SA = TRUE} in
#'   \code{\link{fit.ssm}} or by using \code{\link{update.sensitivity}}.
#' @param type (optional) Character. Determines the type of barplot. One of
#'   \code{"sobol"}, \code{"main_sobol"}, \code{"main_total"}, or
#'   \code{"total"}. Default behaviour is \code{"main_total"}.
#' @param ... arguments passed to the \code{barplot} function call.
#' @export
#' @examples
#' # A 20 point design in four variables
#' X <- matrix(runif(80, -1, 1), ncol = 4)
#' Y <- runif(20)
#' s <- fit.ssm(X, Y, SA = TRUE)
#' sensitivity.plot(s)
#'
#' # In the next plots, the grey bars indicate interactions.
#' sensitivity.plot(s, "sobol")
#' sensitivity.plot(s, "total")
#' # Identifying particular indices is best done using the information held in
#' # the SSM object.  The following orders s@total_int_factors so the
#' # interaction indicated by the top row is the most important.
#' ind <- order(s@total_int, decreasing = TRUE)
#' s@total_int_factors[ind, ]
sensitivity.plot <- function(ssm, type = "main_total", ...){
  # plots a barchart of the sensitivity indices of an SSM object
  # the type argument specifies the kind of plot:
  #    "sobol":      plots all sobol indices for all main effects and all order interactions.  This option should not be used for datasets with
  #                    a large number of variables due to the number of interactions (2^d)
  #    "main_sobol": plots sobol indices for main effects only
  #    "main_total": plots total indices for main effects only
  #    "total":      plots total indices for main effects and all second order interactions
  #    ...:          arguments to pass to barplot (overrides default behaviour)
  if(length(ssm@variances) < 1) stop("Sensitivity analysis has not but run on this model. Please use model <- compute.sensitivity(model).\n")

  if(!type %in% c("sobol", "main_sobol", "main_total", "total"))
    stop("type must be one of \"sobol\", \"main_sobol\", \"main_total\", or \"total\".\n")
  op <- par(no.readonly = TRUE)

  if(type == "sobol" && ssm@dimension > 10){
    print("Too many interactions for full plot. Plotting main effects only.")
    type <- "main_sobol"
  }

  arguments = list(...)
  if (is.null(arguments$border))
    arguments$border <- NA

  if(type == "sobol") {
    ## plot barplot of all sobol indices
    sobol <- c(ssm@main_sobol, ssm@int_sobol)
    indices <- c(1:ssm@dimension, ssm@int_factors)
    if (is.null(arguments$ylim))
      arguments$ylim <- c(0, min(1, max(sobol+0.1)))
    if (is.null(arguments$col))
      arguments$col <-c(rep(2, ssm@dimension), rep("grey", length(ssm@int_factors)))
    if (is.null(arguments$main))
      arguments$main <- "Sobol indices for smooth supersaturated model"
    arguments$height <- sobol
  }

  if(type == "total"){
    ## plot barplot of main sobol indices and total interaction indices
    total <- c(ssm@total_sobol, ssm@total_int)
    indices <- vector("list", length = nrow(ssm@total_int_factors) + ssm@dimension)
    for (i in 1:ssm@dimension) indices[[i]] <- i
    for (i in 1:nrow(ssm@total_int_factors) + ssm@dimension) indices[[i]] <- ssm@total_int_factors[i - ssm@dimension, ]
    if (is.null(arguments$ylim))
      arguments$ylim <- c(0, min(1, max(total+0.2)))
    if (is.null(arguments$col))
      arguments$col <- c(rep(2, ssm@dimension), rep("grey", length(ssm@total_int_factors)))
    if (is.null(arguments$main))
      arguments$main <- "Total indices for smooth supersaturated model"
    arguments$height <- total
  }

  if(type == "main_sobol") {
    ## plot barplot of all main effect sobol indices
    if (is.null(arguments$ylim))
      arguments$ylim <- c(0, min(1, max(ssm@main_sobol+0.1)))
    if (is.null(arguments$col))
      arguments$col <- 2
    if (is.null(arguments$main))
      arguments$main <- "Sobol indices for smooth supersaturated model"
    arguments$height <- ssm@main_sobol
  }

  if(type == "main_total") {
    ## plot barplot of all main effect total indices
    if (is.null(arguments$ylim))
      arguments$ylim <- c(0, min(1, max(ssm@total_sobol+0.2)))
    if (is.null(arguments$col))
      arguments$col <- 2
    if (is.null(arguments$main))
      arguments$main <- "Total indices for smooth supersaturated model"
    arguments$height <- ssm@total_sobol
  }
  do.call(barplot, arguments)
  axis(1, at = c(-100000,100000))
  par(op)
}

