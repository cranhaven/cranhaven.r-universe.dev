# # TODO: perhaps implement this
# ## The Riemannian manifold of trees, _with_ the edges connecting leaves retained. 

# #' @export
# metric.Tree <- function(mfd, p, U, V) {
  # NextMethod('metric')
# }


# #' @export
# norm.Tree <- function(mfd, p, U) {
  # NextMethod('norm')
# }


# #' @export
# distance.Tree <- function(mfd, X, Y, ...) {

  # if (inherits(X, 'phylo')) {
    # X <- list(X)
  # }

  # if (inherits(Y, 'phylo')) {
    # Y <- list(Y)
  # }

  # if (length(X) > length(Y)) {
    # tmp <- X
    # X <- Y
    # Y <- tmp
  # }

  # if (length(X) == 1 && length(Y) > 1) {
    # X <- rep(X, length(Y))
  # }

  # vapply(seq_len(X), function(i) {
           # # Tedious to extract the individual distance functions from distory.
  # }, 0.1)
# }


# # #' Riemannian exponential map at a point p

# #' @export
# rieExp.Tree <- function(mfd, p, V, ...) {

  # p <- as.matrix(p)
  # V <- as.matrix(V)

  # if (length(p) == 0) {
    # return(p)
  # }

  # if (length(V) == 0) {
    # return(V)
  # }

  # if (ncol(p) == 1 && ncol(V) > 1) {
    # p <- matrix(p, nrow=nrow(V), ncol=ncol(V))
  # } 
  # if (ncol(V) == 1 && ncol(p) > 1) {
    # V <- matrix(V, nrow=nrow(p), ncol=ncol(p))
  # }

  # res <- (V + p) %% 1
  # res
# }


# # #' Riemannian log map at a point
# # #' @param mfd A class instance that represents the Riemannian manifold
# # #' @param p A matrix. Each column represents a base point of the log map. If only one base point is supplied, then it is replicated to match the number of points in \emph{X}
# # #' @param X A matrix. Each column represents a point on the manifold
# # #' @returns A matrix with the \emph{i}th column being the log map of the \emph{i}th point


# #' @export
# rieLog.Tree <- function(mfd, p, X, ...) {
  # p <- as.matrix(p)
  # X <- as.matrix(X)

  # if (length(p) == 0) {
    # return(p)
  # } else if (length(X) == 0) {
    # return(X)
  # }

  # if (ncol(p) == 1 && ncol(X) > 1) {
    # p <- matrix(p, nrow=nrow(X), ncol=ncol(X))
  # } 
  # if (ncol(X) == 1 && ncol(p) > 1) {
    # X <- matrix(X, nrow=nrow(p), ncol=ncol(p))
  # }

  # Z <- X - p
  # -0.5 + (Z + 0.5) %% 1

# }


# #' @export
# # Make the edge length nonnegative
# project.Tree <- function(mfd, p) {
  
# }


# #' @export
# calcGeomPar.Tree <- function(mfd, dimAmbient, dimIntrinsic, dimTangent) {

  # if (!missing(dimAmbient)) {
    # dimAmbient
  # } else if (!missing(dimIntrinsic)) {
    # dimIntrinsic
  # } else if (!missing(dimTangent)) {
    # dimTangent
  # }
# }


# #' @export
# calcIntDim.Tree <- function(mfd, geomPar, dimAmbient, dimTangent) {
  
  # if (!missing(geomPar)) {
    # as.integer(geomPar)
  # } else if (!missing(dimAmbient)) {
    # as.integer(dimAmbient)
  # } else if (!missing(dimTangent)) {
    # as.integer(dimTangent)
  # }
# }


# #' @export
# calcTanDim.Tree <- function(mfd, geomPar, dimAmbient, dimIntrinsic) {

  # if (!missing(geomPar)) {
    # as.integer(geomPar)
  # } else if (!missing(dimAmbient)) {
    # as.integer(dimAmbient)
  # } else if (!missing(dimIntrinsic)) {
    # as.integer(dimIntrinsic)
  # }
# }


# #' @export
# calcAmbDim.Tree <- function(mfd, geomPar, dimIntrinsic, dimTangent) {
  # if (!missing(geomPar)) {
    # as.integer(geomPar)
  # } else if (!missing(dimIntrinsic)) {
    # as.integer(dimIntrinsic)
  # } else if (!missing(dimTangent)) {
    # as.integer(dimTangent)
  # }
# }


# #' @export
# projectTangent.Tree <- function(mfd, p, X, projMatOnly=FALSE, ...) {

  # if (projMatOnly) {
    # return(diag(length(p)))
  # } else {
    # -0.5 + (as.matrix(X) + 0.5) %% 1
  # }
# }


# #' @export
# origin.Tree <- function(mfd, dimIntrinsic) {
  # as.matrix(rep(1/2, dimIntrinsic))
# }


# #' @export
# basisTan.Tree <- function(mfd, p) {
  # diag(nrow=length(p))
# }


