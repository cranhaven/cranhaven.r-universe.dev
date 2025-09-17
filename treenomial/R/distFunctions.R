#' Calculates the distance between coefficient matrices
#'
#' Calculates the distance between two coefficient matrices or a coefficient matrix and a list of coefficient matrices.
#'
#' @param x single coefficient matrix to find distances to
#' @param Y a list of coefficient matrices
#' @inheritParams polyToDistMat
#' @return vector of distances
#' @note \itemize{
#'   \item the substituted y coefficient vector only supports the \dQuote{logDiff} method and the \dQuote{fraction} method
#'   \item \dQuote{pa} and \dQuote{ap} force symmetry in the output distance matrix
#' }
#' @examples
#'
#' library(treenomial)
#' library(ape)
#'
#' # distance between coefficient matrices of one 10 tip tree
#' # and 100 trees with 30 tips using
#' # create the coefficient matrices
#' tenTipTree <- rtree(10)
#' tenTipTreeCoeff <- treeToPoly(tenTipTree, numThreads = 0)
#'
#' thirtyTipList <- rmtree(100, 30)
#' thirtyTipCoeffs <- treeToPoly(thirtyTipList, numThreads = 0)
#'
#' # find the distance
#' polyDist(tenTipTreeCoeff, thirtyTipCoeffs, numThreads = 0)
#' @export
polyDist <- function(x, Y, method = c("fraction","logDiff", "wLogDiff", "pa", "ap"), numThreads = -1) {
  # check input arguments
  if (is(x, "list")) {
    stop("argument x must not be a list")
  }

  if (is(Y, "matrix")) {
    Y <- list(Y)
  }

  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if(typeof(x) != typeof(Y[[1]])) stop("arguments x and Y must be of the same form")

  if (is(Y[[1]], "matrix") && typeof(Y[[1]]) == "double" && nrow(Y[[1]]) != 1) {
    coefficientMatrices <- alignCoeffs(c(list(x), Y), type = "default")
    coeffDist(coefficientMatrices, method = method, nThreads = numThreads)
  } else if (is(Y[[1]], "matrix") && (nrow(Y[[1]]) == 1)) {
    if (method != "logDiff"  &&  method != "fraction") warning("only the logDiff and fraction method is available for this polynomial")
    coefficientMatrices <- alignCoeffs(c(list(x), Y), type = "yEvaluated")
    if(method == "logDiff"){
      coeffDist(coefficientMatrices, method = "logDiffComplex", nThreads = numThreads)
    } else {
      coeffDist(coefficientMatrices, method = "fractionComplex", nThreads = numThreads)
    }

  } else if (is(Y[[1]], "matrix") && (typeof(Y[[1]]) == "complex")) {
    if (method != "logDiff") warning("only the logDiff method is available for this polynomial")
    coefficientMatrices <- alignCoeffs(c(x, Y), type = "tipLab")
    coeffDist(coefficientMatrices, method = "tipLab", nThreads = numThreads)
  } else {
    stop("invalid input")
  }
}


#' Calculates the distance between trees
#'
#' Calculates the distance between two trees or a tree and a list of trees.
#'
#' @param x single phylo object
#' @param Y a list of phylo objects
#' @inheritParams treeToDistMat
#' @return vector of distances
#' @note \itemize{
#'   \item the substituted y coefficient vector only supports the \dQuote{logDiff} method and the \dQuote{fraction} method
#'   \item \dQuote{pa} and \dQuote{ap} force symmetry in the output distance matrix
#' }
#' @examples
#'
#' library(treenomial)
#' library(ape)
#'
#' # distance between one 10 tip tree and 100 trees with 30 tips
#'
#' # generate the trees
#' tenTipTree <- rtree(10)
#' thirtyTipList <- rmtree(100, 30)
#'
#' # find the distance
#' treeDist(tenTipTree, thirtyTipList, numThreads = 0)
#' @export
treeDist <- function(x, Y, type = c("default","yEvaluated","tipLabel"), method = c("fraction","logDiff", "wLogDiff", "pa", "ap"), y, numThreads = -1) {
  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if (!missing(type) & length(type) > 1) stop("only one 'type' allowed")
  type <- match.arg(type)

  if (is(x, "list")) {
    stop("argument x must not be a list")
  }

  if (is(Y, "phylo")) {
    Y <- list(Y)
  }

  if(typeof(x) != typeof(Y[[1]])) stop("arguments x and Y must be of the same form")

  coeffs <- treeToPoly(c(list(x), Y), type = type, y = y, numThreads = numThreads)
  coeffs <- alignPoly(coeffs)

  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if(type == "default"){
    coeffDist(coeffs, method = method, nThreads = numThreads)
  } else if(type == "yEvaluated") {
    if(method == "logDiff"){
      coeffDist(coeffs, method = "logDiffComplex", nThreads = numThreads)
    } else {
      coeffDist(coeffs, method = "fractionComplex", nThreads = numThreads)
    }
  } else if(type == "tipLabel"){
    coeffDist(coeffs, method = "tipLab", nThreads = numThreads)
  }
}

#' Calculates the distance matrix from a list coefficient matrices
#'
#'
#' @param coefficientMatrices list of coefficient matrices
#' @param method method to use when calculating coefficient distances:
#' \describe{
#'   \item{\dQuote{fraction}}{for two coefficient matrices A and B returns sum(abs(A-B)/(A+B)), excluding elements where both A and B are zero}
#'   \item{\dQuote{logDiff}}{for two coefficient matrices A and B returns sum(log(1+abs(A-B))}
#'   \item{\dQuote{wLogDiff}}{performs the \dQuote{logDiff} method with weights on the rows}
#'   \item{\dQuote{pa}}{total pairs where the coefficient is present in one matrix and absent in the other (presence-absence)}
#'   \item{\dQuote{ap}}{opposite comparison of pa (absence-presence)}
#' }
#' @return distance matrix calculated from argument coefficient matrices
#' @note \itemize{
#'   \item the substituted y coefficient vector only supports the \dQuote{logDiff} method and the \dQuote{fraction} method
#'   \item \dQuote{pa} and \dQuote{ap} force symmetry in the output distance matrix
#' }
#' @param numThreads number of threads to be used, the default (-1) will use the number of cores in the machine and numThreads = 0 will only use the main thread
#' @examples
#'
#' library(treenomial)
#' library(ape)
#'
#' # coefficient matrices for ten trees of 20 tips
#' coeffs <- treeToPoly(rmtree(10, 20), numThreads = 0)
#'
#' # distance matrix from the list of coefficient matrices
#' d <- polyToDistMat(coeffs, method = "logDiff", numThreads = 0)
#'
#' # using the absence-presence method
#' d <- polyToDistMat(coeffs, method = "ap", numThreads = 0)
#' @export
polyToDistMat <- function(coefficientMatrices, method = c("fraction","logDiff", "wLogDiff", "pa", "ap"), numThreads = -1) {
  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if(!is(coefficientMatrices, "list")){
    stop("input must be a list of coefficient matrices")
  }

  if(is(coefficientMatrices[[1]],"matrix") && typeof(coefficientMatrices[[1]]) == "double" && nrow(coefficientMatrices[[1]]) != 1){
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "default")
    distMat <- coeffDistMat(coefficientMatrices, method = method, nThreads = numThreads)

  } else if(is(coefficientMatrices[[1]],"matrix") && (nrow(coefficientMatrices[[1]]) == 1)){
    if (method != "logDiff" && method != "fraction") warning("only the logDiff and fraction method is available for this polynomial")
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "yEvaluated")
    if(method == "logDiff"){
      distMat <- coeffDistMat(coefficientMatrices, method = "logDiffComplex", nThreads = numThreads)
    } else {
      distMat <- coeffDistMat(coefficientMatrices, method = "fractionComplex", nThreads = numThreads)
    }

  } else if(is(coefficientMatrices[[1]],"matrix") && (typeof(coefficientMatrices[[1]]) == "complex")){
    if (method != "logDiff") warning("only the logDiff method is available for this polynomial")
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "tipLab")
    distMat <- coeffDistMat(coefficientMatrices, method = "tipLab", nThreads = numThreads)

  } else {
    stop("invalid input")
  }

  rownames(distMat) <- names(coefficientMatrices)
  colnames(distMat) <- names(coefficientMatrices)

  return(distMat)
}

#' Calculates the distance matrix from a list of phylo objects
#' @inheritParams polyToDistMat
#' @inheritParams treeToPoly
#' @return a distance matrix
#' @note \itemize{
#'   \item the substituted y coefficient vector only supports the \dQuote{logDiff} method and the \dQuote{fraction} method
#'   \item \dQuote{pa} and \dQuote{ap} force symmetry in the output distance matrix
#' }
#' @export
#' @examples
#'
#' library(treenomial)
#' library(ape)
#' # distance matrix for 10 trees of 30 tips
#' treeToDistMat(rmtree(10, 30), method = "wLogDiff", numThreads = 0)
#' @export
treeToDistMat <- function(trees, method = c("fraction","logDiff", "wLogDiff", "pa", "ap"), type = c("default","yEvaluated","tipLabel"), y, numThreads = -1) {
  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if (!missing(type) & length(type) > 1) stop("only one 'type' allowed")
  type <- match.arg(type)

  polyToDistMat(treeToPoly(trees, type = type, y = y, numThreads = numThreads), method = method, numThreads = numThreads)
}

#' Plot the min/max distance trees from a target tree
#'
#' @param target the phylo object of the tree to calculate the distances to
#' @param trees a list of phylo objects to compare with the \strong{target}
#' @param n the number of trees to find and plot
#' @param comparison whether to find the \dQuote{min} or the \dQuote{max} distance trees from the \strong{target}
#' @return a list of lists containing the \strong{n} min/max distance trees and their distances to \strong{target}
#' @inheritParams treeToDistMat
#' @note \itemize{
#'   \item the substituted y coefficient vector only supports the \dQuote{logDiff} method and the \dQuote{fraction} method
#'   \item \dQuote{pa} and \dQuote{ap} force symmetry in the output distance matrix
#' }
#' @examples
#'
#' library(treenomial)
#' library(ape)
#' trees <- c(rmtree(1000, 50), rmtree(10, 9))
#' target <- rtree(50)
#' minTrees <- plotExtremeTrees(target, trees, 2, comparison = "min", numThreads = 0)
#' @export
plotExtremeTrees <- function(target, trees, n, comparison = "min", method = c("fraction","logDiff", "wLogDiff", "pa", "ap"), type = c("default","yEvaluated","tipLabel"), y, numThreads = -1) {
  if (!missing(method) & length(method) > 1) stop("only one 'method' allowed")
  method <- match.arg(method)

  if (!missing(type) & length(type) > 1) stop("only one 'type' allowed")
  type <- match.arg(type)

  if (is(target, "list")) {
    stop("argument target must not be a list")
  }

  if (is(trees, "phylo")) {
    trees <- list(trees)
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  distances <- treeDist(target, trees, type = type, method = method, y = y, numThreads = numThreads)

  # if greater then 16 trees break up over multiple pages
  if (n < 15) {
    par(mfrow = c(ceiling(n / 4), ifelse(ceiling(n / 4) == 1, 1 + n, 4)), oma = c(2, 0, 2, 0))
  } else {
    par(mfrow = c(4, 4), oma = c(2, 0, 2, 0))
  }

  target$edge.length <- rep(1, length(target$edge.length))

  plot.phylo(ladderize(target), show.tip.label = FALSE, main = "Target:", use.edge.length = T)

  if (comparison == "min") {
    orderMin <- order(distances)

    minList <- vector("list", n)

    for (i in 1:n) {
      minTree <- trees[[orderMin[i]]]
      minTree$edge.length <- rep(1, length(minTree$edge.length))

      currName <- names(trees)[orderMin[i]]
      minTitle <- ifelse(!is.null(currName), paste(currName, "tree distance: ", signif(distances[orderMin[i]], 6)), paste("Distance: ", signif(distances[orderMin[i]], 6)))

      plot.phylo(ladderize(minTree), show.tip.label = FALSE, main = minTitle, use.edge.length = T)

      minList[[i]] <- list(tree = minTree, distance = distances[[orderMin[i]]])
    }

    return(minList)
  } else if (comparison == "max") {
    orderMax <- order(distances, decreasing = TRUE)

    maxList <- vector("list", n)

    for (i in 1:n) {
      maxTree <- trees[[orderMax[i]]]
      maxTree$edge.length <- rep(1, length(maxTree$edge.length))

      currName <- names(trees)[orderMax[i]]
      minTitle <- ifelse(!is.null(currName), paste(currName, "tree distance: ", signif(distances[orderMax[i]], 6)), paste("Distance: ", signif(distances[orderMax[i]], 6)))

      plot.phylo(ladderize(maxTree), show.tip.label = FALSE, main = minTitle, use.edge.length = T)

      maxList[[i]] <- list(tree = maxTree, distance = distances[[orderMax[i]]])
    }

    return(maxList)
  } else {
    stop("invalid comparison")
  }
}
