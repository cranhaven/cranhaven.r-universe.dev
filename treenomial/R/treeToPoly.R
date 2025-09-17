#' Convert trees to coefficient matrices
#'
#' Converts rooted full binary trees to tree distinguishing polynomials described with coefficient matrices.
#'
#' @param trees a single phylo object or a list of phylo objects
#' @param type one of:
#' \describe{
#'   \item{\dQuote{real}}{tree distinguishing polynomials in two variables x (columns) and y (rows)}
#'   \item{\dQuote{yEvaluated}}{tree distinguishing polynomials with y evaluated at a specified argument}
#'   \item{\dQuote{tipLabel}}{complex coefficient polynomial that utilize binary trait tip labels on the phylo objects}
#' }
#' @return the resulting coefficient matrix or matrices of the form:
#' \describe{
#'   \item{\dQuote{real}}{a real matrix where the ith row, jth column represents the x^(j-1)*y^(i-1) coefficient}
#'   \item{\dQuote{yEvaluated}}{a vector where the kth column represents the x^(k-1) coefficient}
#'   \item{\dQuote{tipLabel}}{given trees with two unique tip labels \dQuote{a}, \dQuote{b} a complex matrix where the ith row, jth column represents the a^(i-1)*b^(j-1) coefficient}
#' }
#' @param y the y value to evaluate the polynomial at when type is \dQuote{yEvaluated}, ignored otherwise
#' @param varLabels boolean for whether to add row and column names corresponding to the variables in the polynomial
#' @param numThreads number of threads to be used, the default (-1) will use the number of cores in the machine and numThreads = 0 will only use the main thread
#' @importFrom ape as.phylo
#' @useDynLib treenomial
#' @importFrom Rcpp sourceCpp
#' @importFrom methods is
#' @examples
#' library(treenomial)
#' library(ape)
#'
#' # generate a tree
#' tree <- rtree(n = 30, rooted = TRUE)
#'
#' # a real coefficient matrix
#' treeToPoly(tree, varLabels = TRUE, numThreads = 0)
#'
#' # complex coefficient vector for the tree
#' treeToPoly(tree, type = "yEvaluated", y = 1+1i, varLabels = TRUE, numThreads = 0)
#'
#' # for a list of trees
#' treeToPoly(rmtree(4, 20), varLabels = TRUE, numThreads = 0)
#'
#' @export
treeToPoly <- function(trees,type = c("default","yEvaluated","tipLabel"), y, varLabels = FALSE, numThreads = -1) {

  if(!missing(type) & length(type)>1) stop("only one 'type' allowed")
  type <- match.arg(type)

  # check input format
  if(!is(trees,"phylo") && !is(trees,"list") && !is(trees,"multiPhylo")){
    tryCatch({
      trees <- as.phylo(trees)
    }, error = function(e) {
      stop("incorrect input format, trees must be phylo or list of phylo objects")
    })
  } else if (!is(trees,"phylo") && !is(trees,"multiPhylo")){
    tryCatch({
      trees <- lapply(trees, as.phylo)
      singleTree <- FALSE
    }, error = function(e) {
      stop("incorrect input format, trees must be phylo or list of phylo objects")
    })
  }

  singleTree <- is(trees,"phylo")

  if(type == "default"){
    if(singleTree) trees <- list(trees)

    wedgeSeq <- lapply(trees, function(x) {
      inds <- unique(rev(x$edge[x$edge[, 1] >= length(x$tip.label), ]))
      ifelse(inds <= length(x$tip.label), "0", "1")
    })

    coeffs <- coeffMatList(wedgeSeq, y = 0, type = type, nThreads = numThreads)

    attributes(coeffs) <- NULL
    if(!is.null(names(trees))){
      names(coeffs) <- names(trees)
    }

    if(varLabels) coeffs <- varLabels(coeffs,"default")

  } else if(type == "yEvaluated"){

    if(singleTree) trees <- list(trees)

    wedgeSeq <- lapply(trees, function(x) {
      inds <- unique(rev(x$edge[x$edge[, 1] >= length(x$tip.label), ]))
      ifelse(inds <= length(x$tip.label), "0", "1")
    })

    coeffs <- coeffMatList(wedgeSeq, y = y, type = type, nThreads = numThreads)

    attributes(coeffs) <- NULL
    if(!is.null(names(trees))){
      names(coeffs) <- names(trees)
    }

    if(varLabels) coeffs <- varLabels(coeffs,"yEvaluated")

  } else if(type == "tipLabel"){

    if(singleTree) trees <- list(trees)

    uniqueTipLabFirst <- sort(unique(trees[[1]]$tip.label))

    if (length(uniqueTipLabFirst) == 1) {
      uniqueTipLabFirst <- rep(uniqueTipLabFirst, 2)
    } else if (length(uniqueTipLabFirst) > 2) {
      stop("only phylo trees with two unique tip labels are currently supported")
    }

    lapply(trees, function(i){
      uniqueTipLab <- unique(i$tip.label)

      if (length(uniqueTipLab) == 1) {
        uniqueTipLab <- rep(uniqueTipLab, 2)
      } else if (length(uniqueTipLab) > 2) {
        stop("only phylo trees with two unique tip labels are currently supported")
      }
    })

    if(any(uniqueTipLabFirst == "1")){
      trees <- lapply(trees, function(i) {
        i$tip.label <-  ifelse(i$tip.label == "1", "A",i$tip.label)
        return(i)
      })
    }

    wedgeSeq <- lapply(1:length(trees), function(i) {
      x <- trees[[i]]

      inds <- unique(rev(x$edge[x$edge[, 1] >= length(x$tip.label), ]))
      ifelse(inds <= length(x$tip.label), x$tip.label[inds], "1")
    })

    coeffs <- coeffMatList(wedgeSeq, y = 0, type = type, tipLabA = uniqueTipLabFirst[[1]], tipLabB = uniqueTipLabFirst[[2]], nThreads = numThreads)

    attributes(coeffs) <- NULL
    if(!is.null(names(trees))){
      names(coeffs) <- names(trees)
    }

    if(varLabels) coeffs <- varLabels(coeffs,"tipLabel",uniqueTipLabFirst)
 }

  if(singleTree) coeffs <- coeffs[[1]]

 return(coeffs)
}

varLabels <- function(coeffs, type,uniqueTipLabFirst){
  if(type == "tipLabel"){
    coeffs <- lapply(coeffs, function(i){
      if(!is.null(nrow(i))){
        rownames(i) <- paste0(rep(uniqueTipLabFirst[[2]],nrow(i)),"^",0:(nrow(i)-1))
      }

      if(!is.null(ncol(i))){
        colnames(i) <- paste0(rep(uniqueTipLabFirst[[1]],ncol(i)),"^",0:(ncol(i)-1))
      }

      return(i)
    })
  } else if(type == "default") {
    coeffs <- lapply(coeffs, function(i){
      if(!is.null(nrow(i))){
        rownames(i) <- paste0("y^",0:(nrow(i)-1))
      }

      if(!is.null(ncol(i))){
        colnames(i) <- paste0("x^",0:(ncol(i)-1))
      }

      return(i)
    })
  } else if(type == "yEvaluated") {
    coeffs <- lapply(coeffs, function(i){
      if(!is.null(ncol(i))){
        colnames(i) <- paste0("x^",0:(ncol(i)-1))
      }

      return(i)
    })
  }
  return(coeffs)
}


#' Align various types of coefficient matrices
#'
#' @param coefficientMatrices a list of coefficient matrices of various sizes
#' @return the aligned list of coefficient matrices
#' @useDynLib treenomial
#' @importFrom Rcpp sourceCpp
#' @details
#' Alignment depends on the type of coefficient matrix:
#' \describe{
#'   \item{\dQuote{real}}{the smaller matrices columns are prepended with zero columns to align with the max number of columns and the rows are appended with zero rows to match the max number of rows}
#'   \item{\dQuote{yEvaluated}}{the smaller vectors are appended with zeroes to match the max length vector}
#'   \item{\dQuote{tipLabel}}{the smaller matrices are appended with zeroes to match the max number of rows and columns}
#' }
#' @examples
#'
#' library(treenomial)
#' library(ape)
#' differentSizeTrees <- c(rtree(2), rmtree(10,10))
#' coeffs <- treeToPoly(differentSizeTrees, numThreads = 0)
#' alignedCoeffs <- alignPoly(coeffs)
#'
#'
#' @export
alignPoly <- function(coefficientMatrices){
  if(!is(coefficientMatrices, "list")){
    stop("input must be a list of coefficient matrices")
  }

  if(is(coefficientMatrices[[1]],"matrix") && typeof(coefficientMatrices[[1]]) == "double" && nrow(coefficientMatrices[[1]]) != 1){
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "default")

  } else if(is(coefficientMatrices[[1]],"matrix") && (nrow(coefficientMatrices[[1]]) == 1)){
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "yEvaluated")

  } else if(is(coefficientMatrices[[1]],"matrix") && (typeof(coefficientMatrices[[1]]) == "complex")){
    coefficientMatrices <- alignCoeffs(coefficientMatrices, type = "tipLabel")
  } else {
    stop("invalid input")
  }

  return(coefficientMatrices)
}
