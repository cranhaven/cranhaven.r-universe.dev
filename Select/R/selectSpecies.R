#' Select species based on traits
#'
#' This function returns a probability distribution for a species pool based on their traits and a desired trait profile (Laughlin 2014). It can simultaneously constrain specific trait value(s) and optimise functional diversity.
#'
#' @param t2c "Traits to constrain": A matrix of species trait values, where species are organized as rows and traits as columns. These should be numeric arrays, and no missing values are tolerated. Ordinal data will yield meaningful results, but nominal data entered as numeric vectors are invalid. Row and column names are optional. This matrix can be any dimension, but the number of traits should be less than the number of species.
#' @param constraints Trait constraints: A vector of trait values that serve as constants in the constraint equations. If not specified, the function will not constrain solutions to trait means. These trait contraints must be listed in the same order as columns in t2c. These constraints are community-weighted mean trait values, i.e., average traits weighted by the relative abundance of each species.
#' @param t2d "Traits to diversify": Can be 1) a matrix of species trait values to diversify where species are organized as rows and traits as columns. NAs are tolerated as long as each pair of species can be compared by at least one trait. In this case, dissimilarities among species are computed using Euclidean distance. The number of species in t2d must match those in t2c. Or 2) a distance matrix of class 'dist' that contains dissimilarities among species, no NAs are tolerated in the distance matrix.
#' @param obj Objective function: The objective function to optimise, one of three possibilities = c("QH", "Q", "H"). QH = Quadratic entropy (Q) plus entropy (H'); Q = Quadratic entropy; H = entropy.
#' @param phi A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy (default = 0.5). Phi of 1 corresponds to 100 percent Q, phi of 0.5 corresponds to 50 percent Q and 50 perfect H', phi of 0 corresponds to 100 percent H'.
#' @param capd A logical stating whether the distance matrix should be capped at the mean distance among species (default = FALSE). Mean distance is calculated as the average of all upper triangular entries in the distance matrix calculated from t2d.
#' @param euclid A logical stating whether the distance matrix should be transformed to an Euclidean matrix if necessary (default = TRUE).
#' @return A list with the elements: \item{prob}{Probabilities, i.e. optimal solutions of species relative abundance} \item{cwm_t2d}{Community-weighted mean trait values of resulting community for traits that were diversified, computed as probabilities x t2d using matrix multiplication} \item{cwm_t2c}{Community-weighted mean trait values of resulting community for traits that were constrained, computed as probabilities x t2d using matrix multiplication} \item{H}{Final entropy of community} \item{Q}{Final Rao Q of community} \item{objval}{Values of the objective function being maximized. The last value is the maximum.} \item{lagrange}{Lagrange multipliers.} \item{hessian}{The Hessian at the optimal solution.}
#' @examples
#' ### 1 trait constraint with maximum entropy
#' Spp <- 5 #S = number of species
#' trait <- as.matrix(data.frame(trait=c(1:Spp)))
#' rownames(trait) <- c(letters[1:nrow(trait)])
#' result1 <- selectSpecies(t2c=trait, constraints=c(trait=3.5), t2d=trait, obj="H", capd=FALSE)

#' ### compare result1 with virtually identical maxent output from FD package
#' #FD::maxent(constr=c(3.5),states=trait)$prob

#' ### 1 trait constraint with maximum functional diversity
#' result2 <- selectSpecies(t2c=trait, constraints=c(trait=3.5), t2d=trait, obj="Q", capd=FALSE)

#' ### 1 trait constraint with maximum functional diversity and entropy
#' result3 <- selectSpecies(t2c=trait, constraints=c(trait=3.5), t2d=trait, obj="QH", capd=FALSE)
#'
#' ### Plot results
#' plotProbs(result1,trait, xlab="Trait")
#' plotProbs(result2,trait, xlab="Trait")
#' plotProbs(result3,trait, xlab="Trait")
#'
#' ### 1 trait and no trait constraint
#' result4 <- selectSpecies(t2d=trait, obj="QH", capd=FALSE)
#' plotProbs(result4,trait, xlab="Trait")
#'
#' ##### 2 traits: Constrain trait X at X=3, diversify trait Y
#' traitX <- matrix(c(rep(1,4),rep(2,4),rep(3,4),rep(4,4)))
#' traitY <- matrix(c(rep(c(1,2,3,4),4)))
#' rownames(traitX) <- c(letters[1:16]); colnames(traitX) <- c("traitX")
#' rownames(traitY) <- c(letters[1:16]); colnames(traitY) <- c("traitY")
#'
#' result5 <- selectSpecies(t2c=traitX,constraints=c(traitX=3),t2d=traitY,obj="Q",capd=FALSE)
#' result6 <- selectSpecies(t2c=traitX,constraints=c(traitX=3),t2d=traitY,obj="QH",capd=TRUE)
#'
#' trait.matrix <- cbind(traitX, traitY)
#' plotProbs(result5,trait.matrix)
#' plotProbs(result6,trait.matrix)
#'
#' ##### 3 traits: Constrain trait Z to value 2.5, diversify trait X and Y
#' traitZ <- as.matrix(data.frame(c(1,3,2,2,3,1,2,3,1,2,1,3,2,3,2,2)))
#' rownames(traitZ) <- c(letters[1:16]); colnames(traitZ) <- c("traitZ")
#' result7 <- selectSpecies(t2c=traitZ,constraints=c(traitZ=2.5),t2d=trait.matrix, capd=TRUE, obj="QH")
#' plotProbs(result7,trait.matrix)
#'
#' @references Laughlin, D.C. 2014. Applying trait-based models to achieve functional targets for theory-driven ecological restoration. Ecology Letters, 17, 771-784.
#' @export

selectSpecies <- function (t2c = NULL, constraints = NULL, t2d, obj = c("QH", "Q", "H"),
                           phi = 0.5, capd = FALSE, euclid = TRUE)
{
  # Argument checking  -------------------------
  if (!is.null(t2c)){
    if (!inherits(t2c, "matrix") | !is.numeric(t2c)) {
      stop("object \"t2c\" is not a numerical matrix")
    }
    obj <- match.arg(obj)
    if (ncol(t2c) >= nrow(t2c) - 1) {
      stop("There are more traits than species. Use fewer traits or more species.")
    }

    if (any(is.na(t2c))) {
      stop("Traits to constrain \"t2c\" contains NA")
    }
    if (is.null(colnames(t2c))) warning("\"t2c\" colname vector (traits) is empty")
    if (is.null(rownames(t2c))) warning("\"t2c\" rowname vector (species) is empty")
  }

  if (phi < 0 | phi > 1 | !is.numeric(phi)) {
    stop("phi must be between 0 and 1")
  }

  if (inherits(t2d, "matrix") & is.numeric(t2d)) {
    if (is.null(colnames(t2d))) warning("\"t2d\" colname vector (traits) is empty")
    if (is.null(rownames(t2d))) warning("\"t2d\" rowname vector (species) is empty")

    d <- stats::dist(t2d, upper = TRUE, diag = TRUE)
  } else if (inherits(t2d, "dist")){
    d <- t2d
    if (is.null(names(t2d))) warning("\"t2d\" name vector (species) is empty")
  } else {
    stop("object \"t2d\" must be a numerical trait matrix or a \"dist\" matrix")
  }
  if (any(is.na(d))) stop("Distance matrix \"t2d\" contains NA")

  oldw <- getOption("warn")
  options(warn = -1)
  test <- !ade4::is.euclid(d)
  options(warn = oldw)

  if (euclid & test)  d <- as.matrix(ade4::quasieuclid(stats::as.dist(d)))

  d <- as.matrix(d)
  if (!is.null(t2c)){
    if (nrow(d) != nrow(t2c)) {
      stop("objects \"t2d\" and \"t2c\" do not describe the same number of species")
    }
    if (!is.null(row.names(d)) & !is.null(row.names(t2c)) & !identical(row.names(d), row.names(t2c))) {
      stop("\"t2d\" and \"t2c\" species names do not match")
    }
  }
  if (!is.null(constraints)){
    if (is.null(t2c)){
      stop("If \"constraints\" is specified, \"t2c\" must be specified")
    }
    if (length(constraints) != ncol(t2c)) {
      stop("Number of trait constraints number must be equal to the number of traits to constrain.")
    }
    if (!is.numeric(constraints) | any(is.na(constraints))) {
      stop("Trait constraint must all be numerical non-missing values.")
    }
    range_t2c <- apply(t2c,2, range)
    if (any(range_t2c[1,] > constraints | range_t2c[2,] < constraints)) {
      stop("Trait constraint must be within the range of available trait values in the species pool.")
    }
    if (!is.null(names(constraints)) & !is.null(colnames(t2c)) & !identical(names(constraints), colnames(t2c))) {
      stop("\"constraints\" and \"t2c\" trait names don't match")
    }
    if (is.null(names(constraints))) warning("\"constraints\" name vector is empty")
  }
  # End of argument checking -------------------------

  N <- nrow(d)
  mean.dist <- mean(d[upper.tri(d, diag = FALSE)])
  dcap.fun <- function(d) {
    d[d > mean.dist] <- mean.dist
    return(d)
  }
  if (capd) {
    d <- dcap.fun(d)
  }
  mean.t2d <- mean(t2d)
  tdist <- abs(t2d - mean.t2d)
  tdist[tdist == 0] <- 0.001

  # Diversity functions to optimize
  qh <- function(p) {
    -(t(p) %*% (d/2) %*% p * phi + -(t(p) %*% log(p)) * (1 - phi))
  }
  q <- function(p) {
    -(t(p) %*% (d/2) %*% p)
  }
  h <- function(p) {
    -(-(t(p) %*% log(p)))
  }

  if (!is.null(constraints)) {
    eqfun <- function(p) {
      z1 <- sum(p)
      cwms <- t(as.matrix(p)) %*% t2c
      return(c(z1, cwms))
    }
    all.constraints <- c(1, constraints)
  } else{
    eqfun <- function(p) {
      z1 <- sum(p)
      return(z1)
    }
    all.constraints <- 1
  }

  oldw <- getOption("warn")
  options(warn = -1)

  # Estimation of species probability distribution
  if (obj == "QH") {
    res <- Rsolnp::solnp(pars = c(rep(1/N, N)), fun = qh,
                         eqfun, eqB = all.constraints, LB = c(rep(0, N)),
                         UB = c(rep(1, N)))
  } else if (obj == "Q") {
    res <- Rsolnp::solnp(pars = c(rep(1/N, N)), fun = q, eqfun,
                         eqB = all.constraints, LB = c(rep(0, N)),
                         UB = c(rep(1, N)))
  } else if (obj == "H") {
    res <- Rsolnp::solnp(pars = c(rep(1/N, N)), fun = h, eqfun,
                         eqB = all.constraints, LB = c(rep(0, N)),
                         UB = c(rep(1, N)))
  }

  # Construction of result object
  result = list()
  result$prob <- as.matrix(res$pars)
  if (inherits(t2d, "matrix")){
    rownames(result$prob) <- row.names(t2d)
    result$cwm_t2d <- as.matrix(res$pars %*% t2d)
  }else{
    rownames(result$prob) <- names(t2d)
  }
  if (!is.null(t2c)){
    result$cwm_t2c <- as.matrix(res$pars %*% t2c)
  }
  result$H <- h(res$pars)*-1
  result$Q <- q(res$pars)*-1
  result$objval <- as.matrix(res$values)
  result$lagrange <- as.matrix(res$lagrange)
  result$hessian <- as.matrix(res$hessian)
  options(warn = oldw)
  return(result)

}
