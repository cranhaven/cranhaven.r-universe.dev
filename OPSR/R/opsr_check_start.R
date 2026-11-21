#' Check the User-Specified Starting Values
#'
#' This is a utility function, used in [`opsr`] and should not be used directly.
#' It is included here to document the expected structure of [`opsr`]'s `start` argument.
#' Makes sure, the start vector conforms to the expected structure. Adds the
#' expected parameter names to the numeric vector. Therefore the user has to
#' conform to the expected order. See 'Details' for further explanation.
#'
#' @param start vector of starting values.
#' @param W matrix with explanatory variables for selection process.
#' @param Xs list of matrices with expalanatory varialbes for outcome process for each regime.
#'
#' @return Named numeric vector conforming to the expected structure.
#'
#' @details
#' Expected order: 1. kappa threshold parameters (for ordered probit model),
#' 2. parameters of the selection process (names starting with `s_`), 3. parameters
#' of the outcome processes (names starting with `o[0-9]_`), 4. sigma, 5. rho.
#' If the same outcome process specification is used in the `formula`, the starting
#' values have to be repeated (i.e., the length of the `start` vector has to
#' correspond to the total number of estimated parameters in the model).
#'
#' @seealso [`opsr_2step`]
#' @export
opsr_check_start <- function(start, W, Xs) {
  nReg <- length(Xs)
  nKappa <- nReg - 1

  ## beta selection
  bs <- colnames(W)
  bs <- paste0("s_", bs)

  ## beta outcome
  bo <- unlist(lapply(seq_len(nReg), function(i) {
    paste0("o", i, "_", colnames(Xs[[i]]))
  }))

  kappa <- paste0("kappa", 1:nKappa)
  sigma <- paste0("sigma", 1:nReg)
  rho <- paste0("rho", 1:nReg)

  nn <- c(kappa, bs, bo, sigma, rho)

  if (length(start) != length(nn)) {
    stop("Start vector 'start' has wrong length. Expected length is ", length(nn),
         " but got ", length(start), ". See '?opsr_check_start' for details.")
  }

  names(start) <- nn
  start
}
