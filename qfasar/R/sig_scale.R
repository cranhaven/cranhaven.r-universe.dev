#' Scale fatty acid signature proportions
#'
#' The utility function \code{sig_scale} implements the three options for
#' scaling fatty acid signature data summarized by Bromaghin et al. (2016).
#' A logical vector denotes the subset of all fatty acids to be used in the
#' analysis.  The fatty acids that are not to be used are censored and one of
#' three scaling options is implemented.  See Details.
#'
#'
#' @param sig_data  A numeric matrix containing prey signature data as
#'   proportions in column-major.  These data should have previously been
#'   processed by \code{\link{sig_rep_zero}}.
#' @param fa_use A logical vector denoting the fatty acids to be used, of length
#'   equal to the total number of fatty acids. This vector originates from a
#'   data file required by \code{qfasar}. See the vignette for details.
#' @param scale An integer indicator of the desired scaling option. See
#'   Details. Default value 3.
#'
#'
#' @return A list containing the following elements: \describe{
#'   \item{n_fa}{The number of fatty acids in the processed signatures.}
#'   \item{sig}{A numeric matrix of processed signatures in column-major
#'     format.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#'
#' @section Details:
#' This is an internal utility function.
#'
#' The argument \code{scale} must be one of three integer values and its value
#' denotes the scaling option that will be implemented:\itemize{
#'   \item \code{scale} == 1.  The proportions within each censored signature
#'     are scaled to sum to 1.0.  This option is not recommended for routine
#'     use in QFASA applications, as Bromaghin et al. (2016) found that it
#'     can meaningfully bias diet estimates under some conditions.  It is
#'     implemented here to provide compatibility with original methods and to
#'     facilitate potential future research.
#'   \item \code{scale} == 2.  The proportions within each censored signature
#'     are not scaled, so each signature will have a different partial sum.
#'   \item \code{scale} == 3.  Each censored signature is augmented with an
#'     additional proportion whose value equals the sum of the censored
#'     proportions, so that the proportions in each signature sum to 1.
#'     This is the default option.
#'   }
#'
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
###############################################################################


sig_scale <- function(sig_data, fa_use, scale=3){

  # Initialize returned values.
  n_fa <- NA
  sig <- NA



  # Check that the value of scale is valid.
  if(!(scale %in% 1:3)){
    err_code <- 1
    err_message <- "The argument scale must equal 1, 2, or 3."

    return(list(n_fa=n_fa, sig=sig,
                err_code=err_code, err_message=err_message))
  }


  # Determine whether the fatty acid data consist of one or multiple signatures
  # and the number of fatty acids in a signature.  If the sig_data argument is
  # not a numeric vector or matrix, return an error.
  if(is.vector(x=sig_data, mode="numeric")){
    n_sig <- 1
    n_fa <- length(sig_data)
  } else if(is.matrix(sig_data) & is.numeric(sig_data)){
    n_sig <- ncol(sig_data)
    n_fa <- nrow(sig_data)
  } else{
    err_code <- 1
    err_message <- "Valid fatty acid signatures were not passed."

    return(list(n_fa=n_fa, sig=sig,
                err_code=err_code, err_message=err_message))
  }



  # Determine the number of fatty acids to be used.
  n_fa_use <- sum(fa_use)



  # Check if all fatty acids are to be used.
  if(n_fa_use == n_fa){
    sig <- sig_data
    err_code <- 0
    err_message <- "Success!"

    return(list(n_fa=n_fa, sig=sig,
                err_code=err_code, err_message=err_message))
  }



  # Implement the scaling option.
  if(scale == 1){

    # Isolate fatty acids to be used.
    sig <- sig_data[fa_use, ]

    # Scale to sum to 1.
    sums <- colSums(sig)
    sums <- matrix(data=sums, nrow=n_fa_use, ncol=n_sig, byrow=TRUE)
    sig <- sig/sums

    if(!is.null(rownames(sig_data))){
      rownames(sig) <- rownames(sig_data)[fa_use]
    }

  } else if(scale == 2){

    # Isolate fatty acids to be used.
    sig <- sig_data[fa_use, ]

    if(!is.null(rownames(sig_data))){
      rownames(sig) <- rownames(sig_data)[fa_use]
    }

  } else{

    # Augment the signatures.
    n_fa_use <- n_fa_use + 1
    sig <- matrix(data=0, nrow=n_fa_use, ncol=n_sig)
    sig[1:(n_fa_use-1), ] <- sig_data[fa_use, ]
    sig[n_fa_use, ] <- 1 - colSums(sig)
    if(!is.null(rownames(sig_data))){
      rownames(sig) <- c(rownames(sig_data)[fa_use], "Augmented")
    }
  }

  if(!is.null(colnames(sig_data))){
    colnames(sig) <- colnames(sig_data)
  }



  # Return.
  err_code <- 0
  err_message <- "Success!"


  return(list(n_fa=n_fa_use, sig=sig,
              err_code=err_code, err_message=err_message))
}
