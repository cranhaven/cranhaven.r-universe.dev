#' Replace invalid fatty acid signature proportions
#'
#' The utility function \code{sig_rep_zero} replaces fatty acid signature
#' proportions that are less than or equal to zero or missing with a small
#' constant and uses the multiplicative method (Martin-Fernandez et al. 2011)
#' to scale the proportions to sum to 1.
#'
#'
#' @param sig_data  A numeric matrix containing signature data as either
#'   proportions or percentages in column-major format.
#' @param zero_rep  A constant associated with the method and value to be
#'   used to replace invalid values.  See Details. Default value 75.
#'
#' @return A list containing the following elements: \describe{
#'   \item{sig_adj}{The signature data with non-positive or missing proportions
#'                  replaced and scaled to sum to 1.0.}
#'   \item{rep_val}{The value used to replace invalid proporitons.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#'
#' @section Details:
#' The function \code{sig_rep_zero} is an internal utility function.
#'
#' The Kullback-Leibler (Iverson et al. 2004) and Aitchison (Stewart et al.
#' 2014) distance measures are not defined for proportions of zero.
#' Consequently, if either of these distance measures will be used in an
#' analysis, the argument \code{zero_rep} should be strictly greater than 0.
#' The chi-square distance measure (Stewart et al. 2014) is defined for
#' proportions of zero, so if that distance measure will be used in the
#' analysis, the argument \code{zero_rep} may equal zero.  For simulation or
#' other comparative work involving multiple distance measures, it may be
#' advisable to use a common value to replace zeros.
#'
#' The argument \code{zero_rep} must be either:\itemize{
#'   \item Greater than or equal to 0 and no greater than 0.01, in which case
#'     the specified value is used to replace invalid proportions.
#'   \item Between 10 and 100, with an uninformed  default of 75.  In this
#'     case, \code{zero_rep} is interpreted as a percentage.  The smallest
#'     non-zero proportion in \code{sig_data} is multiplied by the percentage
#'     and divided by 100.  The result is used to replace invalid proportions.
#'   }
#'
#' Although Bromaghin et al. (2016) found that scaling signatures
#' by varying constants introduces a bias in diet estimation, the slight
#' distortion of the signatures caused by replacing invalid proportions
#' with a small constant that varies between signatures is unlikely to
#' introduce meaningful bias.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. 2016. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research} 31:597-606.
#'
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' Martin-Fernandez, J.A., J. Palarea-Albaladejo, and R.A. Olea. 2011.
#'   Dealing with zeros. P. 43-58 in V. Pawlowsky-Glahn and A. Buccianto,
#'   eds. Compositional data analysis: theory and application. John Wiley,
#'   Chichester.
#'
#' Stewart, C., and C. Field. 2011. Managing the essential zeros in quantitative
#'  fatty acid signature analysis. \emph{Journal of Agricultural, Biological, and
#'  Environmental Statistics} 16:45?69.
#'
###############################################################################


sig_rep_zero <- function(sig_data, zero_rep = 75){

  # Initialize return values.
  sig_adj <- NA
  rep_val <- NA



  # Check whether signature inputs are valid.
  # This is mostly redundant to related error checks in fas_prep(), but I
  # decided to leave it in.
  have_sig_mat <- is.matrix(sig_data) & is.numeric(sig_data)
  have_sig_vec <- is.vector(x=sig_data, mode="numeric")

  if(have_sig_mat==FALSE & have_sig_vec==FALSE){
    err_code <- 1
    err_message <- "A valid matrix of signature data was not passed."

    return(list(sig_adj=sig_adj, rep_val=rep_val,
                err_code=err_code, err_message=err_message))
  }



  # Check that the value of zero_rep is valid.
  if(!((zero_rep >= 0 & zero_rep <= 0.01) |
       (zero_rep >= 10 & zero_rep <= 100))){
    err_code <- 2
    err_message <- paste("The argument zero_rep must be between 0 and 0.01,",
                         "inclusive, between 10 and 100.", sep = " ")

    return(list(sig_adj=sig_adj, rep_val=rep_val,
                err_code=err_code, err_message=err_message))
  }



  # Preliminary preparation of the signature data.
  if(have_sig_mat){

    # Copy original data to a new matrix
    sig_adj <- sig_data


    # Identify proportions that are less than or equal to zero or missing, set
    # them equal to zero, and scale to sum to 1 using the multiplicative method.
    zero_prop <- (sig_adj <= 0) | is.na(sig_adj)
    pos_prop <- !zero_prop
    sig_adj[zero_prop] <- 0
    sums <- matrix(data=colSums(sig_adj), nrow=nrow(sig_adj),
                   ncol=ncol(sig_adj), byrow=TRUE)
    sig_adj <- sig_adj/sums
  } else if(have_sig_vec){

    # Copy original data to a new vector.
    sig_adj <- sig_data


    # Identify proportions that are less than or equal to zero or missing, set
    # them equal to zero, and scale to sum to 1 using the multiplicative method.
    zero_prop <- (sig_adj <= 0) | is.na(sig_adj)
    pos_prop <- !zero_prop
    sig_adj[zero_prop] <- 0
    sig_adj <- sig_adj/sum(sig_adj)
  }



  # Determine whether proportions of zero should be replaced by a small constant
  # or by a percentage of the smallest nonzero value in the data.
  if(zero_rep < 1){
    rep_val <- zero_rep
  } else{
    rep_val <- (zero_rep/100)*min(sig_adj[pos_prop])
  }



  # Process signature data.
  if(have_sig_mat){

    # Replace zeros with a small constant.
    sig_adj[zero_prop] <- rep_val


    # Rescale using the multiplicative method.
    adj_fact <- 1.0 - rep_val*colSums(zero_prop)
    adj_fact <- matrix(data=adj_fact, nrow=nrow(sig_adj), ncol=ncol(sig_adj),
                       byrow=TRUE)
    sig_adj[pos_prop] <- sig_adj[pos_prop]*adj_fact[pos_prop]
  } else if(have_sig_vec){

    # Replace zeros with a small constant.
    sig_adj[zero_prop] <- rep_val


    # Rescale using the multiplicative method.
    adj_fact <- 1.0 - rep_val*sum(zero_prop)
    sig_adj[pos_prop] <- sig_adj[pos_prop]*adj_fact
  }



  # Return
  err_code <- 0
  err_message <- "Success!"

  return(list(sig_adj=sig_adj, rep_val=rep_val, err_code=err_code,
              err_message=err_message))
}
