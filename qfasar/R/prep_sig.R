#' Prepare fatty acid signature data for analysis
#'
#' The function \code{prep_sig} prepares raw fatty acid signatures for
#' analysis.  Signature proportions that are missing, negative, or equal to zero
#' are replaced with a small user-specified constant and the signatures are
#' scaled to sum to 1.0. The fatty acids that are not to be used in the analysis
#' are censored and the signatures are scaled using one of three options
#' (Bromaghin et al. In press).
#'
#' @param df_sig  A data frame containing prey fatty acid signature data.
#'   \code{qfasar} has strict formatting requirements for \code{df_sig}; please
#'   see Details and/or the vignette.
#' @param fa_names A character vector of all fatty acid names.
#' @param use_fa A logical vector defining a fatty acid suite.
#' @param zero_rep A constant associated with the method and value to replace
#'   signature proportions that are missing or less than or equal to 0. Default
#'   value 75.
#' @param scale An integer indicator of the desired scaling option. Default
#'   value 3.
#'
#' @return A list containing the following elements: \describe{
#'   \item{type}{A character vector of the type of each signature.}
#'   \item{id}{A character vector of the unique sample ID of each signature.}
#'   \item{n_types}{The number of unique types.}
#'   \item{uniq_types}{A character vector of the unique types, sorted
#'     alphanumerically.}
#'   \item{n_sig}{The total number of signatures.}
#'   \item{type_ss}{The number of signatures for each unique \code{type}.}
#'   \item{loc}{A vector or matrix giving the first and last locations of the
#'     signatures of each \code{type}, after being sorted by \code{type} and
#'     \code{id}.}
#'   \item{sig_rep}{A vector or matrix of the original signatures, with any
#'     values missing or less than or equal to 0 replaced, in column-major
#'     format.}
#'   \item{n_fa_rep}{The number of fatty acids in \code{sig_rep}.}
#'   \item{sig_scale}{A vector or matrix of scaled signatures ready for
#'     analysis, sorted by \code{type} and \code{id}, in column-major format.}
#'   \item{n_fa_suite}{The number of fatty acids in \code{sig_scale}.}
#'   \item{fa_suite}{A character vector of the names of fatty acids in the suite
#'     to be used in the analysis.}
#'   \item{zero_rep_val}{A constant associated with the method and value to be
#'     used to replace proportions that are missing or less than or equal to 0.
#'     See Details.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#' @section Details:
#' This function is designed to be called by the user to prepare fatty acid
#' signatures for analysis.  For most analyses, \code{prep_sig} should be
#' called immediately after the fatty acid suites and fatty acid signatures have
#' been read into data frames, and after the fatty acid suites data frame has
#' been processed by the function \code{prep_fa}.  Please refer to the
#' vignette for additional information.
#'
#' The data frame with fatty acid signatures must meet the following formatting
#' requirements:
#'   \itemize{
#'   \item The file must be in row-major format, i.e., each row contains the
#'     information for an individual animal.
#'   \item The first column must contain a designation of animal \code{type}.
#'     For prey data, \code{type} often denotes species.  For predator data,
#'     \code{type} denotes classes of predators for which separate estimates of
#'     mean diet composition are desired.
#'   \item The second column must contain an identifier unique to each
#'     signature, i.e. a sample ID.
#'   \item The remaining columns must contain fatty acid signature proportions
#'     or percentages.
#'   \item The data frame must contain a header record, with a name for each
#'     column, such as "type", "id", name of fatty acid 1, name of fatty
#'     acid 2, ...
#'   \item The file should contain data from all available fatty acids, rather
#'     than a subset.  The fatty acid suite to be used in the analysis is
#'     defined by the argument \code{fa}.
#'   }
#'
#' Please refer to the documentation for the utility function
#' \code{\link{sig_rep_zero}} for information regarding the argument
#' \code{zero_rep}.
#'
#' Please refer to the documentation for the utility function
#' \code{\link{sig_scale}} for information regarding the argument
#' \code{scale}.
#'
#' @section References:
#' Bromaghin, J.F., S.M. Budge, and G.W. Thiemann. In press. Should fatty
#'   acid signature proportions sum to 1 for diet estimation?
#'   \emph{Ecological Research}.
#'
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' @examples
#' prep_sig(df_sig = data.frame(type = c("Type_1", "Type_1", "Type_2",
#'                                       "Type_2"),
#'                              id = c("ID_1", "ID_2", "ID_3", "ID_4"),
#'                              fa_1 = c(0.0, 0.2, 0.3, 0.6),
#'                              fa_2 = c(0.1, 0.3, 0.3, 0.4),
#'                              fa_3 = c(0.9, 0.5, 0.4, NA),
#'                              row.names = c("Prey_1", "Prey_2", "Prey_3",
#'                                            "Prey_4")),
#'          fa_names = c("fa_1", "fa_2", "fa_3"),
#'          use_fa = c(TRUE, FALSE, TRUE),
#'          zero_rep = 0.0001,
#'          scale=2)
#'
#' prep_sig(df_sig = data.frame(type = c("Type_1", "Type_1", "Type_2",
#'                                       "Type_2"),
#'                              id = c("ID_1", "ID_2", "ID_3", "ID_4"),
#'                              fa_1 = c(0.0, 0.2, 0.3, 0.6),
#'                              fa_2 = c(0.1, 0.3, 0.3, 0.4),
#'                              fa_3 = c(0.9, 0.5, 0.4, NA),
#'                              row.names = c("Prey_1", "Prey_2", "Prey_3",
#'                                            "Prey_4")),
#'          fa_names = c("fa_1", "fa_2", "fa_3"),
#'          use_fa = c(TRUE, FALSE, TRUE),
#'          zero_rep = 90,
#'          scale=1)
#'
#' prep_sig(df_sig = data.frame(type = c("Type_1", "Type_1", "Type_2",
#'                                       "Type_2"),
#'                              id = c("ID_1", "ID_2", "ID_3", "ID_4"),
#'                              fa_1 = c(0.0, 0.2, 0.3, 0.6),
#'                              fa_2 = c(0.1, 0.3, 0.3, 0.4),
#'                              fa_3 = c(0.9, 0.5, 0.4, NA),
#'                              row.names = c("Prey_1", "Prey_2", "Prey_3",
#'                                            "Prey_4")),
#'          fa_names = c("fa_1", "fa_2", "fa_3"),
#'          use_fa = c(TRUE, FALSE, TRUE),
#'          scale=3)
#'
#' prep_sig(df_sig = data.frame(type = c("Type_1", "Type_1", "Type_2",
#'                                       "Type_2"),
#'                              id = c("ID_1", "ID_2", "ID_3", "ID_4"),
#'                              fa_1 = c(0.0, 0.2, 0.3, 0.6),
#'                              fa_2 = c(0.1, 0.3, 0.3, 0.4),
#'                              fa_3 = c(0.9, 0.5, 0.4, NA),
#'                              row.names = c("Prey_1", "Prey_2", "Prey_3",
#'                                            "Prey_4")),
#'          fa_names = c("fa_1", "fa_2", "fa_3"),
#'          use_fa = c(TRUE, FALSE, TRUE))
#'
#' @export
#'
################################################################################


prep_sig <- function(df_sig, fa_names, use_fa, zero_rep=75, scale=3){


  # Initialize return values and validate inputs -------------------------------

  # Initialize returned values.
  type <- NA
  id <- NA
  n_types <- NA
  uniq_types <- NA
  n_sig <- NA
  type_ss <- NA
  loc <- NA
  sig_rep <- NA
  cc_orig <- NA
  n_fa_rep <- NA
  sig_scale <- NA
  n_fa_suite <- NA
  fa_suite <- NA
  zero_rep_val <- NA



  # Check that the signature data are in a valid data frame.
  if(!(is.data.frame(df_sig) | is.list(df_sig))){
    err_code <- 1
    err_message <- "The signature data are not in a valid data frame."
    err_chain <- "sig_prep"

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }



  # Check contents of the fatty acid signature data ----------------------------

  # Check that the first two columns are factors and the remaining columns are
  # numeric.
  are_numeric <- TRUE
  for(li1 in 3:ncol(df_sig)){
    if(mode(df_sig[,li1])!="numeric"){
      are_numeric <- FALSE
      break
    }
  }
  if(!(is.factor(df_sig[,1]) & is.factor(df_sig[,2]) & are_numeric)){

    # There is an error of some kind in the file format.
    err_code <- 2
    err_message <- "The fatty acid signature data are not properly formatted."

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }



  # Compare two sets of fatty acid names ---------------------------------------

  # Check whether the two sets of fatty acid names have the same length.
  if(length(fa_names) != (ncol(df_sig)-2)){
    err_code <- 3
    err_message <- paste("The number of fatty acid names in the argument fa",
                         "differs from the number in the data frame!",
                         sep = " ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }


  # Check whether the two sets of fatty acid names are identical.

  if(!as.logical(min(fa_names == colnames(df_sig)[-(1:2)]))){
    err_code <- 4
    err_message <- paste("The fatty acid names in the argument fa do not match",
                         "those in the data frame, or their order differs!",
                         sep = " ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }


  # Check that all fatty acids are not being used with augmentation.
  if((length(use_fa) == sum(use_fa)) & scale == 3){
    err_code <- 5
    err_message <- paste("Augmentation is not possible if all fatty acids",
                         "are to be used in the analysis!", sep = " ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }


  # Process the fatty acid signature data --------------------------------------

  # Sort the signature data by type and sample identifier.
  df_sig <- df_sig[order(df_sig[,1], df_sig[,2]),]



  # Determine the total sample size,  the unique types and their number, and the
  # total number of fatty acids.
  n_sig <- nrow(df_sig)
  uniq_types <- unique(df_sig[,1])
  n_types <- length(uniq_types)
  n_fa_rep <- ncol(df_sig) - 2



  # Find the location of the data for each type to speed subsequent computations.
  loc <- matrix(0, nrow=n_types, ncol=2)
  type_ss <- table(df_sig[,1])
  loc[1,1] <- 1
  loc[1,2] <- type_ss[1]
  if(n_types > 1){
    for(li1 in 2:n_types){
      loc[li1,1] <- loc[li1-1,2] + 1
      loc[li1,2] <- loc[li1-1,2] + type_ss[li1]
    }
  }
  rownames(loc) <- uniq_types



  # Isolate fatty acid data, removing type and sample identifier, and
  # transposing to column-major format.
  sig <- t(as.matrix(df_sig[,3:(n_fa_rep+2)]))
  colnames(sig) <- paste(df_sig[,1], trimws(df_sig[,2]), sep=" - ")



  # Replace signature proportions missing or equal to zero.
  temp <- sig_rep_zero(sig, zero_rep)



  # Check for errors while replacing zeros.
  if(temp$err_code != 0){
    err_code <- 6
    err_message <- paste("Error replacing signature proportions of zero.",
                         temp$err_message, sep="; ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }



  # Save the original signatures, with proportions that were missing or equal to
  # zero replaced by a small constant.
  sig_rep <- temp$sig_adj



  # Save the value used to replace zeros.
  zero_rep_val <- temp$rep_val



  # Scale the prey signatures.
  temp <- sig_scale(sig_rep, use_fa, scale)



  # Check for errors while scaling signatures.
  if(temp$err_code != 0){
    err_code <- 7
    err_message <- paste("Error scaling signature proportions.",
                         temp$err_message, sep="; ")

    return(list(type = type,
                id = id,
                n_types = n_types,
                uniq_types = uniq_types,
                n_sig = n_sig,
                type_ss = type_ss,
                loc = loc,
                sig_rep = sig_rep,
                n_fa_rep = n_fa_rep,
                sig_scale = sig_scale,
                n_fa_suite = n_fa_suite,
                fa_suites = fa_suite,
                zero_rep_val = zero_rep_val,
                err_code = err_code,
                err_message = err_message))
  }



  # Return ---------------------------------------------------------------------

  type <- df_sig[,1]
  id <- trimws(df_sig[,2])

  sig_scale <- temp$sig
  n_fa_suite <- temp$n_fa
  fa_suite <- rownames(sig_scale)

  err_code <- 0
  err_message <- "Success!"

  return(list(type = type,
              id = id,
              n_types = n_types,
              uniq_types = uniq_types,
              n_sig = n_sig,
              type_ss = type_ss,
              loc = loc,
              sig_rep = sig_rep,
              n_fa_rep = n_fa_rep,
              sig_scale = sig_scale,
              n_fa_suite = n_fa_suite,
              fa_suites = fa_suite,
              zero_rep_val = zero_rep_val,
              err_code = err_code,
              err_message = err_message))
}

