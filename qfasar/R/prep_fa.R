#' Prepare fatty acid information analysis
#'
#' The function \code{prep_fa} processes the information in a fatty acid
#' suites data frame and prepares that information for application to fatty
#' acid signatures.
#'
#' @param df_fa A data frame containing fatty acid names, calibration
#'   coefficients, and 0/1 definitions of fatty acid suites.  \code{qfasar} has
#'   strict formatting requirements for \code{df_fa}; please see Details and/or
#'   the vignette.
#'
#' @return A list containing the following elements: \describe{
#'   \item{cc}{A numeric vector of calibration coefficients.}
#'   \item{use}{A logical vector defining a fatty acid suite.}
#'   \item{fa_names}{A character vector of fatty acid names.}
#'   \item{err_code}{An integer error code (0 if no error is detected).}
#'   \item{err_message}{A string contains a brief summary of the execution.}
#'   }
#'
#'
#' @section Details:
#' This function is designed to be called by the user after the fatty acid data
#' frame has been read.  The data frame should contain a complete list of all
#' fatty acids in the prey and predator signature data, one or more sets of
#' calibration coefficients with an indicator of which set to use, one or more
#' fatty acid suite definitions with an indicator of which suite to use, and
#' optional comments.  Please refer to the vignette for additional information.
#'
#' The fatty acid data frame must strictly meet the following formatting
#' requirements.
#'   \itemize{
#'   \item The first row must contain a header for each column.
#'   \item The second row must list "use_me" in the first column, a 1 in the
#'     column for the set of calibration coefficients to be used, a 1 in the
#'     column for the fatty acid suite to be used, and a 0 in all other columns.
#'   \item Starting with row three, the first column must contain fatty acid
#'     names, which must exactly match the corresponding components of the
#'     headers in any prey and predator signature data frames.
#'   \item Starting with row three, Columns 2 to k must contain calibration
#'     coefficients for each fatty acid.  Multiple sets of calibration
#'     coefficients can be in the data frame.  The set to be used must contain
#'     a 1 in Row 1 and the others must contain a 0 in Row 1.
#'   \item Columns k+1 to m must contain one or more definitions of fatty acid
#'     suites. Membership in a suite is defined by 0/1 indicators, with a 1
#'     indicating membership.  Definitions for multiple suites can be in the
#'     data frame.  For example, two columns could contain indicators defining
#'     membership in the dietary and extended-dietary suites of fatty acids
#'     (Iverson et al. 2004).  The suite to be used must contain a 1 in Row 1
#'     and the others must contain a 0 in Row 1.
#'   \item An optional last column can contain comments.
#'   \item Please see the vignette for examples of how to format this data
#'   frame.
#' }
#'
#' @section References:
#' Iverson, S.J., C. Field, W.D. Bowen, and W. Blanchard. 2004.
#'   Quantitative fatty acid signature analysis: A new method of
#'   estimating predator diets. \emph{Ecological Monographs} 74:211-235.
#'
#' @examples
#' prep_fa(data.frame(fa = c("use_me", "fa_1", "fa_2", "fa_3"),
#'                    cc = c(1, 0.75, 1.25, 1.0),
#'                    use = c(1, 1, 1, 1)))
#'
#' prep_fa(data.frame(fa = c("use_me", "fa_1", "fa_2", "fa_3"),
#'                    cc1 = c(0, 0.75, 1.25, 1.00),
#'                    cc2 = c(1, 1.2, 0.8, 0.9),
#'                    use_1 = c(0, 1, 1, 0),
#'                    use_2 = c(1, 1, 1, 0)))
#'
#' @export
#'
################################################################################


prep_fa <- function(df_fa){


  # Initialize return values and validate inputs -------------------------------

  # Initialize returned values.
  cc <- NA
  use <- NA
  fa_names <- NA



  # Check that the fatty acid data frame is valid.
  if(!(is.data.frame(df_fa) | is.list(df_fa))){
    err_code <- 1
    err_message <- "The fatty acid data are not in a valid data frame."

    return(list(cc = cc,
                use = use,
                fa_names = fa_names,
                err_code = err_code,
                err_message = err_message))
  }



  # Check contents of the fatty acid suites data -------------------------------

  # Identify columns of the fatty acid suites data to be used.
  to_use <- df_fa[1,] == 1



  # Check that only two columns are selected for use.
  if(sum(to_use) != 2){
    err_code <- 2
    err_message <- paste("One and only one set of calibration coefficients and",
                         "fatty acid suites must be selected.", sep=" ")

    return(list(cc = cc,
                use = use,
                fa_names = fa_names,
                err_code = err_code,
                err_message = err_message))
  }



  # Identify the calibration coefficients and use indicators to use.
  cc_to_use <- max.col(to_use, "first")
  fa_to_use <- max.col(to_use, "last")



  # Store the set of calibration coefficients and the fatty acid suite to be
  # used, dropping the use indicators.
  cc <- df_fa[,cc_to_use][-1]
  use <- as.logical(df_fa[,fa_to_use][-1])
  fa_names <- as.character(df_fa[,1][-1])



  # Check that the number of fatty acids to use exceeds 1.
  if(sum(use) < 2){
    err_code <- 3
    err_message <- "The number of fatty acids to use must exceed 1."

    return(list(cc = cc,
                use = use,
                fa_names = fa_names,
                err_code = err_code,
                err_message = err_message))
  }



  # Return ---------------------------------------------------------------------
  err_code <- 0
  err_message <- "Success!"

  return(list(cc = cc,
              use = use,
              fa_names = fa_names,
              err_code = err_code,
              err_message = err_message))
}

