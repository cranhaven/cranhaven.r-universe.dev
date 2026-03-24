#' Construct state matrices from longitudinal EHR Data
#'
#'@description Builds a binary matrix (0/1/NA) encoding whether each individual had each long-term
#' condition (LTC) at each time point from 0 to `l`, based on their age of onset. The matrix
#' includes all LTCs, including those used to determine censoring and failure. However, the
#' presence of `fail_code` or `cens_code` still triggers NA values after their onset.
#' @useDynLib MSCA, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @param data A data frame containing one row per condition occurrence.
#' @param id Name of the column identifying individuals.
#' @param ltc Name of the column containing LTC labels.
#' @param aos Name of the column giving age of onset (or time of onset) for each LTC.
#' @param l The maximum time index (inclusive); matrix has `l + 1` time rows per LTC.
#' @param fail_code Label in `ltc` indicating a failure event (e.g., death).
#' @param cens_code Label in `ltc` indicating censoring.
#'
#' @return A matrix with `(l + 1) * number of LTCs` rows and
#' one column per unique individual. Values are 1 after onset, 0 before, and NA after censor/fail.
#' Rows are named `<ltc>_<time>`, and columns are individual IDs.
#' @references Delord M, Douiri A (2025) <doi:10.1186/s12874-025-02476-7>
#' @note For large datasets, computations may be split into multiple jobs to manage memory and performance. Consider reducing the time granularity and/or the number of long-term condition (event of interest) to improve efficiency and stability.
#' @author @author Marc Delord
#' @keywords Censored state matrix
#' @concept Censored state matrix
#' @export
make_state_matrices <- function(data,
                              id = "link_id",
                              ltc = "reg",
                              aos = "aos",
                              l = 111,
                              fail_code = "death",
                              cens_code = "cens") {
  data <- data[!is.na(data[[ltc]]), ]

  ltc_factor <- factor(data[[ltc]])
  id_factor  <- factor(data[[id]])

  id_int  <- as.integer(id_factor)
  ltc_int <- as.integer(ltc_factor)
  aos_num <- as.numeric(data[[aos]])

  fail_val <- match(fail_code, levels(ltc_factor))
  cens_val <- match(cens_code, levels(ltc_factor))

  mat <- make_state_matrix_rcpp(data,
                                id_int,
                                ltc_int,
                                aos_num,
                                l = l,
                                fail_code = fail_val %||% -1,
                                cens_code = cens_val %||% -2)

  rownames(mat) <- as.vector(outer(0:l, levels(ltc_factor), paste, sep = "_"))
  colnames(mat) <- levels(id_factor)

  return(mat)
}




