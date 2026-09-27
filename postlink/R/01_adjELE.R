#' Secondary Analysis Constructor Assuming ELE
#'
#' Specifies the linked data and information on the underlying record linkage
#' process for regression of linked data assuming exchangeable linkage errors
#' (ELE) as developed by Chambers (2009) and Vo et al. (2024). These approaches
#' correct for bias from mismatch error via weighting matrices estimated using
#' known mismatch rates or clerical reviews (audit samples).
#'
#' @param linked.data A data.frame containing the data after record linkage.
#' @param m.rate Numeric vector; known or estimated probability of mismatch for
#' each record or block. Values must be between 0 and 1. Can be a single global rate,
#' a vector of length equal to the number of unique blocks, or a vector of
#' length equal to the number of rows in \code{linked.data}.
#' @param audit.size Numeric vector; If the m.rate is estimated, provide sample
#' sizes for the clerical review audit. Used for variance estimation.
#' If provided, must align with \code{blocks} similar to \code{m.rate}.
#' Defaults to \code{NULL} (assume m.rate is known).
#' @param blocks A vector or an unquoted variable name found in \code{linked.data}
#' identifying the blocking structure used during linkage. If \code{NULL} (default),
#' all records are assumed to belong to a single block.
#' @param weight.matrix Character; the method for estimating the weight matrix.
#' Must be one of "ratio" (default), "LL", "BLUE", or "all".
#'
#' @return An object of class \code{c("adjELE", "adjustment")}. To minimize
#' memory overhead, the underlying \code{linked.data} is stored by reference
#' within an environment inside this object.
#'
#' @details
#' The constructor validates consistency between the mismatch rates, audit sizes,
#' and block identifiers. If \code{blocks} are provided, \code{m.rate} must
#' be specified either per-block (length equals number of unique blocks) or
#' per-record (length equals number of rows).
#'
#' Explicit provision of \code{linked.data} is strongly recommended for
#' reproducibility and to ensure the adjustment object fully encapsulates
#' the necessary data for downstream model fitting.
#'
#' @examples
#' data(brfss, package = "postlink")
#'
#' adj_object <- adjELE(linked.data = brfss,
#'                     m.rate = unique(brfss$m.rate),
#'                     blocks = imonth,
#'                     weight.matrix = "BLUE")
#'
#' @seealso
#' * [plglm()] for generalized linear regression modeling
#' * [plcoxph()] for Cox proportional hazards regression modeling
#'
#' @references
#' Chambers, R. (2009). Regression analysis of probability-linked data.
#' \emph{Official Statistics Research Series}, 4, 1-15.
#'
#' Chambers, R. L., Fabrizi, E., Ranalli, M. G., Salvati, N., & Wang, S. (2023).
#' Robust regression using probabilistically linked data. \emph{Wiley Interdisciplinary
#' Reviews: Computational Statistics}, 15(2), e1596. \doi{10.1002/wics.1596}
#'
#' Vo, T. H., Garès, V., Zhang, L. C., Happe, A., Oger, E.,
#' Paquelet, S., & Chauvet, G. (2024). Cox regression with linked data.
#' \emph{Statistics in Medicine}, 43(2), 296-314. \doi{10.1002/sim.9960}
#'
#' @note
#' The internal algorithmic structure for the estimating equations was informed by
#' the foundational work presented in Chambers (2009) and Vo et al. (2024).
#' Additionally, the original code provided in the Appendix of Chambers (2009)
#' was utilized as a benchmark oracle during the unit testing phase of package
#' development to check for numerical accuracy and validity of the implementation.
#'
#' @export
adjELE <- function(linked.data,
                   m.rate,
                   audit.size = NULL,
                   blocks = NULL,
                   weight.matrix = c("ratio", "LL", "BLUE", "all")) {

 # 1. Validate linked.data
 # Check if object is coercible to data.frame, but keep it robust
 if (!missing(linked.data) && !is.null(linked.data)) {
  if (is.environment(linked.data) || is.list(linked.data)) {
   linked.data <- tryCatch(as.data.frame(linked.data), error = function(e) {
    stop("'linked.data' must be a data.frame or coercible to one.", call. = FALSE)
   })
  } else if (!is.data.frame(linked.data)) {
   stop("'linked.data' must be a data.frame, list, or environment.", call. = FALSE)
  }
 } else {
  stop("'linked.data' must be provided for adjELE.", call. = FALSE)
 }

 n_obs <- nrow(linked.data)

 # 2. Resolve 'blocks' (NSE vs Standard Evaluation)
 blocks_eval <- NULL
 blocks_expr <- substitute(blocks)

 if (!is.null(blocks_expr)) {
  # Attempt 1: Look inside linked.data (NSE)
  blocks_eval <- tryCatch({
   eval(blocks_expr, linked.data, enclos = parent.frame())
  }, error = function(e) NULL)

  # Attempt 2: Look in parent environment
  if (is.null(blocks_eval)) {
   blocks_eval <- tryCatch({
    eval(blocks_expr, envir = parent.frame())
   }, error = function(e) {
    # If both fail, blocks might be NULL explicitly, which is allowed.
    # But if substitute wasn't NULL, the user tried to pass something.
    NULL
   })
  }
 }

 # If blocks is strictly NULL, create a default "all 1s" block vector
 if (is.null(blocks_eval)) {
  blocks_eval <- rep(1, n_obs)
 }

 # Validate blocks length
 if (length(blocks_eval) != n_obs) {
  stop("Length of 'blocks' must equal the number of rows in 'linked.data'.", call. = FALSE)
 }

 # Identify unique blocks (excluding NA if any, though blocks shouldn't usually have NA)
 unique_blocks <- unique(blocks_eval[!is.na(blocks_eval)])
 n_unique_blocks <- length(unique_blocks)

 # 3. Validate m.rate
 if (missing(m.rate) || is.null(m.rate)) {
  stop("'m.rate' must be specified.", call. = FALSE)
 }
 if (!is.numeric(m.rate)) {
  stop("'m.rate' must be a numeric vector.", call. = FALSE)
 }
 if (any(is.na(m.rate))) {
  stop("'m.rate' cannot contain missing values.", call. = FALSE)
 }
 if (any(m.rate < 0 | m.rate > 1)) {
  stop("All values in 'm.rate' must be between 0 and 1 (inclusive).", call. = FALSE)
 }

 # Check m.rate dimensions against blocks
 len_rate <- length(m.rate)

 # Case A: Rate specified per block
 if (len_rate == n_unique_blocks) {
  # Valid. No further check needed here, mapping happens downstream.
 }
 # Case B: Rate specified per record (full length)
 else if (len_rate == n_obs) {
  # Consistency Check:
  # If m.rate is per-record, every record in the same block must have the same rate.
  # We could check by aggregating variance or checking unique values per block.
  # Using a fast check:
  rate_consistency <- tapply(m.rate, blocks_eval, function(x) length(unique(x)) == 1)
  if (!all(rate_consistency, na.rm = TRUE)) {
   stop("When 'm.rate' is specified per record, values must be constant within each block.", call. = FALSE)
  }
 }
 # Case C: Global rate (len=1) but multiple blocks
 else if (len_rate == 1) {
  # Valid. It applies to all blocks.
 } else {
  stop(paste0("Length of 'm.rate' (", len_rate, ") must match either the number of observations (",
              n_obs, ") or the number of unique blocks (", n_unique_blocks, ")."), call. = FALSE)
 }

 # 4. Validate audit.size (Optional)
 if (!is.null(audit.size)) {
  if (!is.numeric(audit.size)) {
   stop("'audit.size' must be a numeric vector.", call. = FALSE)
  }

  len_audit <- length(audit.size)

  # Case A: Per block
  if (len_audit == n_unique_blocks) {
   # Valid
  }
  # Case B: Per record
  else if (len_audit == n_obs) {
   # Consistency check per block
   audit_consistency <- tapply(audit.size, blocks_eval, function(x) length(unique(x)) == 1)
   if (!all(audit_consistency, na.rm = TRUE)) {
    stop("When 'audit.size' is specified per record, values must be constant within each block.", call. = FALSE)
   }
  }
  # Case C: Global
  else if (len_audit == 1) {
   # Valid
  } else {
   stop(paste0("Length of 'audit.size' (", len_audit, ") must match either the number of observations (",
               n_obs, ") or the number of unique blocks (", n_unique_blocks, ")."), call. = FALSE)
  }
 }

 # 5. Validate weight.matrix
 # Match argument against allowed choices
 weight.matrix <- match.arg(weight.matrix)

 # 6. Construct S3 Object with Reference Semantics
 data_ref <- new.env(parent = emptyenv())
 data_ref$data <- linked.data

 out <- structure(
  list(
   data_ref = data_ref,
   m.rate = m.rate,
   audit.size = audit.size,
   blocks = blocks_eval,
   weight.matrix = weight.matrix
  ),
  class = c("adjELE", "adjustment")
 )

 return(out)
}
