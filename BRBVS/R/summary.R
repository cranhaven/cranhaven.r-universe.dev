#' Summary for BRBVS Object
#'
#' Provides a concise overview of a Bivariate Variable  Selection (BRBVS) object.
#' It reports the maximum number of relevant covariates (`kmax`) across all sets, and details
#' the relevant covariates for each survival function along with their frequency of selection.
#'
#' @param object An object of class `BRBVS`, typically the result of a BRBVS analysis.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return This function does not return a value but prints the summary of the BRBVS object to the console.
#' If the `BRBVS` object is invalid or incomplete, an error message is displayed and the function returns `NULL`.
#'
#' @export
#'
#' @examples
#'############################
#'# Toy Example
#'############################
#'BRBVS_object <- list(
#'mtx.act1E = c("z2", "z1", "0", "0"),
#'scores1E = c(20, 1, 1),
#'freq.rel1E = c(1, 1, 1, 1),
#'mtx.act2E = c("z2", "z1", "0", "0"),
#'scores2E = c(20, 1, 1),
#'freq.rel2E = c(1, 1, 1, 1),
#'metric = "FIM",
#'kmax = 4,
#'copula = "C0",
#'margins = c("PO", "PO"),
#'tau = 0.5,
#'Namecondings = matrix(
#'  c("z1", "z1", "z2", "z2", "z3", "z3", "z4", "z4", "z5", "z5", "z6", "z6"),
#'  nrow = 6, ncol = 2, byrow = TRUE,
#'  dimnames = list(NULL, c("Features", "Code"))
#')
#')
#'
#'class(BRBVS_object) <- "BRBVS"
#'summary(BRBVS_object)
#'
summary.BRBVS <- function(object, ...) {
  if (is.null(object$mtx.act1E) || is.null(object$mtx.act2E)) {
    cat("Error: Invalid or incomplete 'BRBVS' object.\n")
    return()
  }


  cat("Sets of Relevant Covariates\n")
  cat("================================\n\n")
  cat("Metric:",object$metric,"\n")
  cat("kmax:", object$kmax, "\n")
  cat("Copula:", object$copula, "\n")
  cat("Margins:", object$margins, "\n\n")
  cat("================================\n\n")

  feature_names_act1E <- sapply(object$mtx.act1E, lookup_feature_name, object$Namecondings)
  feature_names_act2E <- sapply(object$mtx.act2E, lookup_feature_name, object$Namecondings)

  object$mtx.act1E<- unname(feature_names_act1E)
  object$mtx.act2E<- unname(feature_names_act2E)

  for (i in 1:2) {
    cat("Survival Function ", i, ":\n")
    act <- object[[paste0("mtx.act", i, "E")]]
    freq <- object[[paste0("freq.rel", i, "E")]]

    non_zero_positions <- which(act != "0")
    if (length(non_zero_positions) > 0) {
      # Determine the maximum width of the covariate names
      max_name_width <- max(sapply(act[non_zero_positions], nchar))

      for (pos in seq_along(non_zero_positions)) {
        j <- non_zero_positions[pos]
        formatted_position <- sprintf("%2d%s", pos, get_ordinal_suffix(pos))
        # Format the line with fixed-width fields
        cat(sprintf("  - %s: %-*s (%6.2f%%)\n", formatted_position, max_name_width, act[j], freq[j] * 100))
      }
    } else {
      cat("  No relevant covariates selected.\n")
    }
    cat("\n")
  }
}
