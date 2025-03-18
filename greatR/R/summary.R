#' Summarise registration results
#'
#' @param object Registration results, output of the [register()] registration process.
#' @param ... Arguments to be passed to methods (ignored).
#'
#' @return This function returns a list containing:
#'
#' \item{summary}{table containing the summary of the registration results.}
#' \item{registered_genes}{vector of gene accessions which were successfully registered.}
#' \item{non_registered_genes}{vector of non-registered gene accessions.}
#' \item{reg_params}{table containing distribution of registration parameters.}
#'
#' @name summary
#' @export
summary.res_greatR <- function(object, ...) {
  # Suppress "no visible binding for global variable" note
  gene_id <- NULL
  registered <- NULL

  # Summarise results
  data <- object$model_comparison

  total <- nrow(data)
  reg <- sum(data$registered)
  non_reg <- total - reg

  stretches_list <- unique(data[data$registered, round(stretch, 2)])
  shifts_list <- unique(data[data$registered, round(shift, 2)])
  if (length(stretches_list) > 0) {
    stretch <- range(stretches_list)
    stretch_range <- paste0("[", stretch[1], ", ", stretch[2], "]")
  } else {
    stretch_range <- "NA"
  }
  if (length(shifts_list) > 0) {
    shift <- range(shifts_list)
    shift_range <- paste0("[", shift[1], ", ", shift[2], "]")
  } else {
    shift_range <- "NA"
  }

  # Create summary table
  df_summary <- data.table::data.table(
    Result = c("Total genes", "Registered genes", "Non-registered genes", "Stretch", "Shift"),
    Value = c(total, reg, non_reg, stretch_range, shift_range)
  )

  # List of registered and non-registered genes
  registered_genes <- unique(data[data$registered, gene_id])
  non_registered_genes <- unique(data[!data$registered, gene_id])

  # Results object
  results_list <- list(
    summary = df_summary,
    registered_genes = registered_genes,
    non_registered_genes = non_registered_genes,
    reg_params = data[, .(gene_id, stretch, shift, registered)]
  )

  class(results_list) <- "summary.res_greatR"

  return(results_list)
}

#' @export
print.summary.res_greatR <- function(x, ...) {
  print(x$summary)
  return(invisible(x))
}
