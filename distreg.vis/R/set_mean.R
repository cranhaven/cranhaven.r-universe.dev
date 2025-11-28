#' Obtain mean values and reference categories of variables in a data.frame
#'
#' This function purely exists for the \code{set_mean} argument of
#' \code{\link{plot_moments}}. It takes a data.frame and obtains the mean values
#' (numeric variables) and reference categories (categorical covariates).
#'
#' @param input A \code{data.frame} object
#' @param vary_by A character string with the name of a variable over which the output dataframe should vary.
#' @return A \code{data.frame} object with one row
#' @examples
#'
#' library("betareg")
#'
#' # Get some data
#' beta_dat <- model_fam_data(fam_name = "betareg")
#'
#' # Estimate model
#' betamod <- betareg(betareg ~ ., data = beta_dat)
#'
#' # Obtain explanatory variables and set to mean
#' set_mean(model_data(betamod))
#' set_mean(model_data(betamod), vary_by = "binomial1")
#' @export

set_mean <- function(input, vary_by = NULL) {

  if (!is(input, "data.frame"))
    stop("Argument `input` needs to be a data.frame object")

  # Stop if vary_by is not part of df
  if (!is.null(vary_by))
    if (!(vary_by %in% colnames(input)))
      stop("Argument vary_by has to be a variable name that is part of the utilized model dataset")

  # Do the operations
  new_df <- lapply(names(input), FUN = function(x) {

    # Get variable
    var <- input[[x]]

    # If variable is numeric take mean
    if (is.numeric(var)) {
      if (!is.null(vary_by) && x == vary_by) {
        def_var <- quantile(var, c(seq(0.05, 0.95, length.out = 5)), # If we want to vary
                            na.rm = TRUE)
      } else {
        def_var <- mean(var, na.rm = TRUE)
        if (is.integer(var))
          def_var <- round(def_var)
      }
    }

    # If variable is character take the first observation
    if (is.character(var)) {
      if (!is.null(vary_by) && x == vary_by) {
        def_var <- unique(na.omit(var))
      } else {
        def_var <- na.omit(var)[1]
      }
    }

    # If variable is factor take the first level
    if (is.factor(var)) {
      if (!is.null(vary_by) && x == vary_by) {
        def_var <- levels(var)
        levels(def_var) <- levels(var)
      } else {
        def_var <- as.factor(levels(var)[1])
        levels(def_var) <- levels(var)
      }
    }

    # If variable is logical let it be FALSE
    if (is.logical(var)) {
      if (!is.null(vary_by) && x == vary_by) {
        def_var <- c(FALSE, TRUE)
      } else {
        def_var <- FALSE
      }
    }

    # Return it
    return(def_var)
  })
  if (is.null(vary_by)) {
    new_df <- as.data.frame(new_df,
                            row.names = c("default_vals"))
    colnames(new_df) <- colnames(input)
  } else if (!is.null(vary_by)) {
    new_df <- as.data.frame(
      new_df,
      row.names = paste0("default_vals",
                         seq_len(
                           max(sapply(new_df, length))
                         )
      )
    )
    colnames(new_df) <- colnames(input)
  }

  # Return new df
  return(new_df)
}
