#######################################

# LAPOP Rescale #

#######################################

#' LAPOP Rescale
#'
#' This function allows users to rescale and reorder variables.  It is designed
#' for variables of class "labelled" but the rescaling will work for numeric
#' and factor variables too.
#'
#' @param var Vector (class "labelled" or "haven_labelled").  The original variable
#' to rescale.
#' @param min Integer. Minimum value for the new rescaled variables; default is 0.
#' @param max Integer. Maximum value for the new rescaled variables; default is 1.
#' @param reverse Logical.  Reverse code the variable before rescaling. Default: FALSE.
#' @param only_reverse Logical.  Reverse code the variable, but do not rescale. Default: FALSE.
#' @param only_flip Logical. Flip the variable coding.  Unlike "only_reverse", this will
#' exactly preserve the values of the old variable.  For example, for a variable
#' with codes 1, 2, 3, 5, 10, only_flip will code the values 10, 5, 3, 2, 1 (instead
#' of 10, 9, 8, 6, 1).  Generally, reverse should be preferred to preserve the
#' underlying scale.  Not compatible with rescale. Default: FALSE.
#' @param map Logical. If TRUE, will print a cross-tab showing the old variable
#' and the new, recoded variable.  Used to verify the new variable is coded correctly.
#' Default: FALSE.
#' @param new_varlabel Character.  Variable label for the new variable.
#' Default: old variable's label.
#' @param new_vallabels Character vector. Supply custom names for value labels. Default:
#' value labels of old variable.
#' @return The input variable rescaled
#'
#' @examples
#'
#' require(lapop); data(ym23)
#'
#' # Regular data.frame
#' ym23$pn4r <- lpr_resc(ym23$pn4,
#' reverse = TRUE,
#' map = TRUE)
#'
#' # LPR data.frame
#' ym23lpr<-lpr_data(ym23)
#' ym23lpr$variables$pn4r <- lpr_resc(ym23lpr$variables$pn4,
#' reverse = TRUE,
#' map = TRUE)
#'
#'@export
#'@import haven
#'
#'@author Luke Plutowski, \email{luke.plutowski@@vanderbilt.edu} & Robert Vidigal, \email{robert.vidigal@@vanderbilt.edu}

lpr_resc <- function(var,
                     min = 0L,
                     max = 1L,
                     reverse = FALSE,
                     only_reverse = FALSE,
                     only_flip = FALSE,
                     map = FALSE,
                     new_varlabel = NULL,
                     new_vallabels = NULL) {

  og_var <- var

  if (is.character(var)) {
    stop("Cannot rescale character variable.")
  }

  # Extract labels if haven_labelled
  if (inherits(var, c("haven_labelled", "labelled"))) {
    original_labels <- attr(haven::zap_missing(var), "labels")
  }

  # Convert factor to numeric
  if (is.factor(var)) {
    original_levels <- levels(var)
    var <- as.numeric(var)
  }

  # Internal reverse helper
  reverse_values <- function(x) {
    if (all(is.na(x))) return(x)
    rng <- range(x, na.rm = TRUE)
    reversed <- (rng[2] + rng[1]) - x
    return(reversed)
  }

  # Reverse logic
  if (reverse || only_reverse) {
    var <- reverse_values(var)
  } else if (only_flip) {
    unique_vals <- sort(unique(var))
    flipped_vals <- rev(unique_vals)
    var <- flipped_vals[match(var, unique_vals)]

    # Flip labels if available
    if (exists("original_labels")) {
      flipped_labels <- rev(original_labels)
      names(flipped_labels) <- names(original_labels)
      attr(var, "labels") <- flipped_labels
    }
  } else {
    # Rescale to [min, max]
    var <- (var - min(var, na.rm = TRUE)) /
      (max(var, na.rm = TRUE) - min(var, na.rm = TRUE)) *
      (max - min) + min
  }

  # Optional mapping table
  if (map) {
    print(table(haven::as_factor(haven::zap_missing(og_var)),
                haven::as_factor(haven::zap_missing(var))))
  }

  # Update variable label
  if (!is.null(new_varlabel)) {
    attr(var, "label") <- as.character(new_varlabel)
  }

  # Update value labels
  if (!is.null(new_vallabels) && exists("original_labels")) {
    names(original_labels) <- new_vallabels
    attr(var, "labels") <- original_labels
  }

  return(var)
}
