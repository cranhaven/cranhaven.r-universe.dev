#' Generate abbreviated node labels
#'
#' Internal utility to create compact labels for nodes (observed vs latent).
#'
#' @param x Character vector of variable names.
#' @param latent Logical; if TRUE, use shorter padding/abbreviation for latent vars.
#' @return Character vector of abbreviated and padded labels.
#' @noRd
#' @keywords internal
#' @importFrom stringr str_pad
.generate_node_labels <- function(x, latent = FALSE) {
  if (!is.character(x)) {
    x <- as.character(x)
  }

  if (latent) {
    lab <- abbreviate(x, minlength = 3L, strict = TRUE)
    stringr::str_pad(lab, side = "both", width = 3L)
  } else {
    lab <- abbreviate(x, minlength = 6L, strict = TRUE)
    stringr::str_pad(lab, side = "both", width = 8L)
  }
}
