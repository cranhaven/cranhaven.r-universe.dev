# Scientific Plot Export
#
# Dimension-aware graphics export pipeline saving ggplot2 and autoplot objects
# to raster and vector formats with journal-calibrated canvas aspect ratios.

#########
# GRAPHICS EXPORT PIPELINE
# Dimension resolution, transactional file writing, and device format dispatch.

#' Attach recommended export dimensions to a plot
#' @keywords internal
#' @noRd
.with_export_dim <- function(plot, width, height, units = "in") {
  attr(plot, "simtab_export_dim") <- list(
    width = as.numeric(width),
    height = as.numeric(height),
    units = units
  )
  plot
}

#' Export a SimtablR plot to an image file
#'
#' Writes a plot through `ggplot2::ggsave()`. When `width` and `height` are not
#' supplied, the plot's own recommended dimensions are used; an explicit value
#' always overrides the recommendation.
#'
#' @param x A `ggplot` object, or a SimtablR object with an `autoplot()` method.
#' @param path Output file path; the extension selects the device. When absent,
#'   `.png` is added. Supported suffixes are `.png`, `.pdf`, `.svg`, `.jpeg`,
#'   `.jpg`, `.tiff`, `.tif`, `.bmp`, `.eps`, `.ps`, `.tex`, `.wmf`, and `.emf`;
#'   device availability still depends on the platform.
#' @param width,height Canvas size in inches. Default `NULL`, meaning the plot's
#'   recommended dimensions.
#' @param dpi Resolution for raster devices. Default `300`.
#' @param overwrite Logical. Existing files are protected by default; pass
#'   `TRUE` to replace the destination explicitly.
#' @param ... Passed to `ggplot2::ggsave()`.
#' @return Invisibly returns the normalized output path.
#' @seealso [export_docx()], [export_pptx()]
#' @examples
#' \dontrun{
#' data(epitabl)
#' roc(epitabl, poc_hstn_value, adjudicated_acs) |>
#'   export_plot(tempfile(fileext = ".png"))
#' }
#' @export
export_plot <- function(x, path, width = NULL, height = NULL, dpi = 300, overwrite = FALSE, ...) {
  target <- .prepare_export_path(
    path,
    c("png", "pdf", "svg", "jpeg", "jpg", "tiff", "tif", "bmp", "eps", "ps", "tex", "wmf", "emf"),
    default_extension = "png",
    overwrite = overwrite
  )
  .require_export_pkg("ggplot2")
  plot <- .as_exportable_plot(x)
  dim <- .resolve_export_dim(plot, width, height)

  .write_export_transaction(target, function(temporary) {
    ggplot2::ggsave(
      filename = temporary,
      plot = plot,
      width = dim$width,
      height = dim$height,
      units = dim$units,
      dpi = dpi,
      ...
    )
  })
  message(sprintf("Plot exported to: %s", target$path))
  invisible(target$path)
}

#' Coerce an object into a plot for export
#' @keywords internal
#' @noRd
.as_exportable_plot <- function(x) {
  if (inherits(x, "ggplot")) {
    return(x)
  }
  if (inherits(x, c("simtab_result", "simtab_spec", "simtab_report"))) {
    return(ggplot2::autoplot(x))
  }
  simtab_abort_input(c(
    "{.arg x} must be a {.cls ggplot} or a SimtablR object with an {.fn autoplot} method.",
    "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
    "v" = "Pass {.code autoplot(result)}, or a result that supports plotting."
  ))
}

#' Resolve export canvas dimensions from attributes or explicit overrides
#' @keywords internal
#' @noRd
.resolve_export_dim <- function(plot, width = NULL, height = NULL, default = list(width = 7, height = 5, units = "in")) {
  rec <- attr(plot, "simtab_export_dim", exact = TRUE) %||% default
  list(
    width = width %||% rec$width %||% default$width,
    height = height %||% rec$height %||% default$height,
    units = rec$units %||% "in"
  )
}
