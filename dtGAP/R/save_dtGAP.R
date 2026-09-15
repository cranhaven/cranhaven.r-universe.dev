#' Save dtGAP Visualization to File
#'
#' Exports the dtGAP plot to PNG, PDF, or SVG format.
#'
#' @param file Character. Output file path. The format is inferred from the
#'   file extension unless \code{format} is specified explicitly.
#' @param format Character or NULL. One of \code{"png"}, \code{"pdf"}, or
#'   \code{"svg"}. If NULL (default), inferred from \code{file} extension.
#' @param width Numeric. Page width in mm (default 297, A4 landscape).
#' @param height Numeric. Page height in mm (default 210, A4 landscape).
#' @param dpi Numeric. Resolution for PNG output (default 300). Ignored for
#'   PDF and SVG.
#' @param bg Character. Background color (default \code{"white"}).
#' @param ... Additional arguments passed to \code{\link{dtGAP}()}.
#'
#' @return Invisible file path of the created file.
#'
#' @export
#'
#' @examples
#' \donttest{
#' save_dtGAP(
#'   file = tempfile(fileext = ".png"),
#'   data_train = train_covid,
#'   data_test = test_covid,
#'   target_lab = "Outcome",
#'   show = "test",
#'   print_eval = FALSE
#' )
#' }
save_dtGAP <- function(file,
                       format = NULL,
                       width = 297,
                       height = 210,
                       dpi = 300,
                       bg = "white",
                       ...) {
  supported <- c("png", "pdf", "svg")

  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    if (ext == "") {
      stop("Cannot infer format: '", file,
           "' has no extension. Supply `format` explicitly.")
    }
    format <- ext
  }
  format <- tolower(format)
  if (!format %in% supported) {
    stop("Unsupported format '", format, "'. Use one of: ",
         paste(supported, collapse = ", "), ".")
  }

  out_dir <- dirname(file)
  if (!dir.exists(out_dir)) {
    stop("Directory does not exist: ", out_dir)
  }

  w_in <- width / 25.4
  h_in <- height / 25.4

  switch(format,
    png = png(file, width = w_in, height = h_in, units = "in",
              res = dpi, bg = bg),
    pdf = pdf(file, width = w_in, height = h_in, bg = bg),
    svg = svg(file, width = w_in, height = h_in)
  )
  on.exit(dev.off(), add = TRUE)

  dtGAP(total_w = width, total_h = height, ...)

  invisible(file)
}
