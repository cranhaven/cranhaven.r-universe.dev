#' Tesseract OCR
#'
#' Extract text from an image. Requires that you have training data for the language you
#' are reading. Works best for images with high contrast, little noise and horizontal text.
#' See [tesseract wiki](https://github.com/tesseract-ocr/tessdoc) and
#' the package vignette for image preprocessing tips.
#'
#' The `ocr()` function returns plain text by default, or hOCR text if hOCR is set to `TRUE`.
#' The `ocr_data()` function returns a data frame with a confidence rate and bounding box for
#' each word in the text.
#'
#' @export
#' @return character vector of text extracted from the file. If the file
#'  is has TIFF or PDF extension, it will be a vector of length equal to the
#'  number of pages.
#' @family tesseract
#' @param file file path or raw vector (png, tiff, jpeg, etc).
#' @param engine a tesseract engine created with [tesseract()]. Alternatively a
#' language string which will be passed to [tesseract()].
#' @param HOCR if `TRUE` return results as HOCR xml instead of plain text
#' @param opw owner password to open pdf (please pass it as an environment
#'  variable to avoid leaking sensitive information)
#' @param upw user password to open pdf (please pass it as an environment
#'  variable to avoid leaking sensitive information)
#' @rdname ocr
#' @references [Tesseract: Improving Quality](https://github.com/tesseract-ocr/tesseract/wiki/ImproveQuality)
#' @examples
#' file <- system.file("examples", "test.png", package = "cpp11tesseract")
#' text <- ocr(file)
#' cat(text)
ocr <- function(file, engine = tesseract("eng"), HOCR = FALSE, opw = "", upw = "") {
  if (is.character(engine)) {
    engine <- tesseract(engine)
  }
  stopifnot(inherits(engine, "externalptr"))
  if (isTRUE(is.character(file))) {
    vapply(file, ocr_file, character(1), ptr = engine, HOCR = HOCR, USE.NAMES = FALSE)
  } else if (isTRUE(is.raw(file))) {
    ocr_raw(file, engine, HOCR = HOCR)
  } else {
    stop("Argument 'file' must be file-path, url or raw vector")
  }
}

#' @rdname ocr
#' @export
ocr_data <- function(file, engine = tesseract("eng")) {
  if (is.character(engine)) {
    engine <- tesseract(engine)
  }
  stopifnot(inherits(engine, "externalptr"))
  df_list <- if (is.character(file)) {
    lapply(file, function(im) {
      ocr_file_data(im, ptr = engine)
    })
  } else if (is.raw(file)) {
    list(ocr_raw_data(file, engine))
  } else {
    stop("Argument 'file' must be file-path, url or raw vector")
  }
  df_as_tibble(do.call(rbind.data.frame, unname(df_list)))
}
