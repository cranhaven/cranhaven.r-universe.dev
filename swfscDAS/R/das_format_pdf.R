#' DAS format requirements
#'
#' Save the PDF document describing the DAS format required by \code{swfscDAS} to a specified file
#'
#' @param file character, the name of the file where the PDF will be saved
#' @param ... passed on to \code{\link[base:files]{file.copy}}; might included named argument \code{overwrite}
#'
#' @details A wrapper function for \code{\link[base:files]{file.copy}}.
#'   This function saves the PDF document describing the DAS data format requirements by
#'   copying the PDF document located at \code{system.file("DAS_Format.pdf", package = "swfscDAS")}
#'   to \code{file}
#'
#'   This file can also be downloaded from
#'   \url{https://github.com/swfsc/swfscDAS/blob/master/inst/DAS_Format.pdf}
#'
#' @return output of \code{\link[base:files]{file.copy}};
#'   \code{TRUE} if writing of file was successful, and \code{FALSE} otherwise
#'
#' @examples
#' das_format_pdf(file.path(tempdir(), "DAS_Format.pdf"), overwrite = FALSE)
#'
#' @export
das_format_pdf <- function(file, ...) {
  stopifnot(
    inherits(file, "character"),
    length(file) == 1
  )

  file.copy(
    system.file("DAS_Format.pdf", package = "swfscDAS"), to = file, ...
  )
}
