#' Aerial DAS format requirements
#'
#' Access and save local PDF documents describing the data format of the 
#' different file types supported by \code{swfscAirDAS}
#'
#' @param file character; the name of the file where the PDF will be saved
#' @param file.type character; indicates which data format PDF to extract.
#'   Must be one of: "turtle", "caretta", "survey", or "phocoena" (case sensitive)
#' @param ... passed to \code{\link[base:files]{file.copy}}; 
#'   might included named argument \code{overwrite}
#'
#' @details This function is a wrapper function for \code{\link[base:files]{file.copy}}. 
#'   It saves a PDF document describing the specified aerial DAS data 
#'   format requirements by copying the PDF document to \code{file}
#'   
#'   The PDF files can also be manually copied or downloaded from:
#'   
#'   PHOCOENA
#'   \itemize{
#'     \item Can be copied from: 
#'       \code{system.file("AirDAS_Format_PHOCOENA.pdf", package = "swfscAirDAS")}
#'     \item Can be downloaded from: 
#'       \url{https://github.com/swfsc/swfscAirDAS/blob/master/inst/AirDAS_Format_PHOCOENA.pdf}
#'   }
#'   CARETTA
#'   \itemize{
#'     \item Can be copied from: 
#'       \code{system.file("AirDAS_Format_CARETTA.pdf", package = "swfscAirDAS")}
#'     \item Can be downloaded from: 
#'       \url{https://github.com/swfsc/swfscAirDAS/blob/master/inst/AirDAS_Format_CARETTA.pdf}
#'   }
#'   TURTLE
#'   \itemize{
#'     \item Can be copied from: 
#'       \code{system.file("AirDAS_Format_TURTLE.pdf", package = "swfscAirDAS")}
#'     \item Can be downloaded from: 
#'       \url{https://github.com/swfsc/swfscAirDAS/blob/master/inst/AirDAS_Format_TURTLE.pdf}
#'   }
#'
#' @return output of \code{\link[base:files]{file.copy}}: 
#'   \code{TRUE} if writing of file was successful, and \code{FALSE} otherwise
#'   
#' @seealso \url{https://swfsc.github.io/swfscAirDAS/}
#'
#' @examples
#' if (interactive()) {
#'   airdas_format_pdf(
#'     "AirDAS_Format_TURTLE.pdf", file.type  = "turtle", 
#'     overwrite = FALSE
#'   )
#' }
#'
#' @export
airdas_format_pdf <- function(file, file.type = c("phocoena", "caretta", "turtle"), ...) {
  file.type <- match.arg(file.type)
  
  file.tocopy <- switch(file.type, 
                        phocoena = "AirDAS_Format_PHOCOENA.pdf", 
                        # survey = "AirDAS_Format_SURVEY.pdf", 
                        caretta = "AirDAS_Format_CARETTA.pdf", 
                        turtle = "AirDAS_Format_TURTLE.pdf")
  
  file.copy(system.file(file.tocopy, package = "swfscAirDAS"), to = file, ...)
}
