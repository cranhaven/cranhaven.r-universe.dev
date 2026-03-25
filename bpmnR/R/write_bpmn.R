# ============================== MAIN FUNCTION =================================

#' Write XML or HTML to disk.
#'
#' This writes out both XML and normalised HTML. The default behavior will
#' output the same format which was read. If you want to force output pass
#' `option = "as_xml"` or `option = "as_html"` respectively.
#'
#' @param bpmn A BPMN object as a list of data.frames for the BPMN elements and
#'   an XML document for the XML-based interchange format for the BPMN process.
#' @param file Path to file or connection to write to.
#' @param ... Additional arguments passed to methods.
#'
#' @author Alessio Nigro
#'
#' @importFrom readr write_file
#' @return Writes file to system.
#' @export
write_bpmn <- function(bpmn,
                       file,
                       ...) {
  UseMethod("write_bpmn")
}

#' @describeIn write_bpmn Write bpmn to .bpmn file
#' @export
write_bpmn.bpmn <-
  function(bpmn,
           file,
           ...) {

      write_file(bpmn[["xml"]], file)

      return(message(paste0(
        "Successfully written file to '", file, "'.\n")))

  }
