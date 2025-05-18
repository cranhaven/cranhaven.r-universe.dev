#' @title Transforming an xml file with the help of an xsl style file
#' @description Using an xslt transformation, an xml input file is converted to
#' an other file format (text, xml). The output file name may be given or
#' calculated.
#' In the latest case, the extension is defined after getting xsl method from
#' the xsl file (xml, txt).
#' @param xml_file File path of the input xml file
#' @param style_file File path of the xsl file
#' @param out_file File path of the generated file
#'
#' @return Path of the generated file
#'
#' @examples
#' \dontrun{
#' xml_plt <- file.path(get_examples_path(file_type = "xml"), "file_plt.xml")
#' xsl_file <-
#'    "/path/to/JavaSTICS/folder/bin/resources/xml/stylesheet/xml2txt.xsl"
#'
#' convert_xml2txt_int(xml_file = xml_plt, style_file = xsl_file)
#' }
#'
#' @keywords internal
#'
#' @noRd
#'
convert_xml2txt_int <- function(xml_file, style_file, out_file = NULL) {

  f_names <- c(xml_file, style_file)
  ex_files <- file.exists(f_names)
  if (any(!ex_files)) {
    warning("At least one input file doesn't exist ! \n",
            paste(f_names[!ex_files], collapse = ", ")
    )
    return(FALSE)
  }

  # checking files extensions
  names_split <- lapply(f_names, function(x) {
    unlist(strsplit(x, ".",
                    fixed = TRUE
    ))
  })

  files_ext <- unlist(lapply(names_split, function(x) x[length(x)]))
  # any not matching expected extensions
  if (!all(files_ext %in% c("xml", "xsl"))) {
    warning("Check files extensions: .xml and .xsl needed !")
    return(FALSE)
  }

  # converting file
  style <- xml2::read_xml(style_file)
  doc <- xml2::read_xml(xml_file)
  txt <- xslt::xml_xslt(doc, style)
  out <- substr(txt, 1, nchar(txt) - 1)

  # getting xsl output method for setting file extension
  method <- xml2::xml_attr(xml2::xml_children(style), "method")[1]
  ext <- "xml"
  if (method == "text") {
    ext <- "txt"
  }

  # setting output file name if needed
  if (base::is.null(out_file)) {
    out_file <- file.path(
      dirname(xml_file),
      paste0(unlist(strsplit(basename(xml_file), ".",
                             fixed = TRUE
      ))[1], ".", ext)
    )
  }

  # writing output file
  con <- file(out_file, "w")
  writeLines(out, con, sep = "")
  close(con)

  return(invisible(TRUE))
}
