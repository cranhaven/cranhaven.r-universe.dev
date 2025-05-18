#' @title Transforming a STICS xml file into a text file
#' @description The input file according to his type
#' (ini,plant,tec,station,soil,par)
#' is converted to a text file readable by the STICS model
#' (ficini.txt, ficplt1.txt,...)
#' @param file Path (including name) of the xml file to convert
#' @param plant_id The plant identifier (main crop: 1 ; associated crop: 2)
#' @param out_dir Path of the directory where to generate the file.
#' Optional, set to the path of the input xml file by default
#' @param save_as Name of the output file
#' (optional, default: fixed name for STICS)
#' @param stics_version the STICS files version to use (optional,
#' default to latest).
#' @param xml_file `r lifecycle::badge("deprecated")` `xml_file` is no
#'   longer supported, use `file` instead.
#' @param out_file `r lifecycle::badge("deprecated")` `out_file` is no
#'   longer supported, use `save_as` instead.
#' @param plt_num `r lifecycle::badge("deprecated")` `plt_num` is no
#'   longer supported, use `plant_id` instead.
#'
#' @return None
#'
#' @examples
#'
#' \dontrun{
#' xml_path <- "/path/to/corn_plt.xml"
#' javastics_path <- "/path/to/JavaSTICS/folder"
#' convert_xml2txt(file = xml_path, javastics = javastics_path)
#' }
#'
#' @export
#'
convert_xml2txt <- function(file,
                            plant_id = 1,
                            out_dir = NULL,
                            save_as = NULL,
                            stics_version = "latest",
                            xml_file = lifecycle::deprecated(),
                            plt_num = lifecycle::deprecated(),
                            out_file = lifecycle::deprecated()) {

  if (lifecycle::is_present(xml_file)) {
    lifecycle::deprecate_warn("1.0.0",
                              "convert_xml2txt(xml_file)",
                              "convert_xml2txt(file)")
  } else {
    xml_file <- file # to remove when we update inside the function
  }

  if (lifecycle::is_present(plt_num)) {
    lifecycle::deprecate_warn("1.0.0",
                              "convert_xml2txt(plt_num)",
                              "convert_xml2txt(plant_id)")
  } else {
    plt_num <- plant_id # to remove when we update inside the function
  }
  if (lifecycle::is_present(out_file)) {
    lifecycle::deprecate_warn("1.0.0",
                              "convert_xml2txt(out_file)",
                              "convert_xml2txt(save_as)")
  } else {
    out_file <- save_as # to remove when we update inside the function
  }

  # Defining which xsl file to use according to the input xml file
  xsl_files <- c(
    "ini2txt.xsl", "sol2txt.xsl", "xml2txt.xsl", "xml2txt.xsl",
    "xml2txt.xsl", "xml2txt.xsl", "xml2txt.xsl"
  )
  names(xsl_files) <- c(
    "initialisations", "sols", "fichierplt", "fichiertec",
    "fichiersta", "fichierparamgen", "fichierpar"
  )
  # output text file names
  files_names <- list(
    "ficini.txt", "param.sol", list("ficplt1.txt", "ficplt2.txt"),
    list("fictec1.txt", "fictec2.txt"), "station.txt", "tempoparv6.sti",
    "tempopar.sti"
  )

  # Using tags from in files names for the xml file type identification
  tags <- list("_ini\\.xml", "sols\\.xml", "_plt\\.xml",
               "_tec\\.xml", "_sta\\.xml", "_newform\\.xml", "_gen\\.xml")
  idx <- which(unlist(lapply(tags, function(x) grepl(x, xml_file))))
  calc_name <- length(idx) > 0

  # Not possible to define output file name
  if (base::is.null(out_file) && !calc_name) {
    stop("Output file name not found or not provided as argument! ")
  }

  # Detecting plt or tec in xml file name
  if (calc_name && base::is.null(out_file)) {
    tag <- tags[[idx]]
    if (tag == "_plt\\.xml" || tag == "_tec\\.xml") {
      out_file <- files_names[[idx]][[plt_num]]
    } else {
      out_file <- files_names[[idx]][[1]]
    }
  }

  # Getting the input dir if no output dir in args
  if (base::is.null(out_dir)) {
    out_dir <- dirname(xml_file)
  }

  # Defining output file path
  out_file_path <- file.path(out_dir, out_file)

  # Getting the root element name for identifying the file type
  # TODO: redundancy according to finding tags in files names
  # (see above code, finding idx!)
  doc <- xml2::read_xml(xml_file)
  filet <- xml2::xml_name(doc)

  # Calling get_examples_path
  xsl_dir <- get_examples_path("xsl", stics_version = stics_version)

  style_file <- file.path(xsl_dir, xsl_files[filet])

  # calling the xml conversion function
  status <- convert_xml2txt_int(xml_file, style_file, out_file_path)

  return(status)
}

