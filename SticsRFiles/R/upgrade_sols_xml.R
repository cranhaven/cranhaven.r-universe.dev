#' Upgrading a sols.xml file to a newer version
#'
#' @param file Path of a sols.xml file
#' @param out_dir Output directory path of the generated file
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param stics_version Name of the STICS version (VX.Y format)
#' @param target_version Name of the STICS version to upgrade files to
#'  (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_sols_xml(
#'   file = file.path(dir_path,"sols.xml" ),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_sols_xml <- function(file,
                             out_dir,
                             param_gen_file,
                             stics_version = "V9.2",
                             target_version = "V10.0",
                             check_version = TRUE,
                             overwrite = FALSE) {


  # hecking output directory
  if (!dir.exists(out_dir)) dir.create(out_dir)


  # checking version
  if (check_version) {
    min_version <- get_version_num("V9.1")

    # extracting or detecting the STICS version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file,
      stics_version,
      param_gen_file = param_gen_file
    )

    if (!file_version && is.null(param_gen_file)) {
      stop("param_gen_file must be provided! ")
    }

    if (!file_version) {
      stop(
        "The input version ", stics_version,
        " does not match file version ",
        attr(file_version, "version"), " \n", file
      )
    }

    # Compatibility checks between version and update to target_version
    ver_num <- get_version_num(stics_version)
    if (ver_num < min_version) {
      stop(
        "Files from the version ", stics_version,
        " cannot be converted to the version ", target_version
      )
    }


    # for checking only once when multiple files are treated !
    check_version <- FALSE
  }


  # Loading the old doc
  old_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(old_doc, new_version = target_version,
                       overwrite = overwrite)

  # Checking if layer @nom are up to date (old @nom = horizon)
  tableau_noms <- unlist(get_nodes(old_doc, "//tableau/@nom"))

  if (any(grep(pattern = "horizon", tableau_noms))) {
    new_names <- unlist(lapply(tableau_noms,
        function(x) gsub(pattern = "horizon(.*)", x, replacement = "layer\\1")))
    set_attrs_values(old_doc, "//tableau", "nom", new_names)
  }

  # Nodes to add
  new_node <- XML::xmlParseString(
    '<param format="real" max="1.0" min="0.0" nom="finert">0.65000</param>',
    addFinalizer = TRUE
  )
  # new_node <- XML::xmlParseString('<param nom="finert">0.65000</param>',
  #                           addFinalizer = TRUE)

  prev_sibling <- get_nodes(old_doc, "//param[@nom='CsurNsol']")

  # added for compatibility with old misspelled parameters
  if (is.null(prev_sibling)) {
    prev_sibling <- get_nodes(old_doc, "//param[@nom='csurNsol']")
    # updating nom attribute content
    set_attrs_values(old_doc,
      path = "//param[@nom='csurNsol']",
      attr_name = "nom",
      values_list = "CsurNsol"
    )
  }

  for (n in seq_along(prev_sibling)) {
    XML::addSibling(prev_sibling[[n]], XML::xmlClone(new_node))
  }

  # writing sols.xml file
  write_xml_file(old_doc, file.path(out_dir, basename(file)), overwrite)

  XML::free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
