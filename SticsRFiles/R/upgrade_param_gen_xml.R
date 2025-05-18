#' Upgrading a param_gen.xml file to a newer version
#'
#' @param file Path of a param_gen.xml file
#' @param out_dir Output directory path of the generated file
#' @param stics_version Name of the STICS version (VX.Y format)
#' @param target_version Name of the STICS version to upgrade files
#' to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#'
#' @return None
#'
#' @export
#'
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_param_gen_xml(
#'   file = file.path(dir_path, "param_gen.xml"),
#'   out_dir = tempdir()
#' )

upgrade_param_gen_xml <- function(file,
                                  out_dir,
                                  stics_version = "V9.2",
                                  target_version = "V10.0",
                                  check_version = TRUE,
                                  overwrite = FALSE) {


  # Checking output directory
  if (!dir.exists(out_dir)) dir.create(out_dir)


  if (check_version) {
    min_version <- get_version_num("V9.1")

    # Extracting or detecting the STICS version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file, stics_version)

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
  }


  # Loading the old doc
  old_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(old_doc,
                       new_version = target_version,
                       overwrite = overwrite)

  # Nodes to remove
  rm_names <- c("FINERT", "FMIN1", "FMIN2", "FMIN3", "khaut", "rayon", "concrr")

  rm_nodes <- lapply(rm_names, function(x) {
    get_nodes(
      old_doc,
      path = paste0("//param[@nom='", x, "']")
    )
  })
  lapply(rm_nodes, function(x) XML::removeNodes(x))


  # Nodes to change
  # <param format="real" max="20.0" min="1.0" nom="k_desat">3.0</param>
  # k_desat to kdesat
  nodes_to_change <- get_nodes(old_doc, path = "//param[@nom='k_desat']")
  if (!is.null(nodes_to_change)) {
    set_attrs_values(old_doc,
                  path = "//param[@nom='k_desat']",
                  attr_name = "nom",
                  values_list = "kdesat"
    )
  }



  # Nodes to add
  new_node <- XML::xmlParseString(
    '<param format="real" max="1.0" min="0.0" nom="GMIN1">0.0007</param>
<param format="real" max="1.0" min="0.0" nom="GMIN2">0.02519</param>
<param format="real" max="1.0" min="0.0" nom="GMIN3">0.015</param>
<param format="real" max="1.0" min="0.0" nom="GMIN4">0.11200</param>
<param format="real" max="11.0" min="3.0" nom="GMIN5">8.50000</param>
<param format="real" max="1.0" min="0.0" nom="GMIN6">0.06000</param>
<param format="real" max="35.0" min="5.0" nom="GMIN7">11.00000</param>',
    addFinalizer = TRUE
  )


  new_nodes <- XML::getNodeSet(new_node, path = "//param")
  prev_sibling <- get_nodes(old_doc, "//param[@nom='TREFr']")[[1]]

  # For adding them in the right order
  for (n in seq_along(new_nodes)) {
    new <- XML::xmlClone(new_nodes[[n]])
    XML::addSibling(prev_sibling, new)
    prev_sibling <- new
  }


  # Writing to file param_gen.xml
  write_xml_file(old_doc,
                 file.path(out_dir, basename(file)),
                 overwrite = overwrite)

  XML::free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
