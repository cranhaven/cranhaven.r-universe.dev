#' Upgrading a usms.xml file to a newer version
#'
#' @param file Path of a usms.xml file
#' @param out_dir Output directory path of the generated file
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param obs_dir Directory path of the observation data files
#' @param stics_version Name of the STICS version (VX.Y format)
#' @param target_version Name of the STICS version to upgrade files
#' to (VX.Y format)
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
#' @details See get_stics_versions_compat() for listing versions
#'
#' @examples
#'
#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_usms_xml(
#'   file = file.path(dir_path,"usms.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_usms_xml <- function(file,
                             out_dir,
                             param_gen_file,
                             obs_dir = NULL,
                             stics_version = "V9.2",
                             target_version = "V10.0",
                             check_version = TRUE,
                             overwrite = FALSE) {

  # Checking output directory
  if (!dir.exists(out_dir)) dir.create(out_dir)


  # checking version
  if (check_version) {
    if (is.null(param_gen_file)) stop("param_gen_file must be provided! ")

    min_version <- get_version_num("V9.1")

    # extracting or detecting the STICS version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file,
                                           stics_version,
                                           param_gen_file = param_gen_file
    )

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


  if (is.null(obs_dir)) obs_dir <- dirname(file)

  # loading the old doc
  old_doc <- xmldocument(file = file)

  # setting file STICS version
  set_xml_file_version(old_doc,
                       new_version = target_version,
                       overwrite = overwrite
  )

  # checking if fobs exist
  obs_nodes <- get_nodes(old_doc, "//fobs")

  # TODO: detect if fobs exist and evaluate
  # where to add fobs fields !!!!!
  # default behavior: no existing fobs fields
  if (is.null(obs_nodes)) {
    new_node <- XML::xmlParseString("<fobs>null</fobs>",
                                    addFinalizer = TRUE
    )

    parent_node <- get_nodes(old_doc, "//plante")

    lapply(parent_node,
           function(x) XML::addChildren(x, XML::xmlClone(new_node))
    )
  }


  # Usms names
  usms_names <- get_attrs_values(old_doc, "//usm", "nom")

  # existing obs files
  # intercrops usms are not taken into account in that case
  obs_names <- paste0(usms_names, ".obs")
  obs_exist <- file.exists(file.path(obs_dir, obs_names))
  obs_val <- rep("null", length(usms_names))
  obs_val[obs_exist] <- obs_names[obs_exist]

  # Setting obs files names into fobs for existing files
  set_param_value(old_doc,
                  param_name = "fobs",
                  param_value = obs_val,
                  parent_name = "plante",
                  parent_sel_attr = "1"
  )


  # writing file
  write_xml_file(old_doc, file.path(out_dir, basename(file)), overwrite)

  XML::free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
