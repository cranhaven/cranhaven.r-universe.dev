#' Upgrading _ini.xml file(s) to a newer version
#'
#' @param file Path of an initialisation (*_ini.xml) file or a vector of
#' @param out_dir Output directory path of the generated files
#' @param param_gen_file Path of the param_gen.xml file corresponding
#' to the file version
#' @param stics_version Name of the STICS version (VX.Y format)
#' @param target_version Name of the STICS version to upgrade files
#' to (VX.Y format)
#' @param check_version Perform version consistency with in stics_version input
#' with the file version and finally checking if the upgrade is possible
#' allowed to the target_version. If TRUE, param_gen_file is mandatory.
#' @param overwrite logical (optional),
#' TRUE for overwriting file if it exists, FALSE otherwise
#' @param ... Additional input arguments
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
#' upgrade_ini_xml(
#'   file = file.path(dir_path,"file_ini.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
upgrade_ini_xml <- function(file,
                            out_dir,
                            param_gen_file,
                            stics_version = "V9.2",
                            target_version = "V10.0",
                            check_version = TRUE,
                            overwrite = FALSE,
                            ...) {


  # for verifying output dir existence
  check_dir <- TRUE
  args <- list(...)
  if ("check_dir" %in% names(args)) check_dir <- args$check_dir
  if (check_dir) {
    if (!dir.exists(out_dir)) dir.create(out_dir)
    # for checking only once when multiple files are treated !
    check_dir <- FALSE
  }

  # checking version
  if (check_version) {
    min_version <- get_version_num("V9.1")

    # extracting or detecting the STICS version corresponding to the xml file
    # based on param_gen.xml file content
    file_version <- check_xml_file_version(file[1],
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
        attr(file_version, "version"), " \n", file[1]
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


  # Treating a files list
  if (length(file) > 1) {
    lapply(file, function(x) {
      upgrade_ini_xml(
        file = x,
        out_dir = out_dir,
        stics_version = stics_version,
        target_version = target_version,
        check_version = check_version,
        param_gen_file = param_gen_file,
        overwrite = overwrite,
        check_dir = check_dir
      )
    })
    return(invisible())
  }


  # Loading the old xml file
  old_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(old_doc,
                       new_version = target_version,
                       overwrite = overwrite)

  # Keeping old values
  rm_names <- c("masec0", "QNplante0", "resperenne0")
  old_values <- get_param_value(old_doc, rm_names)

  # Removing useless nodes
  rm_nodes <- unlist(lapply(rm_names, function(x) {
    unlist(get_nodes(
      old_doc,
      path = paste0("//", x)
    ))
  }))
  lapply(rm_nodes, function(x) XML::removeNodes(x))


  # Adding new option node
  # including old nodes masec0,QNplante0,restemp0
  # (previously named resperennes0)


  str_1 <- paste0('<option choix="2" nom="Simulation of Nitrogen and Carbon',
                  ' reserves" nomParam="code_acti_reserve">\n')
  str_2 <-
  '<choix code="1" nom="yes">
		<maperenne0>0</maperenne0>
		<QNperenne0>0</QNperenne0>
		<masecnp0>0</masecnp0>
		<QNplantenp0>0</QNplantenp0>
	</choix>
	<choix code="2" nom="no">
		<masec0>0</masec0>
		<QNplante0>0</QNplante0>
		<restemp0>0</restemp0>
	</choix>
 </option>'

  str <- paste0(str_1, str_2)

  new_node <- XML::xmlParseString(str, addFinalizer = TRUE)

  # Getting zrac0 node
  prev_sibling <- unlist(get_nodes(old_doc, "//zrac0"))

  # Adding new node
  lapply(prev_sibling, function(x) XML::addSibling(x, XML::xmlClone(new_node)))

  # setting values for restructured nodes
  # resperennes0 became restemp0
  rm_names <- c("masec0", "QNplante0", "restemp0")
  set_param_value(old_doc,
                  param_name = as.list(rm_names),
                  param_value = old_values)


  if (is.null(get_nodes(old_doc, "//snow"))) {
    # Adding snow node
    new_node <- XML::xmlParseString(
      "<snow>
    <Sdepth0>0.0</Sdepth0>
    <Sdry0>0.0</Sdry0>
    <Swet0>0.0</Swet0>
    <ps0>0.0</ps0>
    </snow>",
      addFinalizer = TRUE
    )

    parent_node <- get_nodes(old_doc, path = "//initialisations")[[1]]

    XML::addChildren(parent_node, XML::xmlClone(new_node))
  } else {
    # checking names an renaming them !
    old_names <- c("SDepth", "Sdry", "Swet", "ps")
    new_names <- c("Sdepth0", "Sdry0", "Swet0", "ps0")
    n <- get_nodes(old_doc, c(sprintf("//%s", old_names)))

    if (!is.null(n)) {
      nodes_idx <- unlist(lapply(n, XML::xmlName)) %in% old_names
      n <- n[nodes_idx]
      new_names <- new_names[nodes_idx]
      for (i in seq_along(length(n))) {
        XML::xmlName(n[[i]]) <- new_names[i]
      }
    }
  }

  # Renaming soil parameters
  # hinit, NO3init, NH4init => hinitf, NO3initf, NH4initf
  current_node <- get_nodes(old_doc, path = "//hinit")[[1]]
  XML::xmlName(current_node) <- "Hinitf"
  current_node <- get_nodes(old_doc, path = "//NO3init")[[1]]
  XML::xmlName(current_node) <- "NO3initf"
  current_node <- get_nodes(old_doc, path = "//NH4init")[[1]]
  XML::xmlName(current_node) <- "NH4initf"


  # Writing to file _ini.xml
  out_ini <- file.path(out_dir, basename(file))
  write_xml_file(old_doc, out_ini, overwrite)



  XML::free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
