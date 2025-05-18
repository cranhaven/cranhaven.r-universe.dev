#' Upgrading a param_newform.xml file to a newer version
#'
#' @param file Path of a param_newform.xml file
#' @param out_dir Output directory path of the generated file
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
#'
#' @return None
#'
#' @export
#'
#' @details See SticsRFiles::get_stics_versions_compat() for listing versions
#'
#' @examples

#' dir_path <- get_examples_path(file_type = "xml", stics_version = "V9.2")
#'
#' upgrade_param_newform_xml(
#'   file = file.path(dir_path,"param_newform.xml"),
#'   out_dir = tempdir(),
#'   param_gen_file = file.path(dir_path, "param_gen.xml")
#' )
#'
#'
upgrade_param_newform_xml <- function(file,
                                      out_dir,
                                      param_gen_file,
                                      stics_version = "V9.2",
                                      target_version = "V10.0",
                                      check_version = TRUE,
                                      overwrite = FALSE
) {

  # TODO: eliminate when option will be reactivated later.
  codemineral <- FALSE

  # Checking output directory
  if (!dir.exists(out_dir)) dir.create(out_dir)


  if (check_version) {
    min_version <- get_version_num("V9.1")

    # Extracting or detecting the STICS version corresponding to the xml file
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
  }



  # Loading the old doc
  old_doc <- xmldocument(file = file)

  # Setting file STICS version
  set_xml_file_version(old_doc,
                       new_version = target_version,
                       overwrite = overwrite)


  # TODO : add from here if cond for calling specific version ranges updates



  # nodes to remove
  form_names <- c(
    "Specificities of cut crops",
    "Activation of the module simulating tiller dynamics",
    "Calculation of the maximal reserve compartment during reproductive stages",
    "Calculation of the stem elongation stage for perenial grasslands",
    "Moisture test for sowing decision",
    paste0("automatic irrigations (associated with the options of automatic ",
           "irrigation in tec file)"),
    "calculation of the root death at cutting date for grasslands",
    "option for several thinning ",
    "option for several fertilizer type ",
    # useless, options now removed
    "residue incorporation"
  )

  nodes_to_rm <- lapply(form_names, function(x) {
    get_nodes(
      old_doc,
      path = paste0("//formalisme[@nom='", x, "']")
    )
  })

  lapply(nodes_to_rm, function(x) if (!is.null(x)) XML::removeNodes(x))


  # options to be removed
  opt_names <- c(
    "New mineralization of soil organic matter "
  )

  nodes_to_rm <- lapply(opt_names, function(x) {
    get_nodes(
      old_doc,
      path = paste0("//option[@nom='", x, "']")
    )
  })
  lapply(nodes_to_rm, function(x) XML::removeNodes(x))

  # roots
  new_node <- XML::xmlParseString(
    '<formalisme nom="New Roots">
    <param format="integer" max="1.0" min="0.0" nom="humirac">1</param>
  </formalisme>',
    addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(
    old_doc,
    path = "//formalisme[@nom='Mineralization models']"
  )[[1]]
  XML::addSibling(prev_sibling, XML::xmlClone(new_node), after = TRUE)

  # options to add
  # min, CsurN

  parent_node <- prev_sibling

  if (codemineral) {

    new_node <- list(XML::xmlParseString(
      '<option choix="1" nom="New mineralization model" nomParam="codemineral">
  <choix code="1" nom="no"/>
  <choix code="2" nom="new_minr"/>
  <choix code="3" nom="new_minh+new_minr"/>
</option>',
      addFinalizer = TRUE),
      XML::xmlParseString(
        '<option choix="2" nom="CsurNsol dynamic"
      nomParam="code_CsurNsol_dynamic">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
        addFinalizer = TRUE
      ))

    lapply(new_node,
           function(x) XML::addChildren(parent_node, XML::xmlClone(x))
    )
  } else {

    # if a version 10.0 file is retreated
    # codemineral option must be retreived for the moment

    codemineral_node <- get_nodes(
      old_doc,
      path = paste0("//option[@nomParam='codemineral']")
    )

    if (!is.null(codemineral_node)) XML::removeNodes(codemineral_node)

    new_node <- XML::xmlParseString(
      '<option choix="2" nom="CsurNsol dynamic"
      nomParam="code_CsurNsol_dynamic">
  <choix code="1" nom="yes"/>
  <choix code="2" nom="no"/>
</option>',
      addFinalizer = TRUE
    )

    XML::addChildren(parent_node, XML::xmlClone(new_node))
  }


  # formalism modifications
  # replacing formalisme option @nom, choix
  set_attrs_values(
    old_doc,
    path = "//option[@nomParam='codecalferti']",
    attr_name = "nom",
    values_list = "automatic calculation of fertilisation"
  )

  set_attrs_values(
    old_doc,
    path = "//option[@nomParam='codetesthumN']",
    attr_name = "nom",
    values_list = paste0("automatic N fertilisation (1 = based on rainfall",
                         " 2 = based on soil water content)")
  )

  set_attrs_values(
    old_doc,
    path = "//option[@nomParam='codetesthumN']",
    attr_name = "choix",
    values_list = "1"
  )



  # TODO: see what to do for the future v10 version !
  # ---------------------------------------------------------------------------
  # ISOP specific option to temporarily add
  new_node <- XML::xmlParseString('<formalisme nom="ISOP">
		<option choix="2" nom="activation of ISOP equations" nomParam="code_ISOP">
			<choix code="1" nom="yes">
			<option choix="2" nom="activation of legume fixation in grassland"
			nomParam="code_pct_legume">
				<choix code="1" nom="yes">
					<param format="real" max="1.0" min="0.0" nom="pct_legum">0.5</param>
				</choix>
				<choix code="2" nom="no"/>
			</option>
			</choix>
			<choix code="2" nom="no"/>
		</option>
   </formalisme>',
                                  addFinalizer = TRUE
  )

  prev_sibling <- get_nodes(
    old_doc,
    path = "//formalisme[@nom='New Roots']"
  )[[1]]
  XML::addSibling(prev_sibling, XML::xmlClone(new_node))
  # ---------------------------------------------------------------------------


  # Writing to file param_newform.xml
  write_xml_file(old_doc,
                 file.path(out_dir, basename(file)),
                 overwrite = overwrite)

  XML::free(old_doc@content)
  invisible(gc(verbose = FALSE))
}
