get_in_files <- function(in_dir_or_files, kind) {
  files_patterns <- list(
    ini = "_ini", sta = "_sta",
    plt = "_plt", tec = "_tec",
    gen = "_gen", newform = "_newform",
    sols = "^sols", usms = "^usms", obs = "\\.obs$"
  )

  if (!kind %in% names(files_patterns)) stop("file kind error: ", kind)

  if (kind == "obs") {
    file_pattern <- files_patterns[[kind]]
  } else {
    file_pattern <- paste0(files_patterns[[kind]], "\\.xml$")
  }

  if (dir.exists(in_dir_or_files)) {
    files_list <- list.files(
      pattern = file_pattern, path = in_dir_or_files,
      full.names = TRUE
    )
  } else {
    # files path: add grep for filtering
    files_list <- grep(
      pattern = file_pattern, x = in_dir_or_files,
      value = TRUE
    )
    exist <- file.exists(files_list)
    if (!all(exist)) {
      warning("Some files do not exist", paste(files_list[!exist],
        collapse = ", "
      ))
      files_list <- files_list[exist]
    }
  }

  files_list
}

get_param_gen_file <- function(type = c("param_gen.xml", "param_newform.xml"),
                               workspace_dir,
                               javastics_dir = NULL) {
  par_file <- file.path(workspace_dir, type)

  if (file.exists(par_file)) {
    attr(par_file, "where") <- "workspace"
    return(par_file)
  }

  if (is.null(javastics_dir)) {
    warning(
      "JavaSTICS path must be given as input argument\n",
      type, " has not been found in ", workspace_dir
    )
    return()
  }

  par_file <- file.path(javastics_dir, "config", type)

  if (file.exists(par_file)) {
    attr(par_file, "where") <- "javastics"
    return(par_file)
  }

  warning(type, " has not been found in ", javastics_dir)

  return()
}

to_xml_version <- function(stics_version) {
  err <- FALSE

  if (is.numeric(stics_version)) stics_version <- as.character(stics_version)

  if (!grepl(pattern = "\\.", x = stics_version)) {
    stics_version <- paste0(stics_version, ".0")
  }
  numbers <- grepl(pattern = "[0-9]", stics_version)

  # no numbers in version
  if (!numbers) err <- TRUE

  char_no_v <- grepl(pattern = "[a-u w-z A-U W-Z]", stics_version)

  # Wrong character in version
  if (char_no_v) err <- TRUE

  # No dot in version

  if (grepl(pattern = "\\.$", x = stics_version)) err <- TRUE


  if (err) {
    warning("Version must be X.Y, or VX.Y, or vX.Y")
    return()
  }

  as.character(get_version_num(stics_version = stics_version, numeric = FALSE))
}


# set_xml_stics_version <- function(xml_file_or_doc, new_version="V10.0",
# overwrite = FALSE) {
set_xml_file_version <- function(xml_file_or_doc, new_version = "V10.0",
                                 overwrite = FALSE) {
  # Getting xml_document object
  xml_doc <- get_xml_doc(xml_file_or_doc)

  # Adding version to detect if the file have been previously updated to 10.0
  ver <- to_xml_version(new_version)
  names(ver) <- "version"
  root_path <- paste0("/", XML::xmlName(XML::xmlRoot(xml_doc@content)))
  att <- get_attrs(xml_doc, path = root_path)

  # Checking file version
  if (!is.null(att) && att[, "version"] == new_version && !overwrite) {
    stop(paste(
      "The version has already been updated to STICS version",
      new_version
    ))
  }

  add_attrs(xml_doc, path = root_path, named_vector = ver)
}

# TODO : see existing get_xml_stics_version, to be merged or replaced with
# this one and see what to do in gen_*_doc functions using templates ...
get_xml_file_version <- function(xml_file_or_doc, param_gen_file = NULL) {
  xml_doc <- get_xml_doc(xml_file_or_doc)
  xml_root_name <- XML::xmlName(XML::xmlRoot(xml_doc@content))

  att <- get_attrs(xml_doc, path = paste0("/", xml_root_name))

  if ("version" %in% colnames(att)) {
    version_string <- get_version_string(att[, "version"])
    return(version_string)
  }

  # Global detection of the version based for the moment on the
  # param_gen.xml file content
  if (xml_root_name != "fichierpar" && is.null(param_gen_file)) {
    warning("STICS version corresponding to the XML file was not detected.\n")
    return()
  }

  # Getting specific parameters attached to versions from param_gen file
  if (xml_root_name == "fichierpar") {
    param_gen_doc <- xml_doc
  } else {
    param_gen_doc <- get_xml_doc(param_gen_file)
  }
  # Using markers of versions
  codesnow <- get_param_value(param_gen_doc,
    param_name = "codesnow"
  )
  tmin_mineralisation <- get_param_value(param_gen_doc,
    param_name = "tmin_mineralisation"
  )

  # Vector of existence in the doc
  is_null <- c(is.null(codesnow), is.null(tmin_mineralisation))

  # none of them exist
  if (all(is_null)) {
    return("V8.5")
  }

  # only codesnow exists
  if (!is_null[1] && is_null[2]) {
    return("V9.0")
  }

  # both exist
  # How to make a distinction between 9.1 and 9.2 ?
  # using the STICS exe ???
  # For the moment returning a vector of versions !!!
  if (all(!is_null)) {
    return(c("V9.1", "V9.2"))
  }

  warning("Unknown version !")
  return()
}

# TODO: see *xml_file_version functions ...
# check_xml_stics_version <- function(xml_file_or_doc, version,
# param_gen_file = NULL) {
check_xml_file_version <- function(xml_file_or_doc, stics_version,
                                   param_gen_file = NULL) {
  # xml_version <- get_xml_stics_version(xml_file_or_doc,
  # param_gen_file = param_gen_file)
  xml_version <- get_xml_file_version(xml_file_or_doc,
    param_gen_file = param_gen_file
  )

  r <- TRUE

  if (is.null(xml_version)) {
    return(FALSE)
  }

  if (!stics_version %in% xml_version) r <- FALSE

  attr(r, "version") <- xml_version
  return(r)
}

get_xml_doc <- function(xml_file_or_doc) {
  type_id <- c("character", "xml_document") %in% class(xml_file_or_doc)

  if (!any(type_id)) stop("xml_file_or_doc: not a valid input,
                          must be a file path or an xml_document object!")

  if (type_id[1] && !file.exists(xml_file_or_doc)) {
    stop(
      xml_file_or_doc,
      ": does not exist!"
    )
  }

  if (type_id[1]) {
    return(xmldocument(xml_file_or_doc))
  }

  if (type_id[2]) {
    return(xml_file_or_doc)
  }
}


node_exist <- function(xml_file_or_doc, xpath) {
  xml_doc <- get_xml_doc(xml_file_or_doc)

  nodes <- get_nodes(xml_doc, xpath)

  if (is.null(nodes)) {
    return(FALSE)
  }

  unlist(lapply(nodes, function(x) !is.null(x)))
}


write_xml_file <- function(xml_doc, file, overwrite = FALSE) {
  if (file.exists(file) && (!overwrite)) {
    warning(file, ": \nalready exists, consider setting overwrite to TRUE")
    return(invisible(FALSE))
  }

  save_xml_doc(xml_doc, file)
  return(TRUE)
}
