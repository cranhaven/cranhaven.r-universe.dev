#' @title Write R data frame to the Open Data Format.
#'
#' @description Export data from an R data frame to a ZIP file that stores
#' the data as Open Data Format.
#'
#' @import xml2
#' @import magrittr
#' @import data.table
#' @importFrom zip zip
#'
#' @param x R data frame (df) to be writtem.
#'
#' @param file Path to ZIP file or name of zip file to save the odf-dataset
#' in the working directory.
#'
#' @param languages
#' Select the language in which the descriptions and labels of the data will be
#' exported
#' * By default all available language variants are exported
#' (\code{languages = "all"}).
#' * You can also choose to export only the default language
#' (\code{languages = "default"}),
#' * Or only the current language
#' (\code{languages = "current"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#'
#' @param compression_level
#' A number between 1 and 9. 9 compresses best, but it also takes the longest.
#'
#' @param export_data
#' Choose, if you want to export the file that holds the
#' data (data.csv).Default is TRUE.
#' 
#' @param odf_version
#' The ODF version of the output file. Default is the actual/most recent version.
#'
#' * By default the data and metadata are exported (\code{export_data = TRUE}).
#' * To export only metadata and no data, select \code{export_data = FALSE}
#'
#' @param verbose Display more messages.
#'
#' @return ZIP file and unzipped directory containing the data as CSV file and
#' the metadata as XML file (DDI Codebook 2.5.).
#'
#' @examples
#' # get example data from the opendataformat package
#' df  <-  get(data("data_odf"))
#'
#' # write R data frame with attributes to the file my_data.zip specified
#' # as Open Data Format.
#' write_odf(x = df, paste0(tempdir(), "/my_data.zip"))
#' 
#' # write R data frame with attributes to the file my_data.zip
#' # with selected language.
#' write_odf(x = df,  paste0(tempdir(), "/my_data.zip"), languages = "en")
#'
#' # write R data frame with attributes to the file my_data.zip but only
#' # metadata, no data.
#' write_odf(x = df,  file = paste0(tempdir(), "/my_data.zip"), export_data = FALSE)
#'
#'
#' @export
write_odf  <-  function(x,
                        file,
                        languages = "all",
                        export_data = TRUE,
                        verbose = TRUE,
                        compression_level = 5,
                        odf_version = "1.1.0") {
  if (!("data.frame" %in% class(x))){
    stop("x must be a tibble or data.frame")
  }
  if (!inherits(file, "character")){
    stop("file be a filename or filepath")
  }
  if (!inherits(languages,"character")){
    stop("languages must be type character")
  }
  #Get all metadata languages in the dataset
  get_langs <- function(strings) {
    output <- c()
    for (string in strings){
      # Check if the string starts with "label_" or "description_"
      if (grepl("^label_|^labels_|^description_", string)) {
        # Split the string at "_", and take the second element
        split_string <- strsplit(string, "_")[[1]]
        output<- c(output, (split_string[2]))  # Extract the second element
      }
    }
    return(unique(output))
  }
  
  langs_in_data <- get_langs(names(attributes(x)))
  for  (var in names(x)){
    langs_in_data <- unique(c(langs_in_data, get_langs(names(attributes(x[[var]])))))
  }
  #Check if each language is available
  langs_not_in_data <- c()
  for (l in languages){
    if (!(l %in% c(langs_in_data, "all", "default","active", "current"))){
      langs_not_in_data <- c(langs_not_in_data,l)
    }
  }
  #Throw an error if any language input is not available
  if (length(langs_not_in_data)>0){
    stop(paste0("language(s) ", paste0(langs_not_in_data, collapse = ", "), " not available."))
  }

  if (!inherits(export_data, "logical")){
    stop("export_data must be TRUE or FALSE")
  }
  if (!inherits(verbose,"logical")){
    stop("verbose must be TRUE or FALSE")
  }
  if (!(compression_level %in%  1:9)){
    stop("compression_level must be an integer between 1 and 9")
  }
  
  
  # Normalize path from from relative to absolute
  file <- normalizePath(file, winslash = "/", mustWork = FALSE)
  # replace \\\\ with // to avert errors in data.table(...)
  file <- gsub("\\\\\\\\", "//", file)
  
  #check if file path exists
  if (!file.exists(paste0(unlist(strsplit(file, "/"))[1:(length(unlist(strsplit(file, "/")))-1)], collapse = "/"))) {
    stop("Error: Invalid file path. Output directory does not exist.")
  }
  if (!grepl(".zip", file) & !grepl(".odf", file)) {
    file <- paste0(file, ".odf.zip")
  }
  if (!grepl(".zip", file) & grepl(".odf", file)) {
    file <- paste0(file, ".zip")
  }

  #Remove label attributes for haven in active language
  if (!is.null(attr(x, "label"))) {
    attr(x, "label") <- NULL
  }
  for (var in names(x)){
    if (!is.null(attr(x[[var]], "label"))) {
      attr(x[[var]], "label") <- NULL
    }
  }


  unlink(paste0(tempdir(), "/*"), recursive = TRUE)
  folder_url <- gsub(".zip", "", file)
  folder_url <- gsub("\\\\", "/", folder_url)
  folder_name <- strsplit(folder_url, "/")[[1]][length(strsplit(folder_url,
                                                                "/")[[1]])]
  root_dir <- paste0(strsplit(folder_url, "/")[[1]][-length(
    strsplit(folder_url, "/")[[1]])], "/", collapse = "")
  if (root_dir == "/") {
    root_dir <- getwd()
    file <- paste0(getwd(), "/", file)
  }
  if (dir.exists(root_dir) == FALSE &&
      dir.exists(paste0("/", root_dir)) == FALSE &&
      dir.exists(paste0("//", root_dir)) == FALSE) {
    stop("File path not found")
  }

  dir.create(paste0(tempdir(), "/", folder_name), showWarnings = FALSE)
  
  version_major <- unlist(strsplit(odf_version, "\\."))[1]
  version_minor <- unlist(strsplit(odf_version, "\\."))[2]
  version_path <- unlist(strsplit(odf_version, "\\."))[3]
  #create version file
  if (version_major > 1 | version_major == 1 & version_minor >= 1){
    json_data <- list(
      fileType = "opendataformat",
      version = odf_version,
      files = list(
        data = "data.csv",
        metadata = "metadata.xml"
      )
    )
    json_string <- jsonlite::toJSON(json_data, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_string, paste0(tempdir(), "/", folder_name, "/odf-version.json"))
    vers_file = "odf-version.json"
  } else {
    vers_file = NULL
  }
  
  if (export_data  == TRUE) {
    
    # Tell R to save large numbers in non-scientific notation 
    # (not as e.g. 5.3e-10)
    current_scipen<-getOption("scipen")
    options(scipen = 999)
    data.table::fwrite(
      x = x, 
      file = paste0(tempdir(), "/", folder_name, "/data.csv"),
      na = "", encoding = "UTF-8")
    # Reset setting
    options(scipen = current_scipen)
  }

  # Create xml root node with codeBook attributes
  metadata <- xml2::xml_new_root(.value = "codeBook")
  
  # Create codebook attributes
  xml2::xml_attr(metadata, attr = "xmlns:xsi") <- "http://www.w3.org/2001/XMLSchema-instance"
  xml2::xml_attr(metadata, attr = "xmlns") <- "ddi:codebook:2_5"
  xml2::xml_attr(metadata, attr = "xsi:schemaLocation") <- "ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd"
  xml2::xml_attr(metadata, attr = "version") <- "2.5"
  
  # Create mandatory study description metadata
  stdyDscr <- xml2::xml_add_child(metadata, "stdyDscr")
  citation <- xml2::xml_add_child(stdyDscr, "citation")
  titlStmt <- xml2::xml_add_child(citation, "titlStmt")
  xml2::xml_add_child(titlStmt, "titl", attr(x, "study"))
  
  # Create dataset/file metadata
  fileDscr <- xml2::xml_add_child(metadata, "fileDscr")
  fileTxt <- xml2::xml_add_child(fileDscr, "fileTxt")
  xml2::xml_add_child(fileTxt, "fileName", attr(x, "name"))
  
  # Add dataset labels
  fileCitation <- xml2::xml_add_child(fileTxt, "fileCitation")
  fileTitlStmt <- xml2::xml_add_child(fileCitation, "titlStmt")
  
  if (length(names(attributes(x))[grepl("label", names(attributes(x)))]) > 0) {
    first_lang <- FALSE
    for (labl in names(attributes(x))[grepl("label", names(attributes(x)))]) {
      lang <- strsplit(labl, "_")[[1]][2]
      if (languages[1] == "all" || lang %in% languages) {
        if (first_lang == FALSE) {
          if (lang == "NA") {
            xml2::xml_add_child(fileTitlStmt, "titl", attr(x, labl))
          } else {
            xml2::xml_add_child(fileTitlStmt, "titl", attr(x, labl), "xml:lang" = lang)
          }
          first_lang <- TRUE
        } else {
          if (lang == "NA") {
            xml2::xml_add_child(fileTitlStmt, "parTitl", attr(x, labl))
          } else {
            xml2::xml_add_child(fileTitlStmt, "parTitl", attr(x, labl), "xml:lang" = lang)
          }
        }
      }
    }
  }
  
  
  # Add dataset descriptions
  if (length(names(attributes(x))[grepl("description", names(attributes(x)))]) > 0) {
    for (descr in names(attributes(x))[grepl("description", names(attributes(x)))]) {
      lang <- strsplit(descr, "_")[[1]][2]
      if (languages[1] == "all" || lang %in% languages) {
        if (lang == "NA") {
          xml2::xml_add_child(fileTxt, "fileCont", attr(x, descr))
        } else {
          xml2::xml_add_child(fileTxt, "fileCont", attr(x, descr), "xml:lang" = lang)
        }
      }
    }
  }
  
  # Create dataset URL
  url <- attr(x, "url")
  if (is.null(url)) url <- ""
  notes <- xml2::xml_add_child(fileDscr, "notes")
  xml2::xml_add_child(notes, "ExtLink", "URI" = url)
  
  # Add data (variable) metadata
  dataDscr <- xml2::xml_add_child(metadata, "dataDscr")
  
  # Add metadata for each variable
  for (var in names(x)) {
    varNode <- xml2::xml_add_child(dataDscr, "var", "name" = var)
    
    # Add variable labels
    if (length(names(attributes(x[[var]]))[grepl("label", names(attributes(x[[var]])))]) > 0) {
      for (labl in names(attributes(x[[var]]))[grepl("label", names(attributes(x[[var]])))]) {
        if (!grepl("labels", labl)) {
          lang <- strsplit(labl, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            if (lang == "NA") {
              xml2::xml_add_child(varNode, "labl", attr(x[[var]], labl))
            } else {
              xml2::xml_add_child(varNode, "labl", attr(x[[var]], labl), "xml:lang" = lang)
            }
          }
        }
      }
    }
    
    # Add variable descriptions
    if (length(names(attributes(x[[var]]))[grepl("description", names(attributes(x[[var]])))]) > 0) {
      for (descr in names(attributes(x[[var]]))[grepl("description", names(attributes(x[[var]])))]) {
        if (!grepl("labels", descr)) {
          lang <- strsplit(descr, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            if (lang == "NA") {
              xml2::xml_add_child(varNode, "txt", attr(x[[var]], descr))
            } else {
              xml2::xml_add_child(varNode, "txt", attr(x[[var]], descr), "xml:lang" = lang)
            }
          }
        }
      }
    }
    
    # Add value labels
    if (length(names(attributes(x[[var]]))[grepl("labels", names(attributes(x[[var]])))]) > 0) {
      labels <- names(attributes(x[[var]]))[grepl("labels", names(attributes(x[[var]])))]
      values <- unique(unlist(lapply(labels, function(lab) attr(x[[var]], lab))))
      
      for (val in values) {
        catgryNode <- xml2::xml_add_child(varNode, "catgry")
        xml2::xml_add_child(catgryNode, "catValu", val)
        
        for (labl in labels) {
          lang <- strsplit(labl, "_")[[1]][2]
          if (languages[1] == "all" || lang %in% languages) {
            labl_new <- names(attr(x[[var]], labl))[attr(x[[var]], labl) == val]
            if (!is.na(labl_new)) {
              if (lang == "NA") {
                xml2::xml_add_child(catgryNode, "labl", labl_new)
              } else {
                xml2::xml_add_child(catgryNode, "labl", labl_new, "xml:lang" = lang)
              }
            }
          }
        }
      }
    }
    
    # Add variable type
    type <- attr(x[[var]], "type")
    if (is.null(type)) type <- class(x[[var]])
    xml2::xml_add_child(varNode, "varFormat", "type" = type)
    
    # Add URL
    url <- attr(x[[var]], "url")
    if (is.null(url)) url <- ""
    notesVar <- xml2::xml_add_child(varNode, "notes")
    xml2::xml_add_child(notesVar, "ExtLink", "URI" = url)
  }
  

  xml2::write_xml(metadata, paste0(tempdir(), "/", folder_name, "/metadata.xml"))

  #Zip directory
  old_wd <- getwd()
  setwd(paste0(tempdir(), "/", folder_name))
  on.exit(setwd(old_wd))
  if (export_data == TRUE) {
    zip::zip(zipfile = file, files = c("data.csv", "metadata.xml", vers_file),
             compression_level = compression_level)
  } else {
    zip::zip(zipfile = file, files = c("metadata.xml"),
             compression_level = compression_level)
  }

  setwd(old_wd)

  #check if write_odf was successful
  if (file.exists(file) && verbose == TRUE) {
    print(
      paste0(
        "Dataset successfully written to '", file, "'"
      )
    )
  } else {
    if (!file.exists(file)) stop("Datasaet was not written successfully")
  }
}
