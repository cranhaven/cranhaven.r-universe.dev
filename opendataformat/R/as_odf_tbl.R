#' @title Converts a data frame to odf_tbl
#'
#' @description Converts a data.frame (or any subclass) object to an odf_tbl 
#' (Open Data Format tibble).
#'
#' @param x
#' a data.frame that should be converted to an odf_tbl.
#' (\code{languages = "all"}).
#'
#'
#' @param active_language
#' Select the language that should be the active metadata language. 
#' Default is 'en', or the first language occurring.
#' 
#' @param language_of_metadata
#' Language of metadata, where language tag is missing, which is metadata in 
#' attributes label, description and labels. Default is NA.
#' 
#'
#' @return odf_tbl with attributes including dataset and variable
#' information.
#' 	
#'
#' @export
#' @examples
#' # Create a dataframe with 4 variables id, name, age, and diagnosis
#' exampledata <- data.frame(id = 1:5,
#'                           name = c("Klaus", "Anna", "Rebecca",
#'                                    "Kevin", "Janina"),
#'                           age = c(55, 40, 19, 25, 60), 
#'                           diagnosis = c(1,3,3,2,1))
#' # Add metadata for dataset
#' attr(exampledata, "name") <- "patientdata"
#' attr(exampledata, "label_en") <- "Patient Data"
#' attr(exampledata, "description_en") <- "Patient database of the practice Dr. Sommer"
#' attr(exampledata, "url") <- "www.example.url.en"
#' 
#' # Add metadata for diagnosis variable with label, description and value labels.
#' attr(exampledata$id, "name") <- "id"
#' attr(exampledata$id, "label_en") <- "Patiend ID"
#' attr(exampledata$id, "description_en") <- "Practice Patiend ID"
#' attr(exampledata$diagnosis, "name") <- "diagnose"
#' attr(exampledata$diagnosis, "label_en") <- "Diagnosis"
#' attr(exampledata$diagnosis, "description_en") <- "Diagnosis patient last visit"
#' valuelabels_diagnosis <- 1:4
#' names(valuelabels_diagnosis) <- c("Covid", "Influenza", "Common cold", "Tonsillitis")
#' attr(exampledata$diagnosis, "labels_en") <- valuelabels_diagnosis
#' # use as_odf_tbl() to transform dataframe to odf_tibble
#' example_odf  <-  as_odf_tbl(exampledata)
#' 
#' # Display metadata using docu_odf
#' docu_odf(example_odf, style = "print")
#' 
#' # Display metadata of diagnosis Variable
#' docu_odf(example_odf$diagnosis, style = "print")
#' 
#'




as_odf_tbl <- function(x, active_language = "en", language_of_metadata = NA){
  if (!"data.frame" %in% class(x)){
    stop("as_odf_tbl requires a data frame (data.frame, or any subclass like a tibble) as input.")
  }
  if (!is.character(active_language)) {
    stop("Error: 'active_language' must be a string (character type) with the active language for the metadata.")
  }
  if (!is.character(language_of_metadata) & !is.na(language_of_metadata)) {
    stop("Error: 'language_of_metadata' must be a string (character type) with the language for metadata without a language tag, or NA.")
  }
  
  odf_tibble <- tibble::as_tibble(x)
  class(odf_tibble)<-c("odf_tbl", class(odf_tibble))
  languages <- c(attr(odf_tibble, "languages"))
  
  # Check if any metadata without language tag (label, description, and labels) 
  # is unique, and if so, assign it to metadata field with language tag
  if ("label" %in%  names(attributes(odf_tibble))){
    if (!attr(odf_tibble, "label",exact = TRUE) %in% attributes(odf_tibble)[grep("^(label_)", names( attributes(odf_tibble)))]){
      attr(odf_tibble, paste0("label_", language_of_metadata)) <- attr(odf_tibble, "label",exact = TRUE)
    }
  }
  if ("description" %in%  names(attributes(odf_tibble))){
    if (!attr(odf_tibble, "description",exact = TRUE) %in% attributes(odf_tibble)[grep("^(description_)", names( attributes(odf_tibble)))]){
      attr(odf_tibble, paste0("description_", language_of_metadata)) <- attr(odf_tibble, "description",exact = TRUE)
      attr(odf_tibble, "description") <- NULL
    }
  }
  for (col in colnames(odf_tibble)){
    if ("label" %in%  names(attributes(odf_tibble[[col]]))){
      if (!attr(odf_tibble[[col]], "label",exact = TRUE) %in% attributes(odf_tibble[[col]])[grep("^(label_)", names( attributes(odf_tibble[[col]])))]){
        attr(odf_tibble[[col]], paste0("label_", language_of_metadata)) <- attr(odf_tibble[[col]], "label",exact = TRUE)
      }
    }
    if ("description" %in%  names(attributes(odf_tibble[[col]]))){
      if (!attr(odf_tibble[[col]], "description",exact = TRUE) %in% attributes(odf_tibble[[col]])[grep("^(description_)", names( attributes(odf_tibble[[col]])))]){
        attr(odf_tibble[[col]], paste0("description_", language_of_metadata)) <- attr(odf_tibble[[col]], "description",exact = TRUE)
        attr(odf_tibble[[col]], "description") <- NULL
      }
    }
    if ("labels" %in%  names(attributes(odf_tibble[[col]]))){
      labels_unique <- FALSE
      valuelabel_list <- attributes(odf_tibble[[col]])[grep("^(labels_)", names( attributes(odf_tibble[[col]])))]
      if (length(valuelabel_list)==0){
        labels_unique <- TRUE
      } else {
        for (valuelabels in valuelabel_list){
          
          if (all.equal(attr(odf_tibble[[col]], "labels", exact = TRUE), valuelabels)[1]!= TRUE) {
            labels_unique <- TRUE
          }
        }
      }
      if (labels_unique == TRUE){
        attr(odf_tibble[[col]], paste0("labels_", language_of_metadata)) <- attr(odf_tibble[[col]], "labels",exact = TRUE)
        attr(odf_tibble[[col]], "labels") <- NULL
      }
    }
  }
  
  # Check languages of metadata (labels, descriptions and valuelabels)
  attr_names <- names(attributes(odf_tibble))  
  for (col in colnames(odf_tibble)){
    attr_names <- c(attr_names, names(attributes(odf_tibble[[col]])))
  }
  for (nam in attr_names){
    if(grepl("^(label_|labels_|description_)", nam)){
      lang = unlist(strsplit(nam, split = "_"))[2]
      if (!lang %in% languages){
        languages <- c(languages, lang)
      }
    }
  }
  if (is.null(languages)) {
    languages <- "NA"
  }
  # add language attribute and name attribue to variables, if missing
  attr(odf_tibble, "languages") <- languages
  for (col in colnames(odf_tibble)){
    attr(odf_tibble[[col]], "languages") <- languages
    if (!"name" %in% names(attributes(odf_tibble[[col]]))){
      attr(odf_tibble[[col]], "name") <- col
    }
  }
  if(active_language %in% languages){
    attr(odf_tibble, "lang") <- active_language
  } else {
    if (active_language != "en" & "en" %in% languages){
      attr(odf_tibble, "lang") <- "en"
      print(paste0("No metadata in '",active_language, "' available. Active language is 'en'."))
    } else {
      attr(odf_tibble, "lang") <- languages[1]
      print(paste0("No metadata in ",active_language, " available. Active language is '", languages[1], "'."))
    }
  }
  odf_tibble <- opendataformat::setlanguage_odf(odf_tibble, attr(odf_tibble, "lang", exact = TRUE))
  return(odf_tibble)
}
