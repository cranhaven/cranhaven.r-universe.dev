#NOTE:
# NDC code, used in this program, is obtained from the National Drug Code Database, an external website.
# https://www.fda.gov/drugs

#' @importFrom data.table data.table
#' @importFrom data.table merge.data.table
#' @importFrom utils read.csv write.csv write.table download.file unzip

ndc_file_dir <- "extdata/ndctext"

getMissingNDCPath <- function() system.file(ndc_file_dir, "missing_ndcs.csv", package = "MIMER", mustWork = TRUE)
getCombinedkeyPath <- function() system.file("extdata", "combined_key.csv", package = "MIMER", mustWork = TRUE)

missing_ndcs <- read.csv(getMissingNDCPath(),
                         colClasses = 'character')

all_relevant_classes <- c("antimicrobial",
                          "antibacterial",
                          "antifungal",
                          "antiviral",
                          "antimalarial",
                          "antiprotozoal")

antibacterial_classes <- c("antimicrobial",
                           "antibacterial")

relevant_routes_administration <- c("PO/NG",
                                    "IV",
                                    "NG",
                                    "IM",
                                    "IV DRIP",
                                    "PR",
                                    "ORAL",
                                    "IVPCA",
                                    "IV BOLUS",
                                    "DIALYS")

ndc_get_format <- function(s) {
  ifelse(nchar(s[[3]]) == 1,
         "541",
         ifelse(nchar(s[[2]]) == 3,
                "532",
                "442"))
}

convert_split_ndc <- function(split_code, format) {
  if (format == "541") {
    part1 <- split_code[[1]]
    part2 <- split_code[[2]]
    part3 <- paste0("0", split_code[[3]])
    return(paste0(part1, part2, part3, collapse = ""))
  }

  if (format == "442") {
    part1 <- paste0("0", split_code[[1]])
    part2 <- split_code[[2]]
    part3 <- split_code[[3]]
    return(paste0(part1, part2, part3, collapse = ""))
  }

  if (format == "532") {
    part1 <- split_code[[1]]
    part2 <- paste0("0", split_code[[2]])
    part3 <- split_code[[3]]
    return(paste0(part1, part2, part3, collapse = ""))
  }
}

convert_ndc_10_to_11 <- function(code) {
  split_code <- strsplit(code, "-")[[1]]
  code_nchar <- nchar(paste(split_code, collapse = ""))
  if (code_nchar != 10) return(NA)
  format <- ndc_get_format(split_code)
  return(convert_split_ndc(split_code, format))
}

write_to_file <- function (df, file_path){
  write.table(df, file = file_path,row.names = FALSE,append= FALSE, sep = ",",col.names = TRUE)
}

load_combined_key <- function(include_missing_ndcs,
                              re_calculate_combined_key = FALSE){
  combined_key=NULL
  if(re_calculate_combined_key){

      getProductPath <- function() system.file(ndc_file_dir, "product.csv", package = "MIMER", mustWork = TRUE)
      getPackagePath <- function() system.file(ndc_file_dir, "package.csv", package = "MIMER", mustWork = TRUE)

      product <- read.csv(getProductPath(),
                          colClasses = 'character')
      package <- read.csv(getPackagePath(),
                          colClasses = 'character')

      convert_ndc_10_to_11 <- Vectorize(convert_ndc_10_to_11, USE.NAMES = F)

      package$NDC_11 <- convert_ndc_10_to_11(package$NDCPACKAGECODE)

      combined_key <- package[,c("PRODUCTNDC", "NDC_11", "NDCPACKAGECODE")]
      combined_key <- base::merge(combined_key, product, by = "PRODUCTNDC",
                            all.x = TRUE)
      combined_key <- combined_key[!duplicated(combined_key$NDC_11),]
      combined_key <- data.table(combined_key)
    }else{
      tryCatch({
        combined_key <- data.table(read.csv(getCombinedkeyPath(),
                                            colClasses = 'character',
                                            header = TRUE))
      }, error = function(e) {
        message("File is not loaded. Please try with re_calculate_combined_key=TRUE parameter")
      })
    }

  # add missing NDCs (manually generated csv)
  if(include_missing_ndcs) {
    missing <- data.table(missing_ndcs)
    missing$NDC_11 <- stringr::str_pad(missing$NDC_11,
                                       width = 11,
                                       side = "left",
                                       pad = "0")

    # prefer missing NDCs table if present
    combined_key <- data.table::rbindlist(list(missing, combined_key),
                              fill = TRUE, idcol = "priority")
    combined_key <- unique(combined_key, by = "NDC_11")
    combined_key[,"priority":=NULL]
  }

  return(combined_key)
}


#' ndc_to_antimicrobial
#' @title Convert 'ndc' code to corresponding Antibiotic code.
#' @description
#'  Function to convert 'ndc' code to corresponding Antibiotic code.
#' @usage ndc_to_antimicrobial(ndc,
#'  class_names,
#'  include_missing_NDCs = TRUE)
#' @param ndc A vector containing ndc codes. Will be coerced to character.
#' @param class_names A vector containing antibacterial class names - eg: c("antimicrobial", "antibacterial").
#' @param include_missing_NDCs includes a hardcoded database of NDCs that are present in MIMIC-IV but not in NDC database.
#' @return Vector of antimicrobials in antibiotic class from AMR package.
#'
#' @export
ndc_to_antimicrobial <- function(ndc,
                                 class_names = c("antimicrobial",
                                                 "antibacterial"),
                                 include_missing_NDCs = TRUE) {
  #Combined Key
  ndc_char <- as.character(ndc)

  combined_key <- load_combined_key(include_missing_ndcs=include_missing_NDCs)

  data <- data.table(ndc=as.character(ndc))
  data$ndc <- stringr::str_pad(data$ndc,
                               width=11,
                               side = "left",
                               pad = "0")
  data.table::setnames(data, "ndc", "NDC_11")
  data2 <- merge.data.table(data, combined_key, by = "NDC_11", all.x = TRUE, sort = FALSE)
  abx_names <- ifelse(grepl(paste(class_names, collapse = "|"),
                              data2$PHARM_CLASSES,
                              ignore.case=TRUE),
                      data2$SUBSTANCENAME,
                      NA)
  abx_names <- AMR::as.ab(abx_names)
  return(abx_names)
}

#' ndc_is_antimicrobial
#'
#' #' Note: Certain medications, such as Rifaximin, may not be considered antibiotics
#' for all purposes despite their classification. Specific cases should be handled
#' based on context and requirements.
#'
#' @title Check 'ndc' code is belongs to any Antimicrobial.
#' @description
#'  Function to check input 'ndc' code is belongs to any Antimicrobial or not.
#' @usage ndc_is_antimicrobial(ndc,
#'  class_names,
#'  include_missing_NDCs = TRUE)
#' @param ndc A vector containing ndc codes. Will be coerced to character vector.
#' @param class_names A vector  containing antibacterial classes
#'  - eg: c("antimicrobial", "antibacterial")
#' @param include_missing_NDCs includes a hardcoded database of NDCs that are present in MIMIC-IV but not in NDC database.
#' @return Boolean vector for whether input ndc code corresponds to an antimicrobial
#'
#' @export
ndc_is_antimicrobial <- function(ndc,
                                 class_names = c("antimicrobial",
                                                 "antibacterial"),
                                 include_missing_NDCs = TRUE) {
  #Combined Key
  combined_key <- load_combined_key(include_missing_ndcs=include_missing_NDCs)

  data <- data.table(ndc=as.character(ndc))
  data$ndc <- stringr::str_pad(data$ndc,
                               width=11,
                               side = "left",
                               pad = "0")
  data.table::setnames(data, "ndc", "NDC_11")

  data2 <- merge.data.table(data, combined_key, by = "NDC_11", all.x = TRUE, sort = FALSE)
  is_abx <-  grepl(paste(class_names, collapse = "|"),
                     data2$PHARM_CLASSES,
                     ignore.case=TRUE)
  return(is_abx)
}

#' is_systemic_route
#' @title Check 'route' is systemic or not
#' @description
#'  Function to check 'route' is Systemic or not.
#' @usage is_systemic_route(route, class_names)
#' @param route A vector containing route code.
#' @param class_names A vector containing relevant_routes_administration class
#' - Eg: PO/NG
#' @return Boolean
#'
#' @export
is_systemic_route <- function(route, class_names = relevant_routes_administration) {

  is_systemic_route <-  grepl(paste(class_names, collapse = "|"),
                              route,
                   ignore.case=TRUE)
  return(is_systemic_route)
}

#Call this manually to create combined csv file using latest file from NDC website.
manual_update_combined_ndc_file <- function(all_relevant_classes,
                                            re_calculate_combined_key= TRUE,
                                            zip_file_url="https://www.accessdata.fda.gov/cder/ndctext.zip",
                                            zip_file_name="ndctext.zip",
                                            extract_dir = "inst/extdata/ndctext") {
  download_ndc_files_as_csv(zip_file_url,
                            zip_file_name,
                            extract_dir)

  combined <- load_combined_key(include_missing_ndcs = T,
                                re_calculate_combined_key)

  subset(combined,
         grepl(paste(all_relevant_classes, collapse = "|"),
               combined$PHARM_CLASSES, ignore.case = T)) |>
    write.csv(file.path(getwd(), "inst/extdata", "combined_key.csv"))

  remove_files(extract_dir, c("product.csv","package.csv"))
}

#' Download and Convert NDC Files to CSV - To be used in the Manual Build Process
#'
#' This function downloads a zip file containing NDC (National Drug Code) text files from a specified URL,
#' extracts the files to a directory, converts them to CSV format, and optionally deletes the original text files.
#'
#' @param zip_file_url The URL of the zip file containing NDC text files to download.
#' @param zip_file_name The name of the downloaded zip file.
#' @param extract_dir The directory where the NDC text files will be extracted.
#'
download_ndc_files_as_csv <-  function( zip_file_url="https://www.accessdata.fda.gov/cder/ndctext.zip",
                                  zip_file_name="ndctext.zip",
                                  extract_dir = "inst/extdata/ndctext"){
  download.file(zip_file_url,
                zip_file_name)

  unzip(zip_file_name,
        exdir = extract_dir)

  file.remove(zip_file_name)

  convert_all_directory_files_as_csv(extract_dir,
                                     delete_text_files= TRUE)
}

#' Convert All Directory Files to CSV - To be used in the Manual Build Process
#'
#' This function converts all text files in a directory to CSV format.
#'
#' @param extract_dir The directory containing the text files to be converted.
#' @param delete_text_files Logical indicating whether to delete the original text files after conversion.
#'
convert_all_directory_files_as_csv <- function(extract_dir, delete_text_files=FALSE){
  extracted_files <- list.files(extract_dir, pattern = "\\.txt$", full.names = TRUE)
  # Convert the extracted text files to CSV format
  for (file in extracted_files) {
    # Read the text file
    data <- data.table::fread(file)

    # Define the name for the CSV file
    csv_file <- sub("\\.txt$", ".csv", file)

    # Write the data to a CSV file
    data.table::fwrite(data, csv_file)

    if(delete_text_files){
      file.remove(file)
    }
  }

}

#' Remove Files - To be used in the Manual Build Process
#'
#' This function removes files specified by their filenames from a given directory.
#'
#' @param extract_dir The directory from which files will be removed.
#' @param filenames A character vector containing the names of the files to be removed.
#'
remove_files <- function(extract_dir, filenames){
  for (file_name in filenames) {
    file_path <- file.path(extract_dir, file_name)
    if (file.exists(file_path)) {
      file.remove(file_path)
      cat("File", file_path, "removed successfully.\n")
    } else {
      cat("File", file_path, "does not exist.\n")
    }
  }
}
