#' @title Read multiple text files from photometer measurement 
#' @description \code{read_plates} reads raw text files generated from photometer measurements of
#' 96-well plates. The data is returned as a list but without additional first lines that are sometimes
#' used to provide additional information, for example, wavelength used or the date of measurement.
#' For comparison, this information is saved as an attribute of the list and can be retrieved
#' via the \code{"info"} parameter.
#' @param input_data Either a folder path containing raw data files or a list of data frames.
#' @param pattern A character value providing the file pattern to search for. If not provided,
#' defaults to \code{"^BMA|bma"}. 
#' @param skip_lines A numeric value of lines to skip until data is provided in text file. Defaults to 2.
#' @return \code{read_plates} returns a list of data frames containing the raw photometer data.
#' @importFrom tools file_path_sans_ext
#' @export
read_plates <- function(input_data,
                        pattern = NULL,
                        skip_lines = 2) {
  
  # Check pattern to read data
  if (is.character(input_data)) {
    # Check pattern matching
    if (is.null(pattern)) {
      pattern = "^BMA|bma"
    } else {
      pattern = pattern
    }
    
    # If input_data is folder path, then read contained files into list of data frames
    file_list <- list.files(path = input_data,
                            pattern = pattern,
                            full.names = TRUE)

    # Read files and save attributes
    raw_data_list <- list()
    attributes_data <- data.frame(File_name = character(),
                                  Attribute = character())
    for (file in file_list) {
      plate_data <- read_plate(file, skip_lines = skip_lines)
      file_name <- file_path_sans_ext(basename(file))
      data_attribute <- attr(plate_data, "info")
      
      raw_data_list[[file_name]] <- plate_data
      # Append file name and its attribute to attributes_data
      attributes_data <- rbind(attributes_data,
                               data.frame(File_name = file_name,
                                          Attribute = data_attribute))
    }
    
    # Set the attribute data frame to the whole list
    attr(raw_data_list, "info") <- attributes_data
    
  } else if (is.list(input_data) && all(sapply(input_data, inherits, "data.frame"))) {
    # If input_data is list of data frames, process each data frame separately
    raw_data_list <- input_data
  } else {
    stop("Input must be either a folder path to photometer files or a list of named data frames with 8 rows and 12 columns.")
  }
  return(raw_data_list)
}


#' @title Read single text file from photometer measurement 
#' @description \code{read_plate} reads a raw text file generated from a photometer measurement of a
#' 96-well plate. The data is returned as is but without additional first lines that are sometimes
#' used to provide additional information, for example, wavelength used or the date of measurement.
#' For comparison, this information is saved as an attribute of the raw data and can be retrieved
#' via the \code{"info"} parameter.
#' @param file_path The file path to the file containing the raw data.
#' @param skip_lines A numerical value that specifies the number of lines to be skipped until data is
#' provided. These lines will be saved as an attribute and are accessible via the \code{"info"} parameter.
#' Defaults to 2.
#' @return \code{read_plate} returns a data frame containing the raw photometer data
#' @importFrom utils read.table
#' @rdname read_plates
#' @export
read_plate <- function(file_path,
                       skip_lines = 2) {
  plate_data <- read.table(file_path, check.names = FALSE, skip = skip_lines, header = TRUE)
  info <- readLines(file_path, n = skip_lines)
  attr(plate_data, "info") <- info[nzchar(info)]
  return(plate_data)
}
