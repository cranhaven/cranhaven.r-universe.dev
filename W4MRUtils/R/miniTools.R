#' @title Mini tools for Galaxy scripting
#'
#' @name mini_tools
#'
#' @description Mini tools for Galaxy scripting
#' Mini tools for Galaxy scripting
#' Coded by: M.Petera,
#'
#' R functions to use in R scripts and wrappers
#' to make things easier (lightening code, reducing verbose...)
#'
#' V0: script structure + first functions
#' V1: addition of functions to handle special characters in identifiers
#'
NULL

#' @title source_local - source file, from absolute or relative path
#'
#' @description source_local
#' Transforms a relative path to an absolute one, and sources the path.
#' This helps source files located relatively to the main script without
#' the need to know from where it was run.
#' @param ... paths, character vector of file paths to source
#' @param env an environement in which to source the paths
#' @param do_print a logical, telling whether to print sourced paths or
#'   not
#' @param keep_source See the parameter keep.source from source
#' @return a vector resulting from the sourcing of the files provided.
#'
#' @examples
#' ## let's say we have some R file with the following content:
#' file_1_content <- "
#'   setup_logger <- function(args, logger) {
#'     if (!is.null(args$verbose) && args$verbose) {
#'       logger$set_verbose(TRUE)
#'     }
#'     if (!is.null(args$debug) && args$debug) {
#'       logger$set_debug(TRUE)
#'     }
#'     if (!is.null(args$logs)) {
#'       logger$add_out_paths(args$logs)
#'     }
#'   }"
#' file_2_content <- "
#'   processing <- function(args, logger) {
#'     logger$info(\"The tool is working...\")
#'     logger$infof(
#'       \"Parameters: %s\",
#'       paste(capture.output(str(args)), collapse = \"\n\")
#'     )
#'     logger$info(\"The tool ended fine.\")
#'     return(invisible(NULL))
#'   }"
#'
#' if(!file.create(temp_path <- tempfile(fileext = ".R"))) {
#'   stop("This documentation is not terminated doe to unknown error")
#' }
#' writeLines(file_1_content, con = temp_path)
#'
#' local_path = "test-local-path.R"
#' local_full_path = file.path(get_base_dir(), local_path)
#' if(!file.create(local_full_path)) {
#'   stop("This documentation is not terminated doe to unknown error")
#' }
#' writeLines(file_2_content, con = local_full_path)
#'
#' ## now when we source them, the absolute path is sourced, and the
#' ## relative file path is sourced too.
#' W4MRUtils::source_local(c(temp_path, local_path), do_print = TRUE)
#' file.remove(local_full_path)
#'
#' ## the function is accessible here
#' processing(list(), get_logger("Tool Name"))
#'
#' @seealso [source()]
#'
#' @export
source_local <- function(
  ...,
  env = FALSE,
  do_print = FALSE,
  keep_source = TRUE
) {
  do_source <- function(path) {
    if (do_print) {
      printf("Sourcing %s", path)
    }
    base::source(path, local = env, keep.source = keep_source)
  }
  base_dir <- get_base_dir()
  files <- c(...)
  absolute_filter <- grepl("^/|[a-zA-Z]+:\\\\", files, perl = TRUE)
  non_absolutes <- file.path(base_dir, files[!absolute_filter])
  result <- lapply(
    c(files[absolute_filter], non_absolutes),
    do_source
  )
  return(invisible(result))
}

#' @title get_base_dir - to get... the base directory
#'
#' @description get_base_dir
#' @return the directory path of the main script. PWD otherwise.
#'
#' @examples
#' print(get_base_dir())
#'
#' @export
get_base_dir <- function() {
  argv <- commandArgs(trailingOnly = FALSE)
  base_dir <- dirname(substring(argv[grep("^--file=", argv, perl = TRUE)], 8))
  if (is.null(base_dir) || length(base_dir) == 0 || base_dir == ".") {
    base_dir <- dirname(argv[grep("^-f$", argv, perl = TRUE) + 1])
  }
  if (is.null(base_dir) || length(base_dir) == 0 || base_dir == ".") {
    base_dir <- getwd()
  }
  return(base_dir)
}

#' @title Shy Lib
#'
#' @description shy_lib
#' Function to call packages without printing all the verbose
#' (only getting the essentials, like warning messages for example)
#'
#' @param ... Name of libraries to load
#' @return a \code{list} of attached packages
#'
#' @examples
#' \donttest{
#'    W4MRUtils::shy_lib("base", "utils")
#' }
#'
#' @author M.Petera
#'
#' @export
shy_lib <- function(...) {
  lapply(
    c(...),
    function(package) {
      suppressPackageStartupMessages(library(package, character.only = TRUE))
    }
  )
}

#'
#' @title Parse Command arguments
#'
#' @description parse_args
#'  Replacement for the parseCommandArgs utility from batch.
#'  Note that inputs like `script.R some-list c(1, 2, 3)` will result in
#'  args$`some-list` to be the string "c(1, 2, 3)", and not a vector anymore
#'  as this ability was permitted by dangerous behaviours from the
#'  batch package (the usage of `eval` which MUST NEVER be used on user's
#'  inputs).
#'
#'  To get a list of numeric from users, instead of using the `c(1, 2)` trick,
#'  please, use regular lists parsing:
#'
#'  ```
#'  > args$`some-list`
#'  [1] "1,2"
#'  args$`some-list` <- as.numeric(strsplit(args$`some-list`, ",")[[1]])
#'  > args$`some-list`
#'  [1] 1 2
#'  ```
#'
#' @param args optional, provide arguments to parse.
#'  This function will use 'commandArgs()' if args is not provided
#' @param convert_booleans logical - tells the function to convert
#'  values into logical if their value is "TRUE" or "FALSE".
#' @param convert_numerics logical - tells the function to convert
#'  values into numeric if possible.
#' @param strip_trailing_dash - tells whether to remove trailing hyphens from
#'   the start of the parameter name
#' @param replace_dashes - tells whether to turn trailing hyphens into
#'   underscores
#' @return a named \code{list} object containing the input parameters in values
#'  and the parameters names in names
#'
#' @author L.Pavot
#' @examples
#' ## faking command line parameters:
#'
#' commandArgs <- function() {
#'   list(
#'     "--args",
#'     "param1", "a value",
#'     "param2", "42"
#'   )
#' }
#'
#' ## extracting command line parameters:
#' parameters <- W4MRUtils::parse_args(args = commandArgs())
#' str(parameters)
#'
#' @export
parse_args <- function(
  args = NULL,
  convert_booleans = TRUE,
  convert_numerics = TRUE,
  strip_trailing_dash = TRUE,
  replace_dashes = TRUE
) {
  warning(
    "Please, use the 'optparse' library instead of the 'parse_args' function."
  )
  if (is.null(args)) {
    args <- commandArgs()
  }
  start <- which(args == "--args")[1] + 1
  if (is.na(start)) {
    return(list())
  }
  seq_by2 <- seq(start, length(args), by = 2)
  result <- as.list(args[seq_by2 + 1])
  names(result) <- args[seq_by2]
  if (strip_trailing_dash) {
    names(result) <- gsub("^-+", "", names(result), perl = TRUE)
  }
  if (replace_dashes) {
    names(result) <- gsub("-", "_", names(result))
  }
  converters <- c()
  if (convert_booleans) {
    converters <- c(
      converters,
      function(x) {
        return(if (x == "TRUE") TRUE else if (x == "FALSE") FALSE else x)
      }
    )
  }
  if (convert_numerics) {
    converters <- c(
      converters,
      function(x) {
        return(if (is.na(y <- as.numeric(x))) x else y)
      }
    )
  }
  return(convert_parameters(result, converters))
}

#'
#' @title Convert Parameters
#'
#' @description convert_parameters
#'  Applies a list of converters to each values on a list.
#'  If a value is modified during the conversion (successfull conversion)
#'  then, no further convert will be applied to this value, so values are
#'  only converted once.
#'
#' @param args a named list, which values will be converted.
#' @param converters a vector of function. Each function will be applied to
#'  each values with the exception of values already converted by a
#'  previous converter.
#' @return a named \code{list} object with values converted by converters.
#'
#' @author L.Pavot
#' @examples
#' boolean_converter <- function(x) {
#'   return(if (x == "TRUE") TRUE else if (x == "FALSE") FALSE else x)
#' }
#' parameters <- W4MRUtils::convert_parameters(
#'   list("x" = "TRUE"),
#'   c(boolean_converter)
#' )
#' print(parameters$`some-parameter`)
#' ## "TRUE" has becomes TRUE.
#'
#' @export
convert_parameters <- function(args, converters) {
  suppressWarnings(
    for (param in names(args)) {
      for (converter in converters) {
        old_value <- args[[param]]
        args[[param]] <- converter(args[[param]])
        if (!identical(args[[param]], old_value)) {
          ## The value has been modified by the converter, and
          ## we don't want values to be converted multiple times,
          ## so we pass to the next value.
          break
        }
      }
    }
  )
  return(args)
}

#' @title Stock ID
#'
#' @description stock_id
#' Functions to stock identifiers before applying make.names() and
#' to reinject it into final matrices.
#' stock_id stocks original identifiers and original order
#' needs checked data regarding table match.
#'
#' @param data_matrix a \code{data.frame} containing the data_matrix
#' @param metadata a \code{data.frame} containing samplemetadata or
#'  variablemetadata
#' @param metadata_type "sample" or "variable" depending on metadata content
#' @return a names \code{list} with three elements:
#'  - id.match a \code{data.frame} that contains original order of ids, names ;
#'  - dataMatrix the modified data matrix with names sanitized
#'  - Metadata the modified metadata matrix with names sanitized
#' This object can be used in reproduce_id() to replace sanitized names in data
#' matrix by original ones, in the right order.
#'
#' @examples
#' \donttest{
#'
#' myDM <- data.frame(data = 1:6, a = 2:7, b = 3:8, c = 2:7, d = 3:8, e = 2:7)
#' myvM <- data.frame(variable = 1:6, x = 4:9, y = 2:7, z = 3:8)
#'
#' A <- W4MRUtils::stock_id(myDM, myvM, "variable")
#' myDM <- A$dataMatrix
#' myvM <- A$Metadata
#' A <- A$id.match
#'
#' ## processing that may change order or requires specific identifiers format
#' ## ...
#' datamatrix <- as.data.frame(myDM)
#' sample_metadata <- as.data.frame(myvM)
#'
#' B <- W4MRUtils::reproduce_id(datamatrix, sample_metadata, "variable", A)
#' datamatrix <- B$dataMatrix
#' sample_metadata <- B$Metadata
#' }
#'
#' @author M.Petera
#'
#' @export
stock_id <- function(data_matrix, metadata, metadata_type) {
  # data_matrix = data.frame containing data_matrix
  # metadata = data.frame containing samplemetadata or variablemetadata
  # metadata_type = "sample" or "variable" depending on metadata content
  cname <- colnames(data_matrix)[1]
  # data_matrix temporary-stock + transfo - - - -
  if (metadata_type == "sample") {
    id_ori <- colnames(data_matrix)[-1]
    colnames(data_matrix) <- make.names(colnames(data_matrix))
  }
  if (metadata_type == "variable") {
    id_ori <- data_matrix[, 1]
    data_matrix[, 1] <- make.names(data_matrix[, 1])
  }
  # global stock - - - - - - - - - - - - - - - -
  id_new <- data.frame(
    order.ori = seq_along(metadata[, 1]), metadata[, 1],
    id.new = make.names(metadata[, 1]), id_ori,
    id.new.DM = make.names(id_ori), stringsAsFactors = FALSE
  )
  colnames(id_new)[c(2, 4)] <- c(colnames(metadata)[1], cname)
  # metadata transfo + returning data - - - - -
  metadata[, 1] <- make.names(metadata[, 1])
  return(list(
    id.match = id_new,
    dataMatrix = data_matrix,
    Metadata = metadata
  ))
}

#' @title Reproduce ID
#'
#' @description reproduce_id
#' reproduce_id() reinjects original identifiers and original order into
#'  final tables
#'
#' @param data_matrix data.frame containing data_matrix
#' @param metadata data.frame containing samplemetadata or variablemetadata
#' @param metadata_type "sample" or "variable" depending on metadata content
#' @param id_match 'id_match' element produced by stock_id
#' @return a named \code{list} with two elements:
#'  data_matrix: the processed data matrix with its original names and order
#'  metadata: the processed metadata, with its original names and order.
#' @inherit stock_id examples
#'
#' @author M.Petera
#'
#' @export
reproduce_id <- function(data_matrix, metadata, metadata_type, id_match) {
  # Metadada - - - - - - - - - - - - - -
  temp_table <- id_match[, c(1, 2, 3)]
  ## Removing deleted rows
  for (i in 1:(dim(id_match)[1])) {
    if (!(temp_table[i, 3] %in% metadata[, 1])) {
      temp_table[i, 1] <- 0
    }
  }
  if (length(which(temp_table[, 1] == 0)) != 0) {
    temp_table <- temp_table[-c(which(temp_table[, 1] == 0)), ]
  }
  ## Restoring original identifiers and order
  temp_table <- merge(x = temp_table, y = metadata, by.x = 3, by.y = 1)
  temp_table <- temp_table[order(temp_table$order.ori), ]
  metadata <- temp_table[, -c(1, 2)]
  rownames(metadata) <- NULL
  # data_matrix - - - - - - - - - - - - -
  rownames(data_matrix) <- data_matrix[, 1]
  if (metadata_type == "sample") {
    data_matrix <- t(data_matrix[, -1])
  }
  temp_table <- id_match[, c(1, 4, 5)]
  ## Removing deleted rows
  for (i in seq_len(dim(id_match)[1])) {
    if (!(temp_table[i, 3] %in% rownames(data_matrix))) {
      temp_table[i, 1] <- 0
    }
  }
  if (length(which(temp_table[, 1] == 0)) != 0) {
    temp_table <- temp_table[-c(which(temp_table[, 1] == 0)), ]
  }
  ## Restoring original identifiers and order
  temp_table <- merge(x = temp_table, y = data_matrix, by.x = 3, by.y = 0)
  temp_table <- temp_table[order(temp_table$order.ori), ]
  if (metadata_type == "variable") {
    data_matrix <- temp_table[, -c(1, 2, 4)]
    colnames(data_matrix)[1] <- colnames(id_match)[4]
  } else {
    rownames(temp_table) <- temp_table[, 3]
    temp_table <- t(temp_table[, -c(1, 2, 3)])
    data_matrix <- data.frame(
      rownames(temp_table),
      temp_table,
      check.names = FALSE
    )
    colnames(data_matrix)[1] <- colnames(id_match)[4]
  }
  rownames(data_matrix) <- NULL
  # return datasets - - - - - - - - - - -
  return(list(dataMatrix = data_matrix, Metadata = metadata))
}

#' @title Import two W4M tables
#'
#' @description import2
#' Function to import a metadata table file and its corresponding
#' dataMatrix file.
#' import2 performs checks to ensure the identifiers match between
#' the two tables and stops with an explicit error message in case
#' identifiers do not match.
#'
#' @param pathDM a path to a file corresponding to the dataMatrix
#' @param pathMeta a path to a file corresponding to the metadata table
#' @param typeMeta "sample" or "variable" depending on the metadata content
#' @param disable_comm a \code{boolean} with default to \code{TRUE} to indicate
#' whether the comment character \code{#} should be disabled as a comment tag
#' for the import of the metadata file; when \code{TRUE}, \code{#} in the
#' metadata table's columns will be considered as any other character.
#' @return a \code{list} containing two elements:
#'  - dataMatrix a \code{data.frame} corresponding to the imported dataMatrix table;
#'  - metadata a \code{data.frame} corresponding to the imported metadata table
#'
#' @examples
#' \donttest{
#'
#' dm_path <- system.file(
#'   "extdata",
#'   "mini_datamatrix.txt",
#'   package="W4MRUtils"
#' )
#' meta_path <- system.file(
#'   "extdata",
#'   "mini_variablemetadata.txt",
#'   package="W4MRUtils"
#' )
#'
#' ## import considering # is not a comment character
#' A <- W4MRUtils::import2(dm_path, meta_path, "variable")
#' print(A$dataMatrix[1:5, 1:5])
#' print(A$metadata[1:5, ])
#'
#' ## import considering # is a comment character
#' B <- W4MRUtils::import2(dm_path, meta_path, "variable", disable_comm = FALSE)
#' print(B$dataMatrix[1:5, 1:5])
#' print(B$metadata[1:5, ])
#' }
#'
#' @author M.Petera
#'
#' @export
import2 <- function(pathDM, pathMeta, typeMeta, disable_comm = TRUE){
  input_check <- c(
    ifelse(is.character(pathDM), "", "/!\\ The input dataMatrix path parameter is not a character string.\n"),
    ifelse(is.character(pathMeta), "", "/!\\ The input metadata file path parameter is not a character string.\n"),
    ifelse(typeMeta %in% c("sample", "variable"), "", "/!\\ The input metadata type parameter is not one of 'sample' and 'variable'.\n"),
    ifelse(is.logical(disable_comm), "", "/!\\ The input disable_comm parameter is not one of 'TRUE' and 'FALSE'. \n"))
  if(sum(input_check == "") != 4) {
    W4MRUtils::check_err(input_check)
  }
  comm_option <- ifelse(disable_comm, "", "#")
  # Table import
  DM <- read.table(pathDM, header = TRUE, sep = "\t", check.names = FALSE)
  meta <- read.table(pathMeta, header = TRUE, sep = "\t", check.names = FALSE, comment.char = comm_option)
  # Table match check
  table_check <- W4MRUtils::match2(DM, meta, typeMeta)
  W4MRUtils::check_err(table_check)
  # Return
  return(list(dataMatrix = DM, metadata = meta))
}

#' @title Import the three W4M tables
#'
#' @description import3
#' Function to import the three W4M tables from files
#' (dataMatrix, sampleMetadata, variableMetadata)
#' import3 performs checks to ensure the identifiers match between
#' the three tables and stops with an explicit error message in case
#' identifiers do not match.
#'
#' @param pathDM a path to a file corresponding to the dataMatrix
#' @param pathSM a path to a file corresponding to the sampleMetadata
#' @param pathVM a path to a file corresponding to the variableMetadata
#' @param disable_comm a \code{boolean} with default to \code{TRUE} to indicate
#' whether the comment character \code{#} should be disabled as a comment tag
#' for the import of the metadata files; when \code{TRUE}, \code{#} in the
#' metadata table's columns will be considered as any other character.
#' @return a \code{list} containing three elements:
#'  - dataMatrix a \code{data.frame} corresponding to the imported dataMatrix table;
#'  - sampleMetadata a \code{data.frame} corresponding to the imported sampleMetadata table;
#'  - variableMetadata a \code{data.frame} corresponding to the imported variableMetadata table
#'
#' @examples
#' \donttest{
#'
#' dm_path <- system.file(
#'   "extdata",
#'   "mini_datamatrix.txt",
#'   package="W4MRUtils"
#' )
#' vm_path <- system.file(
#'   "extdata",
#'   "mini_variablemetadata.txt",
#'   package="W4MRUtils"
#' )
#' sm_path <- system.file(
#'   "extdata",
#'   "mini_samplemetadata.txt",
#'   package="W4MRUtils"
#' )
#'
#' ## import considering # is not a comment character
#' A <- W4MRUtils::import3(dm_path, sm_path, vm_path)
#' print(A$dataMatrix[1:5, 1:5])
#' print(A$sampleMetadata[1:5, ])
#' print(A$variableMetadata[1:5, ])
#'
#' ## import considering # is a comment character
#' B <- W4MRUtils::import3(dm_path, sm_path, vm_path, disable_comm = FALSE)
#' print(B$dataMatrix[1:5, 1:5])
#' print(B$sampleMetadata[1:5, ])
#' print(B$variableMetadata[1:5, ])
#' }
#'
#' @author M.Petera
#'
#' @export
import3 <- function(pathDM, pathSM, pathVM, disable_comm = TRUE){
  input_check <- c(
    ifelse(is.character(pathDM), "", "/!\\ The input dataMatrix path parameter is not a character string.\n"),
    ifelse(is.character(pathSM), "", "/!\\ The input sampleMetadata file path parameter is not a character string.\n"),
    ifelse(is.character(pathVM), "", "/!\\ The input variableMetadata file path parameter is not a character string.\n"),
    ifelse(is.logical(disable_comm), "", "/!\\ The input disable_comm parameter is not one of 'TRUE' and 'FALSE'. \n"))
  if(sum(input_check == "") != 4) {
    W4MRUtils::check_err(input_check)
  }
  comm_option <- ifelse(disable_comm, "", "#")
  # Table import
  DM <- read.table(pathDM, header = TRUE, sep = "\t", check.names = FALSE)
  VM <- read.table(pathVM, header = TRUE, sep = "\t", check.names = FALSE, comment.char = comm_option)
  SM <- read.table(pathSM, header = TRUE, sep = "\t", check.names = FALSE, comment.char = comm_option)
  # Table match check
  table_check <- W4MRUtils::match3(DM, SM, VM)
  W4MRUtils::check_err(table_check)
  # Return
  return(list(dataMatrix = DM, sampleMetadata = SM, variableMetadata = VM))
}
