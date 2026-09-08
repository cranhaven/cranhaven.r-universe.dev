#' Split a pdf file into individual pages
#' @export
#' @param input path or url to the input pdf file
#' @param output base path of the output file(s)
#' @param password string with password to open pdf file
#' @examples
#' # extract some pages
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_split(pdf_file, fout, password = "")
#' @return a character vector with the paths of the split pdf files
pdf_split <- function(input, output = NULL, password = ""){
  input <- get_input(input)
  if(!length(output))
    output <- sub("\\.pdf$", "", input)
  cpp_pdf_split(input, output, password)
}

#' Get the number of pages in a pdf file
#' @export
#' @inheritParams pdf_split
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' pdf_length(pdf_file, "")
#' @return an integer value with the number of pages in the pdf file
pdf_length <- function(input, password = ""){
  input <- get_input(input)
  cpp_pdf_length(input, password)
}

#' Subset a pdf file to a new pdf file containing the selected pages
#' @export
#' @inheritParams pdf_split
#' @param pages a vector with page numbers to select. Negative numbers
#' means removing those pages (same as R indexing)
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_subset(pdf_file, 1, fout, "")
#' @return a character vector with the path of the subsetted pdf file
pdf_subset <- function(input, pages = 1, output = NULL, password = ""){
  input <- get_input(input)
  if(!length(output))
    output <- sub("\\.pdf$", "_output.pdf", input)
  output <- normalizePath(output, mustWork = FALSE)
  if(!grepl("\\.pdf$", output)) output <- paste0(output, ".pdf")
  size <- pdf_length(input, password = password)
  pages <- seq_len(size)[pages]
  if(any(is.na(pages)) || !length(pages))
    stop("Selected pages out of range")
  cpp_pdf_select(input, output, pages, password)
}

#' Combine multiple pdf files into a single pdf file
#' @export
#' @inheritParams pdf_split
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_combine(pdf_file, fout, "")
#' @return a character vector with the path of the combined pdf file
pdf_combine <- function(input, output = NULL, password = ""){
  input <- get_input_multi(input)
  if(!length(output))
    output <- sub("\\.pdf$", "_combined.pdf", input[1])
  output <- normalizePath(output, mustWork = FALSE)
  if (!grepl("\\.pdf$", output)) output <- paste0(output, ".pdf")
  cpp_pdf_combine(input, output, password)
}

#' Compress a pdf file
#' @export
#' @inheritParams pdf_split
#' @param linearize enable pdf linearization (streamable pdf)
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_compress(pdf_file, fout, TRUE, "")
#' @return a character vector with the path of the compressed pdf file
pdf_compress <- function(input, output = NULL, linearize = FALSE, password = ""){
  input <- get_input(input)
  if(!length(output))
    output <- sub("\\.pdf$", "_output.pdf", input)
  output <- normalizePath(output, mustWork = FALSE)
  if (!grepl("\\.pdf$", output)) output <- paste0(output, ".pdf")
  cpp_pdf_compress(input, output, linearize, password)
}

#' Overlay a pdf file into another pdf file
#' @export
#' @inheritParams pdf_split
#' @param stamp pdf file of which the first page is overlayed into each page of
#'  input
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' stamp_file <- system.file("examples", "header.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_overlay_stamp(pdf_file, stamp_file, fout, "")
#' @return a character vector with the path of the stamped pdf file
pdf_overlay_stamp <- function(input, stamp, output = NULL, password = ""){
  input <- get_input(input)
  stamp <- get_input(stamp)
  if(!length(output))
    output <- sub("\\.pdf$", "_output.pdf", input)
  output <- normalizePath(output, mustWork = FALSE)
  if (!grepl("\\.pdf$", output)) output <- paste0(output, ".pdf")
  cpp_pdf_overlay(input, stamp, output, password)
}

#' Rotate pages in a pdf file
#' @export
#' @inheritParams pdf_split
#' @param pages a vector with page numbers to rotate
#' @param angle rotation angle in degrees (positive = clockwise)
#' @param relative if `TRUE`, pages are rotated relative to their current
#'  orientation. If `FALSE`, rotation is absolute (0 = portrait, 90 = landscape,
#'  rotated 90 degrees clockwise from portrait)
#' @examples
#' pdf_file <- system.file("examples", "sufganiyot.pdf", package = "cpp11qpdf")
#' fout <- tempfile()
#' pdf_rotate_pages(pdf_file, 1, 90, FALSE, fout, "")
#' @return a character vector with the path of the rotated pdf file
pdf_rotate_pages <- function(input, pages, angle = 90, relative = FALSE, output = NULL, password = ""){
  input <- get_input(input)
  if(!length(output))
    output <- sub("\\.pdf$", "_output.pdf", input)
  output <- normalizePath(output, mustWork = FALSE)
  if (!grepl("\\.pdf$", output)) output <- paste0(output, ".pdf")
  size <- pdf_length(input, password = password)
  pages <- seq_len(size)[pages]
  if(any(is.na(pages)) || !length(pages))
    stop("Selected pages out of range")
  cpp_pdf_rotate_pages(input, output, pages, angle, relative, password)
}

get_input <- function(path){
  if(length(path) != 1)
    stop("input should contain exactly one file")
  if(grepl("^https?://", path)){
    tmp <- file.path(tempdir(), basename(path))
    curl::curl_download(path, tmp)
    path <- tmp
  }
  normalizePath(path, mustWork = TRUE)
}

get_input_multi <- function(path){
  vapply(path, get_input, character(1))
}
