#' Convert a document (.docx, .pdf, .odt, .rtf, or .html) to a plain text file
#'
#' This used to be a thin wrapper around `textreadr::read_document()` that also
#' writes the result to `output`, doing its best to correctly write UTF-8
#' (based on the approach recommended in [this blog post](
#' https://tomizonor.wordpress.com/2013/04/17/file-utf8-windows/)). However,
#' `textreadr` was archived from CRAN. It now directly wraps the functions
#' that `textreadr` wraps: `pdftools::pdf_text()`, `striprtf::read_rtf`, and
#' it uses `xml2` to import `.docx` and `.odt` files, and `rvest` to import
#' `.html` files, using the code from the `textreadr` package.
#'
#' @param input The path to the input file.
#' @param output The path and filename to write to. If this is a path to
#' an existing directory (without a filename specified), the `input` filename
#' will be used, and the extension will be replaced with `extension`.
#' @param encoding The encoding to use when writing the text file.
#' @param newExt The extension to append: only used if `output = NULL` and
#' `newExt` is not `NULL`, in which case the output will be written to a file
#' with the same name as `input` but with `newExt` as extension.
#' @param preventOverwriting Whether to prevent overwriting existing files.
#' @param silent Whether to the silent or chatty.
# #' @param skip The number of lines to skip (see [textreadr::read_document()]).
# #' @param remove.empty If `TRUE` empty elements in the vector are
# #' removed (see [textreadr::read_document()]).
# #' @param trim If `TRUE` the leading/training white space is
# #' removed (see [textreadr::read_document()]).
# #' @param combine If `TRUE` the vector is concatenated into a single string
# #' `textshape::combine()`. (see [textreadr::read_document()]).
# #' @param format For .doc files only. Logical. If `TRUE` the output will keep
# #' doc formatting (e.g., bold, italics, underlined). This corresponds to
# #' the `-f` flag in antiword (see [textreadr::read_document()]).
# #' @param ocr If `TRUE` .pdf documents with a non-text pull using
# #' `pdftools::pdf_text()` will be re-run using OCR via the `tesseract::ocr()`
# #' function. This will create temporary .png files and will require a much
# #' larger compute time (see [textreadr::read_document()]).
# #' @param ... Other arguments passed to [textreadr::read_pdf()],
# #' [textreadr::read_html()], [textreadr::read_docx()], [textreadr::read_doc()],
# #' or [base::readLines()] (by [textreadr::read_document()]).
#'
#' @return The converted source, as a character vector.
#' @export
#'
#' @examples ### This example requires the {xml2} package
#' if (requireNamespace("xml2", quietly = TRUE)) {
#'   print(
#'     rock::doc_to_txt(
#'       input = system.file(
#'         "extdata/doc-to-test.docx", package="rock"
#'       )
#'     )
#'   );
#' }
doc_to_txt <- function(input,
                       output = NULL,
                       encoding = rock::opts$get("encoding"),
                       newExt = NULL,
                       preventOverwriting = rock::opts$get("preventOverwriting"),
                       silent = rock::opts$get("silent")
                       # ,
                       # skip = 0,
                       # remove.empty = TRUE,
                       # trim = TRUE,
                       # combine = FALSE,
                       # format = FALSE,
                       # ocr = TRUE,
                       # ...
                      ) {

  # if (!requireNamespace("textreadr", quietly = TRUE)) {
  #   stop("To use this function, you need the {textreadr} package! ",
  #        "To install it, you can use:\n\n    ",
  #        "install.packages('textreadr');\n\n");
  # }

  msg("Reading input file from '", input, "'.\n",
      silent = silent);

  if (!base::file.exists(input)) {
    stop("The file you specified to read, '",
         input, "', does not exist.");
  }

  fileExt <- trimws(tolower(tools::file_ext(input)));

  if (fileExt == "pdf") {

    if (!requireNamespace("pdftools", quietly = TRUE)) {
      stop("To use this function to read PDF files, you need the {pdftools} ",
           "package! To install it, you can use:\n\n    ",
           "install.packages('pdftools');\n\n");
    }

    res <- pdftools::pdf_text(input);

  } else if (fileExt == "docx") {

    ###-------------------------------------------------------------------------
    ### Taken from textreadr::read_docx() by Tyler Rinker:
    ### https://github.com/trinker/textreadr/blob/master/R/read_docx.R
    ###-------------------------------------------------------------------------

    ## create temp dir
    tempDir <- tempfile();

    if (!dir.create(tempDir)) {
      stop("I failed to create temporary directory '", tempDir, "'.");
    }

    ## clean up
    on.exit(unlink(tempDir, recursive=TRUE))

    ## unzip docx
    xmlfile <- file.path(tempDir, "word", "document.xml")
    utils::unzip(input, exdir = tempDir);

    ## read in the unzipped docx
    res <- xml2::read_xml(xmlfile)

    res <- xml2::xml_text(xml2::xml_find_all(res, '//w:p'))

    if (length(res) == 0) res <- ''

  } else if (fileExt == "odt") {

    ###-------------------------------------------------------------------------
    ### Taken from textreadr::read_odt() by Tyler Rinker:
    ### https://github.com/trinker/textreadr/blob/master/R/read_odt.R
    ###-------------------------------------------------------------------------

    ## create temp dir
    tempDir <- tempfile();

    if (!dir.create(tempDir)) {
      stop("I failed to create temporary directory '", tempDir, "'.");
    }

    ## clean up
    on.exit(unlink(tempDir, recursive=TRUE))

    ## unzip docx
    xmlfile <- file.path(tempDir, "word", "content.xml")
    utils::unzip(input, exdir = tempDir);

    ## read in the unzipped docx
    res <- xml2::read_xml(xmlfile)

    res <- xml2::xml_text(xml2::xml_find_all(res, '//text:p'))

    if (length(res) == 0) res <- ''

  } else if (fileExt == "rtf") {

    if (!requireNamespace("striprtf", quietly = TRUE)) {
      stop("To use this function to read RTF files, you need the {striprtf} ",
           "package! To install it, you can use:\n\n    ",
           "install.packages('striprtf');\n\n");
    }

    res <- striprtf::read_rtf(file)

  } else if (fileExt == "html") {

    if (!requireNamespace("rvest", quietly = TRUE)) {
      stop("To use this function to read HTML files, you need the {rvest} ",
           "package! To install it, you can use:\n\n    ",
           "install.packages('rvest');\n\n");
    }

    ## read in the html
    doc <- xml2::read_html(input);

    ## extract the body content
    res <- rvest::html_text(
      rvest::html_nodes(
        doc,
        xpath = "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]"
      )
    );

    if (length(res) == 0) res <- '';

  }

  # res <-
  #   textreadr::read_document(
  #     file = input,
  #     skip = skip,
  #     remove.empty = remove.empty,
  #     trim = trim,
  #     combine = combine,
  #     format = format,
  #     ocr = ocr,
  #     ... = ...
  #   );

  if ((is.null(output)) && (!is.null(newExt))) {
    output <-
      paste0(
        tools::file_path_sans_ext(
          input
        ),
        ".",
        newExt
      );
  } else if ((!is.null(output)) &&
             (dir.exists(output)) &&
             (!is.null(newExt))) {

    output <-
      file.path(
        output,
        paste0(
          basename(
            tools::file_path_sans_ext(
              input
            )
          ),
          ".",
          newExt
        )
      );
  }

  if (!is.null(output)) {

    writingResult <-
      writeTxtFile(
        x = res,
        output = output,
        preventOverwriting = preventOverwriting,
        encoding = encoding,
        silent = silent
      );

    if (!writingResult) {
      warning("Could not write output file to `",
              output, "`.");
    }

  }

  return(invisible(res));

}
