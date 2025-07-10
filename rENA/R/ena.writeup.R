###
#' Calculate the correlations
#'
#' @description Calculate both Spearman and Pearson correlations for the
#' provided ENAset
#'
#' @param enaset ENAset to view methods of
#' @param tool c("rENA","webENA")
#' @param tool.version as.character(packageVersion(tool))
#' @param comparison character string representing the comparison used, c(NULL, "parametric", "non-parametric"). Default NULL
#' @param comparison.groups Groups that were used for the comparison
#' @param sig.dig Integer for the number of digits to round to
#' @param output_dir Where to save the output file
#' @param type c("file","stream") File will save to a file in output_dir, Stream returns the contents directly
#' @param theory Logical indicating whether to include theory in the writeup
#' @param methods Logical indicating whether to include methods in the writeup
#' @param params additional parameters for rmarkdown::render
#' @param output_file character
#' @param output_format character
#'
#' @export
#'
#' @return String representing the methods used to generate the model
ena.writeup <- function(
  enaset,
  tool = "rENA", tool.version = as.character(packageVersion(tool)),
  comparison = NULL, comparison.groups = NULL, sig.dig = 2,
  output_dir = getwd(), type = c("file","stream"), theory = T, methods = T,
  params = NULL, output_file = NULL, output_format = NULL
) {
  if(is.null(enaset$`_function.params`$weight.by))
    enaset$`_function.params`$weight.by <- enaset$`_function.params`$args$weight.by

  type = match.arg(type, choices = c("file","stream"), several.ok = FALSE)

  if(type == "file") {
    output_format = "word_document"
  }

  file = rmarkdown::render(system.file("rmd","methods.rmd", package="rENA"), output_dir = output_dir,
                    knit_root_dir = output_dir, intermediates_dir = output_dir, quiet = TRUE,
                    params = params, output_file = output_file
                    ,output_format = output_format
                    # ,output_format = ifelse(type == "file", rENA::methods_report, rENA::methods_report_stream)
                  )

  if(type == "file")
    file
  else if (type == "stream" && endsWith(file, ".plain"))
    readChar(file, file.info(file)$size)
}

#' @title methods_report
#' @description Methods report for rmarkdwon
#' @param toc [TBD]
#' @param toc_depth [TBD]
#' @param fig_width [TBD]
#' @param fig_height [TBD]
#' @param keep_md [TBD]
#' @param md_extensions [TBD]
#' @param pandoc_args [TBD]
#'
#' @export
methods_report <- function(toc = FALSE,
                          toc_depth = 3,
                          fig_width = 5,
                          fig_height = 4,
                          keep_md = FALSE,
                          md_extensions = NULL,
                          pandoc_args = NULL) {

  # knitr options and hooks
  knitr <- rmarkdown::knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig_width,
                      fig.height = fig_height)
  )

  # build pandoc args
  args <- c("--standalone")

  # table of contents
  args <- c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))

  # pandoc args
  args <- c(args, pandoc_args)

  preserved_chunks <- character()

  # pre_processor <- function(metadata, input_file, runtime, knit_meta,
  #                           files_dir, output_dir) {
  #   preserved_chunks <<- extract_preserve_chunks(input_file, knitr::extract_raw_output)
  #   NULL
  # }

  # post_processor <- function(metadata, input_file, output_file, clean, verbose) {
  #   output_str <- readLines(output_file, encoding = 'UTF-8')
  #   output_res <- knitr::restore_raw_output(output_str, preserved_chunks)
  #   if (!identical(output_str, output_res))
  #     writeLines(enc2utf8(output_res), output_file, useBytes = TRUE)
  #   output_file
  # }

  # return output format
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = rmarkdown::pandoc_options(to = "docx",
                            from = rmarkdown::from_rmarkdown(extensions = md_extensions),
                            args = args),
    keep_md = keep_md
    # ,pre_processor = pre_processor,
    # post_processor = post_processor
  )
}

#' @title methods_report_stream
#' @description Methods report for rmarkdwon
#' @param toc [TBD]
#' @param toc_depth [TBD]
#' @param fig_width [TBD]
#' @param fig_height [TBD]
#' @param keep_md [TBD]
#' @param md_extensions [TBD]
#' @param pandoc_args [TBD]
#'
#' @export
methods_report_stream <- function(toc = FALSE,
                          toc_depth = 3,
                          fig_width = 5,
                          fig_height = 4,
                          keep_md = FALSE,
                          md_extensions = NULL,
                          pandoc_args = NULL) {

  # knitr options and hooks
  knitr <- rmarkdown::knitr_options(
    opts_chunk = list(dev = 'png',
                      dpi = 96,
                      fig.width = fig_width,
                      fig.height = fig_height)
  )

  # build pandoc args
  args <- c("--standalone")

  # table of contents
  args <- c(args, rmarkdown::pandoc_toc_args(toc, toc_depth))

  # pandoc args
  args <- c(args, pandoc_args)

  preserved_chunks <- character()

  # return output format
  rmarkdown::output_format(
    knitr = knitr,
    pandoc = rmarkdown::pandoc_options(to = "plain",
                            from = rmarkdown::from_rmarkdown(extensions = md_extensions),
                            args = args),
    keep_md = keep_md
  )
}
