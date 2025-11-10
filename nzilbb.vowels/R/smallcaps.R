#' @importFrom stringr str_extract
smallcaps <- function() {

  # Get currently selected ranges.
  doc_selections <- rstudioapi::getSourceEditorContext()['selection']

  # Determine prefix and postfix. For qmd files, use [text](.smallcaps),
  # otherwise use <span style="font-variant: small-caps;">text</span>.
  current_path <- rstudioapi::getSourceEditorContext()['path'][[1]]

  extension <- str_extract(current_path, "\\.([A-Za-z]+)$", group=1)

  if (extension == "qmd") {
    pre <- "["
    post <- "]{.smallcaps}"
  } else {
    pre <- '<span style="font-variant: small-caps;">'
    post <- '</span>'
  }

  for (i in seq_along(doc_selections$selection)) {
    # Add text
    rstudioapi::modifyRange(
      location=doc_selections$selection[[i]]$range,
      text=base::paste0(
        pre,
        doc_selections$selection[[i]]$text,
        post
      )
    )
  }

  # Now modify selection to account for addition of pre.
  doc_ranges <- list()

  for (i in base::seq_along(doc_selections$selection)) {

    shifted_start <- doc_selections$selection[[i]]$range$start +
      c(0, nchar(pre))

    shifted_end <- doc_selections$selection[[i]]$range$end +
      c(0, nchar(pre))

    new_range = rstudioapi::document_range(
      start = rstudioapi::document_position(
        row = shifted_start[[1]],
        column = shifted_start[[2]]
      ),
      end = rstudioapi::document_position(
        row = shifted_end[[1]],
        column = shifted_end[[2]]
      )
    )

    doc_ranges <- append(doc_ranges, list(new_range))

  }

  # Select content within span.
  rstudioapi::setSelectionRanges(
    ranges = doc_ranges
  )

}
