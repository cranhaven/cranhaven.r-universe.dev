#' Create a nisra_df object
#'
#' Create a nisra_df, a data-frame with additional metadata. This uses
#' [vctrs::new_data_frame()] to convert the `...` argument to a data-frame.
#'
#' @param ... named arguments to convert to columns, or an existing data-frame,
#' or a named list of columns
#' @param meta List of metadata fields
#'
#' @returns A `nisra_df`, a [tbl_df][tibble::tbl_df-class] with an additional
#' `"nisra_df"` class attribute
nisra_df <- function(..., meta = list()) {
  data <- vctrs::df_list(...)
  new_nisra_df(data, meta = meta, class = c("tbl_df", "tbl"))
}

new_nisra_df <- function(
  x = list(),
  n = NULL,
  meta = list(),
  ...,
  class = NULL
) {
  out <- vctrs::new_data_frame(x, n = n, ..., class = c("nisra_df", class))
  attr(out, "meta") <- new_nisra_meta(meta)
  out
}

#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.nisra_df <- function(x, setup, ...) {
  default_footer <- NextMethod()

  if (!"label" %in% names(attr(x, "meta", exact = TRUE))) {
    return(default_footer)
  }

  source_info <- paste0("Source: ", get_metadata_field(x, "label"))
  source_footer <- pillar::style_subtle(paste0("# ", source_info))
  c(default_footer, source_footer)
}

new_nisra_meta <- function(x = list()) {
  structure(
    x,
    class = c("nisra_meta", "list")
  )
}

official_stat_type <- function(official, experimental) {
  dplyr::case_when(
    official & !experimental ~ "Official statistics",
    official & experimental ~ "Official statistics in development",
    .default = "Not official statistics"
  )
}

#' @exportS3Method
print.nisra_meta <- function(x, ...) {
  stat_type <- official_stat_type(x$official, x$experimental)

  note <- x$note
  if (nchar(note) > 100) {
    note <- stringr::str_replace(
      stringr::str_sub(note, 1, 95),
      " +\\S*$",
      " ..."
    )
  }

  cat(
    glue::glue(
      "Label: {x$label}",
      "Subject: {x$subject$value}",
      "Type: {stat_type}",
      "Updated: {x$updated}",
      "Note: {note}",
      "Contact: {x$contact$name}",
      "Contact email: {x$contact$email}",
      "Contact phone: {x$contact$phone}",
      "Copyright: {x$copyright$name} ({x$copyright$href})",
      .sep = "\n"
    )
  )
}
