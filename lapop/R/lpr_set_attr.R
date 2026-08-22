##############################################################

# LAPOP Set Variable Attributes from AmericasBarometer Notes #

##############################################################

#' Set Variable Attributes from AmericasBarometer Notes (with propagation)
#'
#' Applies notes stored in a data frame as attributes to variables in `data`.
#' If a variable has expanded children (e.g., vb20_1, vb20_2), the attribute
#' is propagated to all of them by default.
#'
#' @param data data.frame with variables to annotate
#' @param notes data.frame with columns variable_name, note_id, note_value
#' @param noteid character scalar; which note_id to use (e.g., "qtext_en")
#' @param attribute_name character scalar; attribute name to set (e.g., "qwording_en")
#' @param verbose logical; if TRUE it prints all variables notes available but not found in data
#' @param propagate logical; if TRUE, also set on <varname>_* children.
#' Useful for nominal variables or multiple response options variables. Default TRUE.
#' @param overwrite logical; if FALSE, do not overwrite existing attribute on a variable. Default TRUE.
#'
#' @return The input data frame with attributes applied (i.e., question wording)
#'
#' @export

lpr_set_attr <- function(data,
                         notes,
                         noteid = character(),
                         attribute_name = character(),
                         verbose = FALSE,
                         propagate = TRUE,
                         overwrite = TRUE) {

  # basic checks
  req_cols <- c("variable_name", "note_id", "note_value")
  miss <- setdiff(req_cols, names(notes))
  if (length(miss)) stop("`notes` is missing columns: ",
                         paste(miss, collapse=", "))

  note_subset <- notes[notes$note_id == noteid & !is.na(notes$note_value),
                       c("variable_name","note_value")]
  if (!nrow(note_subset)) return(data)

  # escape regex metacharacters in base var names
  .escape <- function(x) gsub("([\\^\\$\\.\\|\\(\\)\\[\\]\\{\\}\\+\\*\\?\\\\-])",
                              "\\\\\\1", x, perl = TRUE)

  for (i in seq_len(nrow(note_subset))) {
    base_var <- as.character(note_subset$variable_name[i])
    text_label <- note_subset$note_value[i]

    if (propagate) {
      # match exact varname base or base_...
      pat <- paste0("^", .escape(base_var), "($|_)")
      targets <- grep(pat, names(data), value = TRUE, perl = TRUE)
    } else {
      targets <- intersect(base_var, names(data))
    }

    if (!length(targets) & verbose == TRUE) {
      message(
        sprintf(
          "Variable %s not found in data (no base or expanded matches) but notes are available.",
          base_var
        )
      )
      next
    }

    for (nm in targets) {
      if (!overwrite && !is.null(attr(data[[nm]], attribute_name))) next
      attr(data[[nm]], attribute_name) <- text_label
    }
  }

  data
}
