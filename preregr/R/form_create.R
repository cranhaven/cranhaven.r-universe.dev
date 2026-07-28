#' Create a new (pre)registration form
#'
#' You can use this function to create a new (pre)registration form.
#' The "Creating a (pre)registration form" vignette explains how this works.
#' That is available at
#' <https://preregr.opens.science/articles/creating_prereg_form.html>
#' or can be shown by running \code{vignette("creating_prereg_form",
#' package = "preregr")}.
#'
#' @param title The form's title
#' @param version The form's version. If there is only one version and the
#' creators do not plan to release future version, the recommendation is to
#' set the version to `1`. If the creators have no clear idea about how to
#' version the form (i.e. there may be improvements in the future), the
#' recommendation is to set the first version to `0.0.1`, and roughly adopt
#' the system common for software: increment the first number when the form
#' is substantially updated (e.g. such that preregistrations that used the
#' previous version of the form may no longer be valid given the new version,
#' for example becaue sections were added or removed, or value templates
#' changed, etc), the third number for very small changes (e.g. typos,
#' spelling corrections, clarification or extra explanations, bug fixes in
#' regular expressions in value templates, etc), and the second number for
#' changes in between (e.g. changing the order of items or moving an item
#' to another section, or changing value templates to be more permissive (and
#' so, retaining compatibility with (pre)registrations that used the previous
#' version of the form)). In that case, use version `1.0.0` to signal that the
#' form has reached maturity.
#' @param author The authors of the form
#' @param date The date the form was created
#' @param ... Additional field-content pairs to specify arbitrary metadata.
#'
#' @return The `preregr` form object prefilled with some examples.
#' @export
#'
#' @examples exampleForm <-
#'   preregr::form_create(
#'     title = "Example form",
#'     version = "0.1.0"
#'   );
#'
#' ### Show the form summary
#' exampleForm;
form_create <- function(title,
                        version,
                        author = NA,
                        date = format(Sys.Date(), "%Y-%m-%d"),
                        ...) {

  dots <- list(...);

  metadata <-
    data.frame(
      field = c("title", "version", "author", "date", names(dots)),
      contents = c(title, version, author, date, unname(unlist(dots)))
    );

  res <- list(
    instructions = data.frame(heading = "First instruction",
                              description = "Actual instructions"),
    metadata = metadata,
    items = data.frame(section_id = "example_section",
                       item_id = "example_item",
                       item_label = "Example Label",
                       item_description = "Example item description",
                       item_valueTemplate = "string",
                       item_validValues = NA,
                       item_validation = NA),
    sections = data.frame(section_id = "example_section",
                          section_label = "Example Section",
                          section_description = "Example section description"),
    valueTemplates = data.frame(
      identifier = c("numeric", "integer", "string", "categorical"),
      description = c("Any valid number",
                      "Any valid whole number",
                      "A single character value",
                      "A string that has to exactly match one of the values specified in the 'items_validValues' column of the items sheet"),
      validValues = c(NA, NA, NA, NA),
      default = c(NA, NA, NA, NA),
      examples = c("2.3 || 643.2", "30 || 8762", "'Example' || 'Another example'", "VALIDVALUES"),
      validation = c("is.na(VALUE) || (is.numeric(VALUE) && (length(VALUE) == 1))",
                     "is.na(VALUE) || (is.numeric(VALUE) && (VALUE%%1==0) && (length(VALUE) == 1))",
                     "is.na(VALUE) || (is.character(VALUE) && length(VALUE) == 1)",
                     "is.na(VALUE) || (VALUE %in% VALIDVALUES)"),
      error = c("NAME is not numeric, or contains more than one value.",
                "NAME is not numeric, contains more than one value, or contains decimals.",
                "NAME is not a single text string.",
                "NAME is not one of VALIDVALUES."))
  );

  class(res) <- c("preregr", "preregr_form", "list");

  return(res);

}
