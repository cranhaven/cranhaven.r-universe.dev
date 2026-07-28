#' Add an instruction, section, or item to a (pre)registration form
#'
#' @param x The (pre)registration form as created by [preregr::form_create()].
#' @param heading The instruction's heading
#' @param description The description of the instruction, section, or item
#' @param id The identifier of the section or item
#' @param label The label (i.e. title) of the section or item
#' @param section_id The section identifier of the section the item should
#' be placed in
#' @param valueTemplate The name of the value template of the item
#' @param validValues The valid values (for categorical items)
#' @param validation The validation statement (an R expression)
#' @param overwrite Whether to overwrite existing content or append the
#' new content
#'
#' @return The modified (pre)registration form
#'
#' @examples ### Create an empty example form
#' exampleForm <-
#'   preregr::form_create(
#'     title = "Example form",
#'     version = "0.1.0"
#'   ) |>
#'   preregr::form_show();
#'
#' ### Add some stuff;
#' exampleForm <-
#'   exampleForm |>
#'   preregr::form_add_instruction(
#'     heading = "First Real Instruction",
#'     description = "Which normally also contains real instructions here"
#'   ) |>
#'   preregr::form_add_section(
#'     id = "first_section",
#'     label = "First Real Section",
#'     description = "This section is very, very important."
#'   ) |>
#'   preregr::form_add_section(
#'     id = "second_section",
#'     label = "Second Real Section",
#'     description = "This section is even more important then the first one."
#'   ) |>
#'   preregr::form_add_item(
#'     id = "study_title",
#'     label = "Study Title",
#'     section_id = "first_section",
#'     description = paste0(
#'       "Think of a catching title, preferably with a colon in ",
#'       "the middle. Bonus points for pop culture references."
#'     )
#'   ) |>
#'   preregr::form_add_item(
#'     id = "study_authors",
#'     label = "Authors",
#'     section_id = "first_section",
#'     description = "Maybe list the authors, too."
#'   ) |>
#'   preregr::form_add_item(
#'     id = "registration_type",
#'     label = "Registration type",
#'     section_id = "second_section",
#'     description = paste0(
#'       "Describe briefly why you are (pre)registering this ",
#'       "study. For example, this might be a preregistration ",
#'       "to allow others to know you're doing this study; or to ",
#'       "make it clear you value transparency in science; or to ",
#'       "remember your original plans later on. Or this might be ",
#'       "a registration to update your plans after the data came ",
#'       "in; or to document pragmatic changes in plans."
#'     )
#'   );
#'
#' ### Show the result of our hard labour
#' preregr::form_show(exampleForm);
#'
#' @rdname form_add
#' @export
form_add_instruction <- function(x,
                                 heading,
                                 description,
                                 overwrite = TRUE) {

  form_add_stuff(
    x = x,
    type = "instructions",
    idCol = "heading",
    idValue = heading,
    description = description,
    defaultId = "First instruction",
    overwrite = overwrite
  );

}

#' @rdname form_add
#' @export
form_add_section <- function(x,
                             id,
                             label,
                             description,
                             overwrite = TRUE) {

  form_add_stuff(
    x = x,
    type = "sections",
    idCol = "section_id",
    idValue = id,
    section_label = label,
    section_description = description,
    defaultId = "example_section",
    overwrite = overwrite
  );

}

#' @rdname form_add
#' @export
form_add_item <- function(x,
                          id,
                          label,
                          description,
                          section_id,
                          valueTemplate = "string",
                          validValues = NA,
                          validation = NA,
                          overwrite = TRUE) {

  form_add_stuff(
    x = x,
    type = "items",
    idCol = "item_id",
    idValue = id,
    item_label = label,
    item_description = description,
    section_id = section_id,
    item_valueTemplate = valueTemplate,
    item_validValues = validValues,
    item_validation = validation,
    defaultId = "example_item",
    overwrite = overwrite
  );

}


form_add_stuff <- function(x,
                           type,
                           idCol,
                           idValue,
                           defaultId,
                           ...,
                           overwrite = TRUE) {

  if (!(inherits(x, "preregr") && inherits(x, "preregr_form"))) {
    stop("As `x`, you have to pass a {preregr} form (see\n\n",
         "  ?preregr::form_create()\n\n",
         "for more information) or an initialized {preregr}",
         " object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  if (!(type %in% names(x))) {
    x$type <- data.frame();
    rowNr <- 1;
  } else {

    if (defaultId %in% x[[type]][, idCol]) {
      rowNr <- which(x[[type]][, idCol] == defaultId);
    } else if (overwrite && (idValue %in% x[[type]][, idCol])) {
      rowNr <- which(x[[type]][, idCol] == idValue);
    } else {
      rowNr <- nrow(x[[type]]) + 1;
    }
  }

  dots <- list(...);

  x[[type]][rowNr, idCol] <- idValue;

  for (i in names(dots)) {
    x[[type]][rowNr, i] <- dots[[i]];
  }

  return(x);

}
