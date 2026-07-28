#' Specify the content for one or more (pre)registration items
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param ... Item-content pairings.
#' @param append Whether to replace (`append=FALSE`) or append (`append=TRUE`)
#' the content if an item already contains some content.
#' @param validate Whether to validate the specified content for each item
#' using the validation rules in the (pre)registration form.
#' @param requireValidContent Whether to only store new content if it passes
#' validation. Note that this is ignored if `validate=FALSE`.
#' @param silent Whether to be silent or chatty.
#'
#' @return x, invisibly
#' @export
#'
#' @examples
#' ### Load an example (pre)registration specification
#' data("examplePrereg_1", package = "preregr");
#'
#' ### Specify some fields and show the results
#' examplePrereg_1 |>
#'   preregr::prereg_specify(
#'     tasks_and_roles = "All authors contributed equally",
#'     nonExistent_item = "This can't be stored anywhere",
#'     start_date = "2021-9-01"
#'   ) |>
#'   preregr::prereg_show_item_completion(
#'     section="metadata"
#'   );
prereg_specify <- function(x,
                           ...,
                           append = TRUE,
                           validate = TRUE,
                           requireValidContent = TRUE,
                           silent = preregr::opts$get("silent")) {

  itemCols <- preregr::opts$get("itemCols");
  valueTemplateCols <- preregr::opts$get("valueTemplateCols");

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  dots <- list(...);

  specNames = names(dots);

  specNameValidity <-
    specNames %in% names(x$specs);

  validSpecNames <- specNames[specNameValidity];

  invalidSpecNames <- specNames[!specNameValidity];

  if (!silent) {
    cli::cli_h1("Specifying content for (pre)registration items");
  }

  if (length(invalidSpecNames) > 0) {
    if (!silent) {
      cli::cli_alert_warning(
        paste0(
          "You specified content for item{?s} {cli::qty(invalidSpecNames)}",
          vecTxtQ(invalidSpecNames, colFun=cli::col_red),
          ", ",
          "but {?this/these} item{?s} do{?es/} not exist in the prereg form specification ",
          "that you initialized (",
          cli::col_cyan(
            x$form$meta[x$form$meta$field=="title", "content"]
          ),
          "). Ignoring these items."
        )
      );
    }
  }

  for (i in validSpecNames) {

    itemsRow <- which(x$form$items[, itemCols["id_Col"]] == i);

    valueTemplatesRow <-
      which(
        x$form$valueTemplates[
          ,
          valueTemplateCols["identifierCol"]
        ] ==
          x$form$items[
            x$form$items[, itemCols["id_Col"]] == i,
            itemCols["valueTemplate_Col"]
          ]
      );

    item_label <- x$form$items[itemsRow, itemCols["label_Col"]];

    setNewContent <- TRUE;

    if (validate) {

      if (dots[[i]] == x$config$initialText) {
        valueToCheck <- NA;
      } else {
        valueToCheck <- dots[[i]];
      }

      validationResult <-
        validate_value(
          VALUE = valueToCheck,
          validations = list(
            x$form$items[
              itemsRow,
              itemCols["validation_Col"]
            ],
            x$form$valueTemplates[
              valueTemplatesRow,
              valueTemplateCols["validationCol"]
            ]
          ),
          replacementSources = list(
            x$form$items,
            x$form$valueTemplates
          ),
          errorMessages = list(
            x$form$items[
              itemsRow,
              itemCols["error_Col"]
            ],
            x$form$valueTemplates[
              valueTemplatesRow,
              valueTemplateCols["errorCol"]
            ]
          )
        );

      if (!silent) {

        if (nchar(validationResult) == 0) {

          cli::cli_alert_success(
            "Content specified for item {.field {item_label}} passed validation!"
          );

        } else {

          cli::cli_alert_warning(
            paste0(
              "Item {.field {item_label}} did not pass validation: ",
              cli::col_magenta(validationResult), "."
            )
          );

        }

      }

      if ((requireValidContent) && (nchar(validationResult) > 0)) {
        setNewContent <- FALSE;
      }

    }

    if (setNewContent) {

      if (x$specs[[i]]$text == x$config$initialText) {

        x$specs[[i]]$text <- dots[[i]];

      } else {

        if (append) {

          x$specs[[i]]$text <-
            paste0(
              x$specs[[i]]$text,
              "\n",
              dots[[i]]
            );

        } else {

          if (nchar(x$specs[[i]]$text) == 0) {

            x$specs[[i]]$text <- dots[[i]];

          } else {

            if (!silent) {
              cli::cli_alert_warning(
                paste0(
                  "Item {.field {item_label}} already had content (\"",
                  cli::col_magenta(x$specs[[i]]$text),
                  "\"), which was overwritten."
                )
              );
            }

          }

        }
      }

      if (!silent) {
        cli::cli_alert_success(
          paste(
            "Specified content for {.field {item_label}}",
            cli::col_silver(paste0("[", i, "]"))
          )
        );
      }

    } else {

      if (!silent) {
        cli::cli_alert_danger(
          paste0(
            "The content you specified for {.field {item_label}} ",
            cli::col_silver(paste0("[", i, "]")),
            " failed to pass validation with message '", validationResult,
            "', so did not set it."
          )
        );
      }

    }

  }

  class(x) <- c("preregr", "preregr_spec", "list");

  return(x);

}
