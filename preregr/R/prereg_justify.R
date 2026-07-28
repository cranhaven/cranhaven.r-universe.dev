#' Justify (and optionally specify) the content for one or more (pre)registration items
#'
#' @param x The (pre)registration object (as produced by a call to
#' [preregr::prereg_initialize()]).
#' @param item The identifier of the item for which to specify the justification
#' of the (pre)registration content.
#' @param decision,justification,assertion,source The decision(s) (with
#' optionally nested within it, one or more justifications), justification(s)
#' (with optionally nested within it, one or more assertions), assertion(s)
#' (with optionally nested within it, one or more sources), or source(s).
#' @param content Optionally, content to specify or append for this item.
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
#' @examples ### Start with an empty form, then specify and justify
#' ### content for an item.
#' preregExampl <-
#'   preregr::prereg_initialize(
#'     "inclSysRev_v0_92"
#'   ) |>
#'     preregr::prereg_justify(
#'       item = "title",
#'       content = "Example title",
#'       decision = "We decide to call this study 'Example title'.",
#'       justification = "It seems a fitting title for an example."
#'     ) |>
#'     preregr::prereg_show_item_completion(
#'       section="metadata"
#'     );
prereg_justify <- function(x,
                           item,
                           decision = NULL,
                           justification = NULL,
                           assertion = NULL,
                           source = NULL,
                           content = NULL,
                           append = TRUE,
                           validate = TRUE,
                           requireValidContent = TRUE,
                           silent = preregr::opts$get("silent")) {

  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------

  if (!requireNamespace('justifier', quietly = TRUE)) {
    stop(
      cli::format_error(
        c(
          "You can't justify without the `justifier` package.",
          "i" = "You can install it with:\n\ninstall.packages('justifier');\n"
        )
      )
    );
  }

  itemCols <- preregr::opts$get("itemCols");
  valueTemplateCols <- preregr::opts$get("valueTemplateCols");

  if (!(inherits(x, "preregr") && inherits(x, "preregr_spec"))) {
    stop("As `x`, you have to pass an initialized {preregr} object (see\n\n",
         "  ?preregr::prereg_initialize()\n\n",
         "for more information).");
  }

  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------

  if (is.null(decision) &&
      is.null(justification) &&
      is.null(assertion) &&
      is.null(source)) {

    stop(
      cli::format_error(
        c(
          paste0(
            "You have to pass at least one of `decision`, ",
            "`justification`, `assertion`, and/or `source`."
          ),
          "i" =
            paste0(
              "To specify (pre)registration content without justifying ",
              "it, use `preregr::prereg_specify()`."
            )
        )
      )
    );

  }

  if (!silent) {
    cli::cli_h1("Specifying justifications for (pre)registration items");
  }

  ###---------------------------------------------------------------------------
  ###--- Source
  ###---------------------------------------------------------------------------

  if (!is.null(source)) {

    if (inherits(source, "justifierElement")) {

      sourceToStore <- source;

    } else {

      sourceToStore <-
        preregr::source(
          source,
          tag = "preregr",
          preregr_item_id = item
        );

    }

    cli::cli_alert_success(
      "Specified a justifier source."
    );

    # sourceToStore <- justifier::flatten(sourceToStore);
    #
    # sourceIds <- justifier:::get_ids_from_structured_justifierElements(
    #   sourceToStore$sources
    # );

  } else {
    sourceToStore <- NULL;
    # sourceIds <- NULL;
  }

  ###---------------------------------------------------------------------------
  ###--- Assertion
  ###---------------------------------------------------------------------------

  if (!is.null(assertion)) {

    if (inherits(assertion, "justifierElement")) {

      assertionToStore <- assertion;

    } else {

      if (is.null(sourceToStore)) {
        assertionToStore <-
          preregr::assert(
            assertion,
            tag = "preregr",
            preregr_item_id = item
          );
      } else {
        assertionToStore <-
          preregr::assert(
            assertion,
            source = sourceToStore,
            tag = "preregr",
            preregr_item_id = item
          );
      }

    }

    cli::cli_alert_success(
      "Specified a justifier assertion."
    );

    # assertionToStore <- justifier::flatten(assertionToStore);
    #
    # assertionIds <- justifier:::get_ids_from_structured_justifierElements(
    #   assertionToStore$assertions
    # );

  } else {
    assertionToStore <- NULL;
  }

  ###---------------------------------------------------------------------------
  ###--- Justification
  ###---------------------------------------------------------------------------

  if (!is.null(justification)) {

    if (inherits(justification, "justifierElement")) {

      justificationToStore <- justification;

    } else {

      if (is.null(assertionToStore)) {
        justificationToStore <-
          preregr::justify(
            justification,
            tag = "preregr",
            preregr_item_id = item
          );
      } else {
        justificationToStore <-
          preregr::justify(
            justification,
            assertion = assertionToStore,
            tag = "preregr",
            preregr_item_id = item
          );
      }

    }

    cli::cli_alert_success(
      "Specified a justifier justification."
    );

    # justificationToStore <- justifier::flatten(justificationToStore);
    #
    # justificationIds <- justifier:::get_ids_from_structured_justifierElements(
    #   justificationToStore$justifications
    # );

  } else {
    justificationToStore <- NULL;
  }

  ###---------------------------------------------------------------------------
  ###--- Decision
  ###---------------------------------------------------------------------------

  if (!is.null(decision)) {

    if (inherits(decision, "justifierElement")) {

      decisionToStore <- decision;

    } else {

      if (is.null(justificationToStore)) {
        decisionToStore <-
          preregr::decide(
            decision,
            tag = "preregr",
            preregr_item_id = item
          )
      } else {
        decisionToStore <-
          preregr::decide(
            decision,
            justification = justificationToStore,
            tag = "preregr",
            preregr_item_id = item
          )
      }

    }

    cli::cli_alert_success(
      "Specified a justifier decision."
    );

    # decisionToStore$justification <- NULL;
    #
    # decisionToStore <- justifier::flatten(decisionToStore);

    # if (!is.null(justificationIds)) {
    #
    #   justificationIdsAsObjects <-
    #     justifier::idRef(
    #       structure(
    #         lapply(
    #           justificationIds,
    #           justifier::justify,
    #           label = "",
    #           description = "",
    #           type = NULL
    #         ),
    #         class = c("justifier", "multipleJustifierElements",
    #                   "justifierJustification", "list")
    #       )
    #     );
    #
    #   names(justificationIdsAsObjects) <-
    #     justificationIds;
    #
    #   if (is.null(decisionToStore$justifications) ||
    #       (length(decisionToStore$justifications) == 0)) {
    #     decisionToStore$justifications <-
    #       justificationIdsAsObjects;
    #   } else {
    #     decisionToStore$justifications <-
    #       c(
    #         decisionToStore$justifications,
    #         justificationIdsAsObjects
    #       );
    #   }
    #
    # }

  } else {
    decisionToStore <- NULL;
  }

  ###---------------------------------------------------------------------------
  ###--- Combining
  ###---------------------------------------------------------------------------

  if (!is.null(decisionToStore)) {
    x$jstf <-
      c(x$jstf,
        justifier::flatten(decisionToStore)
        );
  } else if (!is.null(justificationToStore)) {
    x$jstf <-
      c(x$jstf,
        justifier::flatten(justificationToStore)
      );
  } else if (!is.null(assertionToStore)) {
    x$jstf <-
      c(x$jstf,
        justifier::flatten(assertionToStore)
      );
  } else if (!is.null(sourceToStore)) {
    x$jstf <-
      c(x$jstf,
        justifier::flatten(sourceToStore)
      );
  }

  # ###---------------------------------------------------------------------------
  # ###--- Source - link to assertion
  # ###---------------------------------------------------------------------------
  #
  # if (!is.null(source)) {
  #
  #   if (inherits(source, "justifierElement")) {
  #
  #     sourceToStore <- source;
  #
  #   } else {
  #
  #     sourceToStore <- preregr::source(source, tag = "preregr");
  #
  #   }
  #
  #   if (is.null(x$jstf[[item]]$source)) {
  #
  #     x$jstf[[item]]$source <- sourceToStore;
  #
  #   } else {
  #
  #     x$jstf[[item]]$source <- c(x$jstf[[item]]$source, sourceToStore);
  #
  #   }
  #
  # }

  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------

  if (!is.null(content)) {

    if (!silent) {
      cli::cli_h1("Specifying content for (pre)registration items");
    }

    dots <-
      stats::setNames(
        content,
        nm = item
      );

    specNames = names(dots);

    specNameValidity <-
      specNames %in% names(x$specs);

    validSpecNames <- specNames[specNameValidity];

    invalidSpecNames <- specNames[!specNameValidity];

  }

  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------
  ###---------------------------------------------------------------------------

  if (exists('invalidSpecNames') && length(invalidSpecNames) > 0) {
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

  if (exists('validSpecNames')) {

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
  }

  class(x) <- c("preregr", "preregr_spec", "list");

  return(x);

}
