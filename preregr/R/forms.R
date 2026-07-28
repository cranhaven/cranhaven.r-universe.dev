#' Included (pre)registration forms
#'
#' Show an overview of all included (pre)registration forms.
#'
#' @return Invisibly, a list of form identifiers of the available forms.
#' @export
#'
#' @examples forms();
forms <- function() {
  res <-
    utils::data(package="preregr");
  res <-
    grep(
      "^form_",
      res$results[, "Item"],
      value=TRUE
    );
  loadedForms <-
    lapply(
      res,
      function(dataname) {
        formSpec <-
          get(
            utils::data(list=dataname,
                        package='preregr',
                        envir = environment())
          );
        return(formSpec);
      }
    );
  formIds <-
    gsub(
      "^form_",
      "",
      res);
  formNames <-
    lapply(
      loadedForms,
      function(form) {
        return(
          form$metadata[form$metadata$field=="title", "content"]
        );
      }
    );
  names(loadedForms) <- formIds;
  formList <-
    stats::setNames(
      paste0(cli::col_green(formIds),
             ": ", formNames),
      nm = rep("v", length(formIds))
    );
  cli::cli({
    cli::cli_alert_info("The following forms are available:\n");
    cli::cli_bullets(formList);
    cli::cli_text();
    cli::cli_alert_info(
      paste0("You can view a form with {.code preregr::form_show()}, ",
             "and initialize one with {.code preregr::prereg_initialize()}, ",
             "passing its quoted identifier (e.g. {.code \"{formIds[1]}\"}).\n")
    );
  });
  return(invisible(formIds));
}
