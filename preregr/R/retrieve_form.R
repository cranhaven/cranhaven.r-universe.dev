retrieve_form <- function(x) {

  if (inherits(x, "preregr_spec")) {

    if ((!is.null(x$form)) && (inherits(x$form, "preregr_form"))) {
      formSpec <- x$form
    } else {
      stop("You passed a (pre)registration specification as `x`, but ",
           "the form wasn't saved alongside the preregistration, so ",
           "you didn't actually pass me a form.");
    }

  } else if (inherits(x, "preregr_form")) {

    formSpec <- x;

  } else {

    ### This works if x is a valid file or Google Sheets URL

    formSpec <- read_spreadsheet(
      x,
      exportGoogleSheet = TRUE,
      failQuietly = TRUE,
      silent = TRUE
    );

  }

  ### If it's still NULL, maybe it's a pre-installed form name

  if (is.null(formSpec)) {

    if (is.character(x) && (length(x) == 1)) {

      includedFormName <- paste0("form_", x);

      if (includedFormName %in%
          utils::data(package='preregr')$results[, 'Item']) {

        ### Load pre-installed form

        formSpec <-
          get(
            utils::data(list=includedFormName,
                        package='preregr',
                        envir = environment())
          );

      } else {

        ### Otherwise, try to load any object we can find

        formSpec <- get0(includedFormName);

      }

      ### If we failed, give an error

      if (is.null(formSpec)) {
        stop("As `x`, pass either a URL or file with a (pre)registration ",
             "form specification, the name of a (pre)registration form ",
             "included with `preregr`, a (pre)registration form that ",
             "you created with `preregr::form_create()`, imported from ",
             "a URL with `preregr::import_from_html()`, or an object ",
             "with a (pre)registration specification that includes a ",
             "form.");
      }

    } else {

      stop("As `x`, pass either a URL or file with a (pre)registration ",
           "form specification, the name of a (pre)registration form ",
           "included with `preregr`, a (pre)registration form that ",
           "you created with `preregr::form_create()`, imported from ",
           "a URL with `preregr::import_from_html()`, or an object ",
           "with a (pre)registration specification that includes a ",
           "form.");

    }
  }

  class(formSpec) <- c("preregr", "preregr_form", "list");

  return(formSpec);

}

