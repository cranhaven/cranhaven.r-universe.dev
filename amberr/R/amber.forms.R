#' Get the forms
#'
#' @title Get the forms
#' @family studies functions
#' @param amber An Amber object
#' @param study Study name or identifier, optional
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.forms(a)
#' amber.forms(a, study="Trauma Registry", skip = 0, limit = 10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.forms <-
  function(amber,
           study = NULL,
           query = list(),
           skip = 0,
           limit = 100,
           df = TRUE) {
    if (!is.null(study)) {
      studyObj <- amber.study(amber, study)
      if (!is.null(studyObj)) {
        query$study <- studyObj$`_id`
      } else {
        stop("No such study with ID or name: ", study, call. = FALSE)
      }
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "form", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          study = val$study,
          name = val$name,
          description = val$description,
          createdBy = val$createdBy,
          createdAt = val$createdAt,
          updatedAt = val$updatedAt
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }

#' Get a form by name or identifier.
#'
#' @title Get a form
#' @family studies functions
#' @param amber An Amber object
#' @param id Form's name or identifier
#' @param study Study name or identifier, optional
#' @param query The search query, to desambiguate form lookup by name
#' @return A form object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form(a, id = "Adult trauma")
#' amber.form(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.form <- function(amber,
                       id,
                       study = NULL,
                       query = list()) {
  if (!is.null(study)) {
    studyObj <- amber.study(amber, study)
    if (!is.null(studyObj)) {
      query$study <- studyObj$`_id`
    } else {
      stop("No such study with ID or name: ", study, call. = FALSE)
    }
  }
  if (regexpr("^[a-zA-Z]+", id) == 1) {
    query$name <- id
  } else {
    query$`_id` <- id
  }
  res <- .get(amber, "form", query = query)
  if (length(res$data) > 0) {
    if (length(res$data) > 1)
      warning(
        "There are more than one form matching the criteria",
        immediate. = TRUE,
        call. = FALSE
      )
    res$data[[1]]
  } else {
    NULL
  }
}

#' Get the revisions of one or several form(s).
#'
#' @title Get the form revisions
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form_revisions(a, form="61e69a22fea2df2f3108b508", skip = 0, limit = 10)
#' amber.form_revisions(a, form="Adult trauma")
#' amber.form_revisions(a, study="Trauma Registry", query = list(revision = 1))
#' amber.form_revisions(a, query = list(revision = 1))
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.form_revisions <-
  function(amber,
           study = NULL,
           form = NULL,
           query = list(),
           skip = 0,
           limit = 100,
           df = TRUE) {
    if (!is.null(study)) {
      studyObj <- amber.study(amber, study)
      if (!is.null(studyObj)) {
        query$study <- studyObj$`_id`
      } else {
        stop("No such study with ID or name: ", study, call. = FALSE)
      }
    }
    if (!is.null(form)) {
      formObj <- amber.form(amber, form, study = study)
      if (!is.null(formObj)) {
        query$form <- formObj$`_id`
      } else {
        stop("No such form with ID or name: ", form, call. = FALSE)
      }
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "form-revision", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          study = val$study,
          form = val$form,
          revision = val$revision,
          comment = val$comment,
          publishedBy = val$publishedBy,
          createdAt = val$createdAt,
          updatedAt = val$updatedAt
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }

#' Get a form revision by its form name or identifier and its revision number.
#'
#' @title Get a form revision
#' @family studies functions
#' @param amber An Amber object
#' @param form Form's name or identifier
#' @param revision Revision number
#' @param study Study identifier (name or id), optional.
#' @return A form revision object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.form_revision(a, form = "Adult trauma", revision = 10)
#' amber.form_revision(a, form = "61e69a22fea2df2f3108b508", revision = 10)
#' amber.logout(a)
#' }
#' @export
amber.form_revision <-
  function(amber, form, revision, study = NULL) {
    formObj <- amber.form(amber, form, study = study)
    if (!is.null(formObj)) {
      query <- list(form = formObj$`_id`,
                    revision = revision)
      res <- .get(amber, "form-revision", query = query)
      if (length(res$data) > 0) {
        if (length(res$data) > 1)
          warning(
            "There are more than one form revision matching the criteria",
            immediate. = TRUE,
            call. = FALSE
          )
        res$data[[1]]
      } else {
        NULL
      }
    } else {
      stop("No such form with ID or name: ", form, call. = FALSE)
    }
  }
