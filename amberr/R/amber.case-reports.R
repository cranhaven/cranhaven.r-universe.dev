#' Get the case reports of one or several form(s).
#'
#' @title Get the case report forms
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param revision Revision number, optional, default is NULL (means that the
#' case report form uses the latest form revision).
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.case_report_forms(a, form="61e69a22fea2df2f3108b508", skip=0, limit=10)
#' amber.case_report_forms(a, form="Adult trauma")
#' amber.case_report_forms(a, study="Trauma Registry", query = list(revision = 1))
#' amber.case_report_forms(a, query = list(revision = 1))
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.case_report_forms <-
  function(amber,
           study = NULL,
           form = NULL,
           revision = NULL,
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
    query$revision <- revision
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "case-report-form", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          name = val$name,
          description = val$description,
          study = val$study,
          form = val$form,
          revision = val$revision,
          state = val$state,
          repeatPolicy = val$repeatPolicy,
          permissions_users = ifelse(
            is.null(val$permissions) ||
              is.null(val$permissions$users),
            NA,
            paste(val$permissions$users, collapse = "|")
          ),
          permissions_groups = ifelse(
            is.null(val$permissions) ||
              is.null(val$permissions$groups),
            NA,
            paste(val$permissions$groups, collapse = "|")
          ),
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

#' Get a case report form by its name or identifier and its revision number (if any).
#'
#' @title Get a case report form
#' @family studies functions
#' @param amber An Amber object
#' @param id Case report form's name or identifier
#' @param study Study identifier (name or id), optional.
#' @param form Form's name or identifier
#' @param revision Revision number, optional, default is NULL (means that the
#' case report form uses the latest form revision).
#' @param query The search query, to desambiguate form lookup by name
#' @return A case report form object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.case_report_form(a, id = "Adult trauma - baseline", form = "Adult trauma", revision = 10)
#' amber.case_report_form(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.case_report_form <-
  function(amber,
           id,
           study = NULL,
           form = NULL,
           revision = NULL,
           query = list()) {
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
    query$revision <- revision
    if (regexpr("^[a-zA-Z]+", id) == 1) {
      query$name <- id
    } else {
      query$`_id` <- id
    }
    res <- .get(amber, "case-report-form", query = query)
    if (length(res$data) > 0) {
      if (length(res$data) > 1)
        warning(
          "There are more than one case report form matching the criteria",
          immediate. = TRUE,
          call. = FALSE
        )
      res$data[[1]]
    } else {
      NULL
    }
  }


#' Get the case report records of one or several form(s).
#'
#' @title Get the case report records
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param caseReportForm Case report form identifier (name or id), optional.
#' @param from From date (included), optional
#' @param to To date (included), optional
#' @param pId Patient/participant identifier
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find all case reports
#' amber.case_reports(a)
#'
#' # Find all case reports in a range of time
#' amber.case_reports(a, from = "2022-01-12 00:00", to = "2022-02-13")
#'
#' # Find all case reports for a specific participant/patient identifier
#' amber.case_reports(a, pId = "1231")
#'
#' # Find all case reports having their identifier matching a regular expression
#' amber.case_reports(a, query = list(`data._id[$search]` = "^12"))
#'
#' # Find all case reports which form data is equal to some value
#' # (will not work if the data are encrypted in the database)
#' amber.case_reports(a, query = list(data.PATIENT.ORIGIN_REGION = "xyz"))
#'
#' # Export records collected with a study's form in a specific version
#' amber.case_reports(a,
#'   study = "Trauma Registry",
#'   form = "Adult trauma",
#'   query = list(revision = 6))
#'
#' # Export records collected with a specific case report form
#' amber.case_reports(a, caseReportForm = "Adult trauma - test")
#'
#' # Export records collected with a study's form in all versions used
#' amber.case_reports(a,
#'   study = "Trauma Registry",
#'   form = "Adult trauma")
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.case_reports <-
  function(amber,
           study = NULL,
           form = NULL,
           caseReportForm = NULL,
           from = NULL,
           to = NULL,
           pId = NULL,
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
    if (!is.null(caseReportForm)) {
      caseReportFormObj <-
        amber.case_report_form(amber, caseReportForm, form = form, study = study)
      if (!is.null(caseReportFormObj)) {
        query$caseReportForm <- caseReportFormObj$`_id`
      } else {
        stop("No such case report form with ID or name: ",
             caseReportForm,
             call. = FALSE)
      }
    }
    if (!is.null(from)) {
      query$`updatedAt[$gte]` <- .formatDate(from)
    }
    if (!is.null(to)) {
      query$`updatedAt[$lte]` <- .formatDate(to)
    }
    if (!is.null(pId)) {
      query$`data._id` <- pId
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "case-report", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          caseReportForm = val$caseReportForm,
          study = val$study,
          form = val$form,
          revision = val$revision,
          state = val$state,
          actions = as.character(jsonlite::toJSON(
            lapply(val$actions, function(action) {
              action$`_id` <- NULL
              action
            }), auto_unbox = TRUE
          )),
          data = as.character(jsonlite::toJSON(val$data, auto_unbox = TRUE)),
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

#' Export the case report records of one or several form(s).
#'
#' @title Export the case report records
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param form Form identifier (name or id), optional.
#' @param caseReportForm Case report form identifier (name or id), optional.
#' @param from From date (included), optional
#' @param to To date (included), optional
#' @param pId Patient/participant identifier
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A named list of data.frames, a data dictionary and a data data.frame
#' per form revision (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find all case reports
#' amber.case_report_export(a)
#'
#' # Find all case reports in a range of time
#' amber.case_report_export(a, from = "2022-01-12 00:00", to = "2022-02-13")
#'
#' # Find all case reports for a specific participant/patient identifier
#' amber.case_report_export(a, pId = "1231")
#'
#' # Find all case reports having their identifier matching a regular expression
#' amber.case_report_export(a, query = list(`data._id[$search]` = "^12"))
#'
#' # Find all case reports which form data is equal to some value
#' # (will not work if the data are encrypted in the database)
#' amber.case_report_export(a, query = list(data.PATIENT.ORIGIN_REGION = "xyz"))
#'
#' # Export records collected with a study's form in a specific version
#' amber.case_report_export(a,
#'   study = "Trauma Registry",
#'   form = "Adult trauma",
#'   query = list(revision = 6))
#'
#' # Export records collected with a specific case report form
#' tables <- amber.case_report_export(a, caseReportForm = "Adult trauma - test")
#'
#' # Export records collected with a study's form in all versions used
#' tables <- amber.case_report_export(a,
#'   study = "Trauma Registry",
#'   form = "Adult trauma")
#'
#' # Result contains both data and dictionary
#' tables
#'
#' # Tables are named with the <case report form name>-<revision> pattern
#' names(tables)
#'
#' # Merge datasets from different versions if relevant
#' dplyr::bind_rows(lapply(tables, function (t) {
#'   t$data
#' }))
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.case_report_export <-
  function(amber,
           study = NULL,
           form = NULL,
           caseReportForm = NULL,
           from = NULL,
           to = NULL,
           pId = NULL,
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
    if (!is.null(caseReportForm)) {
      caseReportFormObj <-
        amber.case_report_form(amber, caseReportForm, form = form, study = study)
      if (!is.null(caseReportFormObj)) {
        query$caseReportForm <- caseReportFormObj$`_id`
      } else {
        stop("No such case report form with ID or name: ",
             caseReportForm,
             call. = FALSE)
      }
    }
    if (!is.null(from)) {
      query$`updatedAt[$gte]` <- .formatDate(from)
    }
    if (!is.null(to)) {
      query$`updatedAt[$lte]` <- .formatDate(to)
    }
    if (!is.null(pId)) {
      query$`data._id` <- pId
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "case-report-export", query = query)
    .reportListMetrics(res)

    if (df) {
      out <- list()
      tables <- res$data
      for (name in names(tables)) {
        data <- tables[[name]]$data
        variables <- tables[[name]]$variables

        # make sure every variable has a column (records do not have a field when there is no value for it)
        tbl <- NULL
        for (values in data) {
          row <- list(`_id` = values$`_id`)
          for (variable in variables) {
            row[[variable$name]] <- .unlist(values[[variable$name]])
          }
          tbl <- dplyr::bind_rows(tbl, row)
        }

        # prepare dictionary in tabular format
        out[[name]] <- list(dictionary = .makeDictionary(variables),
                            data = tbl)
      }
      out
    } else {
      res
    }
  }
