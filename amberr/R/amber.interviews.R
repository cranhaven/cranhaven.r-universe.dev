#' Get the interview designs.
#'
#' @title Get the interview designs
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param query A search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.interview_designs(a)
#' amber.interview_designs(a, study="Liftup")
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.interview_designs <-
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
    res <- .get(amber, "interview-design", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          name = val$name,
          description = val$description,
          study = val$study,
          state = val$state,
          steps = ifelse(is.null(val$steps), NA, paste(unlist(
            lapply(val$steps, function(i)
              i$name)
          ), collapse = "|")),
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

#' Get an interview design by its name or identifier.
#'
#' @title Get an interview design
#' @family studies functions
#' @param amber An Amber object
#' @param id Interview design's name or identifier
#' @param study Study identifier (name or id), optional.
#' @param query A search query
#' @return An interview design object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.interview_design(a, id = "Adult trauma - baseline")
#' amber.interview_design(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.interview_design <-
  function(amber,
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
    res <- .get(amber, "interview-design", query = query)
    if (length(res$data) > 0) {
      if (length(res$data) > 1)
        warning(
          "There are more than one interview design matching the criteria",
          immediate. = TRUE,
          call. = FALSE
        )
      res$data[[1]]
    } else {
      NULL
    }
  }

#' Get the interview design campaigns.
#'
#' @title Get the campaigns
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param query A search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.campaigns(a)
#' amber.campaigns(a, study="Liftup", interviewDesign = "Treocapa")
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.campaigns <-
  function(amber,
           study = NULL,
           interviewDesign = NULL,
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
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "campaign", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          name = val$name,
          description = val$description,
          study = val$study,
          interviewDesign = val$interviewDesign,
          validFrom = val$validFrom,
          validUntil = val$validUntil,
          weeksInfoBeforeActivation = val$weeksInfoBeforeActivation,
          weeksBetweenReminders = val$weeksBetweenReminders,
          numberOfReminders = val$numberOfReminders,
          weeksToDeactivate = val$weeksToDeactivate,
          weeksInfoBeforeDeactivation = val$weeksInfoBeforeDeactivation,
          withPassword = val$withPassword,
          visitUrl = val$visitUrl,
          investigators = ifelse(
            is.null(val$investigators),
            NA,
            paste(val$investigators, collapse = "|")
          ),
          supporters = ifelse(
            is.null(val$supporters),
            NA,
            paste(val$supporters, collapse = "|")
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

#' Get an interview design campaign by its name or identifier.
#'
#' @title Get a campaign
#' @family studies functions
#' @param amber An Amber object
#' @param id Interview design's name or identifier
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param query A search query
#' @return A campaign object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.campaign(a, id = "base", interviewDesign = "Adult trauma - baseline")
#' amber.logout(a)
#' }
#' @export
amber.campaign <-
  function(amber,
           id,
           study = NULL,
           interviewDesign = NULL,
           query = list()) {
    if (!is.null(study)) {
      studyObj <- amber.study(amber, study)
      if (!is.null(studyObj)) {
        query$study <- studyObj$`_id`
      } else {
        stop("No such study with ID or name: ", study, call. = FALSE)
      }
    }
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    if (regexpr("^[a-zA-Z]+", id) == 1) {
      query$name <- id
    } else {
      query$`_id` <- id
    }
    res <- .get(amber, "campaign", query = query)
    if (length(res$data) > 0) {
      if (length(res$data) > 1)
        warning(
          "There are more than one campaign matching the criteria",
          immediate. = TRUE,
          call. = FALSE
        )
      res$data[[1]]
    } else {
      NULL
    }
  }

#' Get the participants of one or several interview design campaign(s).
#'
#' @title Get the participants
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param campaign Campaign identifier (name or id), optional.
#' @param code Participant interview code, optional
#' @param identifier Participant identifier, optional
#' @param valid Participant is valid (logical): active and in the valid date range
#' @param query A search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find all participants
#' amber.participants(a)
#'
#' # Find all participants for a specific participant/patient study identifier
#' amber.participants(a, identifier = "1231")
#'
#' # Find all participants which data is equal to some value
#' # (will not work if the data are encrypted in the database)
#' amber.participants(a, query = list(data.country = "fr"))
#'
#' # Find the participants of a study interview campaign
#' amber.participants(a, study = "Liftup", campaign = "base")
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.participants <-
  function(amber,
           study = NULL,
           interviewDesign = NULL,
           campaign = NULL,
           code = NULL,
           identifier = NULL,
           valid = NULL,
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
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    if (!is.null(campaign)) {
      campaignObj <-
        amber.campaign(amber,
                       campaign,
                       study = study,
                       interviewDesign = interviewDesign)
      if (!is.null(campaignObj)) {
        query$campaign <- campaignObj$`_id`
      } else {
        stop("No such campaign with ID or name: ", campaign, call. = FALSE)
      }
    }
    if (!is.null(identifier)) {
      query$identifier <- identifier
    }
    if (!is.null(code)) {
      query$code <- code
    }
    if (!is.null(valid)) {
      query$valid <- ifelse(isTRUE(valid), 'true', 'false')
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "participant", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          code = val$code,
          identifier = val$identifier,
          study = val$study,
          interviewDesign = val$interviewDesign,
          campaign = val$campaign,
          activated = val$activated,
          data = as.character(jsonlite::toJSON(val$data, auto_unbox = TRUE)),
          validFrom = val$validFrom,
          validUntil = val$validUntil,
          valid = val$valid,
          initialContact = val$initialContact,
          initAt = val$initAt,
          reminders = as.character(jsonlite::toJSON(val$reminders, auto_unbox = TRUE)),
          lastSeen = val$lastSeen,
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

#' Get an interview design campaign participant by its code or identifier.
#'
#' @title Get a participant
#' @family studies functions
#' @param amber An Amber object
#' @param code Participant's code
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param campaign Campaign identifier (name or id), optional.
#' @param query A search query
#' @return A participant object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.campaign(a, id = "base", interviewDesign = "Adult trauma - baseline")
#' amber.logout(a)
#' }
#' @export
amber.participant <-
  function(amber,
           code,
           study = NULL,
           interviewDesign = NULL,
           campaign = NULL,
           query = list()) {
    if (!is.null(study)) {
      studyObj <- amber.study(amber, study)
      if (!is.null(studyObj)) {
        query$study <- studyObj$`_id`
      } else {
        stop("No such study with ID or name: ", study, call. = FALSE)
      }
    }
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    if (!is.null(campaign)) {
      campaignObj <-
        amber.campaign(amber,
                       campaign,
                       study = study,
                       interviewDesign = interviewDesign)
      if (!is.null(campaignObj)) {
        query$campaign <- campaignObj$`_id`
      } else {
        stop("No such campaign with ID or name: ", campaign, call. = FALSE)
      }
    }
    query$code <- code
    res <- .get(amber, "participant", query = query)
    if (length(res$data) > 0) {
      if (length(res$data) > 1)
        warning(
          "There are more than one participant matching the criteria",
          immediate. = TRUE,
          call. = FALSE
        )
      res$data[[1]]
    } else {
      NULL
    }
  }

#' Get the interviews of one or several interview design(s).
#'
#' @title Get the interview records
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param campaign Campaign identifier (name or id), optional.
#' @param from From date (included), optional
#' @param to To date (included), optional
#' @param code Participant interview code, optional
#' @param identifier Participant identifier, optional
#' @param state State of the interview: 'initiated', 'in_progress', 'completed'
#' @param participantValid Participant is valid (logical): active and in the valid date range
#' @param query A search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param mergeUpdatedFillingDates Set the last update date as being the filling date, if filling date is empty (default is FALSE).
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find all interviews
#' amber.interviews(a)
#'
#' # Find all interviews in a range of time
#' amber.interviews(a, from = "2022-01-12 00:00", to = "2022-02-13")
#'
#' # Find all interviews for a specific participant/patient study identifier
#' amber.interviews(a, identifier = "1231")
#'
#' # Find all interviews having their identifier matching a regular expression
#' amber.interviews(a, query = list(`data._id[$search]` = "^12"))
#'
#' # Find all interviews which form data is equal to some value
#' # (will not work if the data are encrypted in the database)
#' amber.interviews(a, query = list(data.PATIENT.ORIGIN_REGION = "xyz"))
#'
#' # Find interview records of a study
#' amber.interviews(a, study = "Trauma Registry")
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.interviews <-
  function(amber,
           study = NULL,
           interviewDesign = NULL,
           campaign = NULL,
           from = NULL,
           to = NULL,
           code = NULL,
           identifier = NULL,
           state = NULL,
           participantValid = NULL,
           query = list(),
           skip = 0,
           limit = 100,
           mergeUpdatedFillingDates = FALSE,
           df = TRUE) {
    if (!is.null(study)) {
      studyObj <- amber.study(amber, study)
      if (!is.null(studyObj)) {
        query$study <- studyObj$`_id`
      } else {
        stop("No such study with ID or name: ", study, call. = FALSE)
      }
    }
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    if (!is.null(campaign)) {
      campaignObj <-
        amber.campaign(amber,
                       campaign,
                       study = study,
                       interviewDesign = interviewDesign)
      if (!is.null(campaignObj)) {
        query$campaign <- campaignObj$`_id`
      } else {
        stop("No such campaign with ID or name: ", campaign, call. = FALSE)
      }
    }
    if (!is.null(from)) {
      query$`updatedAt[$gte]` <- .formatDate(from)
    }
    if (!is.null(to)) {
      query$`updatedAt[$lte]` <- .formatDate(to)
    }
    if (!is.null(identifier)) {
      query$identifier <- identifier
    }
    if (!is.null(code)) {
      query$code <- code
    }
    if (!is.null(state)) {
      query$state <- state
    }
    if (!is.null(participantValid)) {
      query$participantValid <- ifelse(isTRUE(participantValid), 'true', 'false')
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "interview", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          code = val$code,
          identifier = val$identifier,
          interviewDesign = val$interviewDesign,
          study = val$study,
          campaign = val$campaign,
          state = val$state,
          participantValid = val$participantValid,
          data = as.character(jsonlite::toJSON(val$data, auto_unbox = TRUE)),
          steps = as.character(jsonlite::toJSON(val$steps, auto_unbox = TRUE)),
          createdBy = val$createdBy,
          createdAt = val$createdAt,
          updatedAt = val$updatedAt,
          fillingDate = val$fillingDate
        )
      })
      itws <- dplyr::bind_rows(vals)
      if (mergeUpdatedFillingDates) {
        itws <- itws %>%
          # merge updated date with explicit filling date
          mutate(fillingDate = ifelse(is.na(.data$fillingDate), .data$updatedAt, .data$fillingDate))
      }
      itws %>%
        # convert character to date type
        mutate(fillingDate = as.Date(.data$fillingDate))
    } else {
      res
    }
  }

#' Export the interview records (data and dictionary) of interview step(s).
#'
#' @title Export the interview records
#' @family studies functions
#' @param amber An Amber object
#' @param study Study identifier (name or id), optional.
#' @param interviewDesign Interview design identifier (name or id), optional.
#' @param campaign Campaign identifier (name or id), optional.
#' @param from From date (included), optional
#' @param to To date (included), optional
#' @param completed When TRUE export data from completed interviews only (default is NULL)
#' @param code Participant interview code
#' @param identifier Patient/participant study identifier
#' @param state State of the interview: 'initiated', 'in_progress', 'completed'
#' @param participantValid Participant is valid (logical): active and in the valid date range
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
#' # Extract data from all interviews
#' amber.interview_export(a)
#'
#' # Extract data from all completed interviews
#' amber.interview_export(a, completed = TRUE)
#'
#' # Extract data from all interviews in a range of time
#' amber.interview_export(a, from = "2022-01-12 00:00", to = "2022-02-13")
#'
#' # Extract data from all interviews for a specific participant/patient
#' # study identifier
#' amber.interview_export(a, identifier = "1231")
#'
#' # Extract data from all interviews having their participant study
#' # identifier matching a regular expression
#' amber.interview_export(a, query = list(`identifier[$search]` = "^12"))
#'
#' # Extract data from all interviews having their participant attributes
#' # matching a value
#' # (will not work if the data are encrypted in the database)
#' amber.interview_export(a, query = list(data.country = "fr"))
#'
#' # Export records collected with a study's interview design
#' tables <- amber.interview_export(a,
#'   study = "liftup",
#'   interviewDesign = "treocapa_lt")
#'
#' # Result contains both data and dictionary
#' tables
#'
#' # Tables are named with the <interview design name>-<form name>-<revision> pattern
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
amber.interview_export <-
  function(amber,
           study = NULL,
           interviewDesign = NULL,
           campaign = NULL,
           from = NULL,
           to = NULL,
           completed = NULL,
           code = NULL,
           identifier = NULL,
           state = NULL,
           participantValid = NULL,
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
    if (!is.null(interviewDesign)) {
      interviewDesignObj <-
        amber.interview_design(amber, interviewDesign, study = study)
      if (!is.null(interviewDesignObj)) {
        query$interviewDesign <- interviewDesignObj$`_id`
      } else {
        stop("No such interview design with ID or name: ",
             interviewDesign,
             call. = FALSE)
      }
    }
    if (!is.null(campaign)) {
      campaignObj <-
        amber.campaign(amber,
                       campaign,
                       study = study,
                       interviewDesign = interviewDesign)
      if (!is.null(campaignObj)) {
        query$campaign <- campaignObj$`_id`
      } else {
        stop("No such campaign with ID or name: ", campaign, call. = FALSE)
      }
    }
    if (!is.null(from)) {
      query$`updatedAt[$gte]` <- .formatDate(from)
    }
    if (!is.null(to)) {
      query$`updatedAt[$lte]` <- .formatDate(to)
    }
    if (isTRUE(completed)) {
      query$state <- "completed"
    }
    if (!is.null(state)) {
      query$state <- state
    }
    if (!is.null(participantValid)) {
      query$participantValid <- ifelse(isTRUE(participantValid), 'true', 'false')
    }
    if (!is.null(code)) {
      query$code <- code
    }
    if (!is.null(identifier)) {
      query$identifier <- identifier
    }
    query$`$skip` <- skip
    query$`$limit` <- limit
    res <- .get(amber, "interview-export", query = query)
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

#' Get the steps of the provided interviews.
#'
#' @title Get the steps of interview records
#' @family studies functions
#' @param interviews The data frame of interviews
#' @return A data.frame of steps
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find interview records of a study
#' itws <- amber.interviews(a, study = "Trauma Registry")
#'
#' # Get steps
#' steps <- amber.interviews_steps(itws)
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
#' @importFrom tidyjson as.tbl_json gather_array spread_all
amber.interviews_steps <-
  function(interviews) {
    if (is.null(interviews)) {
      stop("No interviews provided")
    }
    # Extract interview steps
    steps <- interviews %>%
      # exclude interviews from the test campaign
      filter(!is.na(.data$identifier)) %>%
      # select some columns: 'steps' contains collected data for each step
      select(.data$code, .data$identifier, .data$steps) %>%
      # change steps type to JSON type
      as.tbl_json(json.column = "steps") %>%
      # steps is a JSON array, then make a row per step
      gather_array %>%
      # make columns per step values (first level only)
      spread_all(recursive = F) %>%
      # select some columns
      select(.data$code, .data$identifier, .data$name, .data$state)
    steps
  }


#' Get the data of the steps with a specific name.
#'
#' @title Get the data of some interviews step
#' @family studies functions
#' @param steps The data frame of interviews steps
#' @param step_name The name of the step
#' @return A data.frame of step data
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find interview records of a study
#' itws <- amber.interviews(a, study = "Trauma Registry")
#'
#' # Get steps
#' steps <- amber.interviews_steps(itws)
#'
#' # Get data of the step with name CONSENT
#' consent_data <- amber.interviews_step_data(steps, 'CONSENT')
#'
#' # Count per agreement values (possible values are 0 or 1)
#' consent_data %>% count(AGREEMENT)
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
#' @importFrom tidyjson enter_object spread_all
amber.interviews_step_data <-
  function(steps, step_name) {
    if (is.null(steps)) {
      stop("No interviews steps provided")
    }
    data <- steps %>%
      # filter steps
      filter(.data$name == step_name) %>%
      # in the step object, use the data object (contains the answers of the participant)
      enter_object(data) %>%
      # make one column per consent data
      spread_all
    data
  }

#' Get the actions of the steps with a specific name.
#'
#' @title Get the actions of some interviews step
#' @family studies functions
#' @param steps The data frame of interviews steps
#' @param step_name The name of the step
#' @return A data.frame of step data
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#'
#' # Find interview records of a study
#' itws <- amber.interviews(a, study = "Trauma Registry")
#'
#' # Get steps
#' steps <- amber.interviews_steps(itws)
#'
#' # Get actions of the step with name QUESTIONNAIRE
#' actions <- amber.interviews_step_actions(steps, 'QUESTIONNAIRE')
#'
#' # Count the number of completed QUESTIONNAIRE steps ('complete' action type)
#' # in completed interviews ('completed' state)
#' actions %>% filter(state == 'completed', type == 'complete') %>% count()
#'
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
#' @importFrom tidyjson enter_object gather_array spread_all
amber.interviews_step_actions <-
  function(steps, step_name) {
    if (is.null(steps)) {
      stop("No interviews steps provided")
    }
    actions <- steps %>%
      # filter steps
      filter(.data$name == step_name) %>%
      # in the step object, use the actions array (contains the actions done on this step)
      enter_object(actions) %>%
      # make one row per action
      gather_array %>%
      # make one column per action's value
      # note: there is a 'user' column if the action is performed by a registered user (= not the participant)
      # note: type possible values are
      #   * 'init' (step without data),
      #   * 'pause' (step is paused),
      #   * 'complete' (step is completed),
      #   * 'invalid' (step is to be skipped (condition not satisfied)),
      #   * 'reopen' (step is forced to be reopened, by investigator)
      spread_all %>%
      # Turn action timestamp into date
      mutate(timestamp = as.POSIXct(.data$timestamp / 1000, origin = "1970-01-01"))
    actions
  }
