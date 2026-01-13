#' Get the tasks
#'
#' @title Get the tasks
#' @family tasks functions
#' @param amber An Amber object
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.tasks(a, skip = 0, limit = 10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.tasks <-
  function(amber,
           query = list(),
           skip = 0,
           limit = 100,
           df = TRUE) {
    query$`$skip` <- skip
    query$`$limit` <- limit
    query$`$sort[createdAt]` <- -1
    res <- .get(amber, "task", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(res$data, function(val) {
        list(
          `_id` = val$`_id`,
          state = val$state,
          type = val$type,
          createdAt = val$createdAt,
          updatedAt = val$updatedAt
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }

#' Get the tasks logs
#'
#' @title Get the tasks logs
#' @family tasks functions
#' @param amber An Amber object
#' @param query The search query
#' @param skip Number of items to skip
#' @param limit Max number of items
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.tasks_logs(a, skip = 0, limit = 10)
#' amber.logout(a)
#' }
#' @export
#' @import dplyr
amber.tasks_logs <-
  function(amber,
           query = list(),
           skip = 0,
           limit = 100,
           df = TRUE) {
    query$`$skip` <- skip
    query$`$limit` <- limit
    query$`$sort[createdAt]` <- -1
    res <- .get(amber, "task", query = query)
    .reportListMetrics(res)

    if (df) {
      vals <- lapply(unlist(lapply(res$data, function(val) {
        lapply(val$logs, function(log) {
          list(
            `_id` = log$`_id`,
            task_id = val$`_id`,
            task_state = val$state,
            task_type = val$type,
            level = log$level,
            message = log$message,
            timestamp = log$timestamp
          )
        })
      }), recursive = FALSE), function(log) {
        list(
          `_id` = log$`_id`,
          task_id = log$`_id`,
          task_state = log$task_state,
          task_type = log$task_type,
          level = log$level,
          message = log$message,
          timestamp = log$timestamp
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }

#' Get a task by its identifier.
#'
#' @title Get a task
#' @family tasks functions
#' @param amber An Amber object
#' @param id Task's identifier
#' @return An task object as a named list
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.task(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.task <-
  function(amber,
           id) {
    if (is.null(id)) {
      stop("Task identifier is required")
    }
  .get(amber, "task", id)
  }


#' Get the logs of a task by its identifier.
#'
#' @title Get the logs of a task
#' @family tasks functions
#' @param amber An Amber object
#' @param id Task's identifier
#' @param df Return a data.frame (default is TRUE)
#' @return A data.frame (or a named list of raw results when 'df' is FALSE)
#' @examples
#' \dontrun{
#' a <- amber.login("https://amber-demo.obiba.org")
#' amber.task(a, id = "61e69a22fea2df2f3108b508")
#' amber.logout(a)
#' }
#' @export
amber.task_logs <-
  function(amber,
           id,
           df = TRUE) {
    if (is.null(id)) {
      stop("Task identifier is required")
    }
    res <- .get(amber, "task", id)
    if (df) {
      vals <- lapply(res$logs, function(val) {
        list(
          `_id` = val$`_id`,
          level = val$level,
          message = val$message,
          timestamp = val$timestamp
        )
      })
      dplyr::bind_rows(vals)
    } else {
      res
    }
  }
