MetaculR__questions_id <- function(MetaculR_questions, scope = NULL, days = NULL) {
  switch (scope,
    "resolved_community" = {
      unlist(lapply(MetaculR_questions, function(x)
        x$results$id[which(
          x$results$possibilities$type == "binary" &
            !is.na(x$results$resolution) &
            x$results$resolution != -1 &
            unlist(
              lapply(x$results$metaculus_prediction$full, function(z)
                ! is.null(z))
            )
        )]))
    },
    "resolved_me" = {
      unlist(lapply(MetaculR_questions, function(x)
        if (is.data.frame(x$results$my_predictions)) {
          x$results$id[which(
            x$results$possibilities$type == "binary" &
              !is.na(x$results$resolution) &
              x$results$resolution != -1 &
              !is.na(x$results$my_predictions$question) &
              unlist(
                lapply(x$results$metaculus_prediction$full, function(z)
                  ! is.null(z))
              )
          )]
        }))
    },
    "unresolved_me" = {
      unlist(lapply(MetaculR_questions, function(x)
        if (is.data.frame(x$results$my_predictions)) {
          x$results$id[which(
            x$results$possibilities$type == "binary" &
              is.na(x$results$resolution) &
              !is.na(x$results$community_prediction$full$q2) &
              !is.na(x$results$my_predictions$active)
          )]
        }))
    },
    "open_community" = {
      assertthat::assert_that(!is.null(days))

      unlist(lapply(MetaculR_questions, function(x)
        if (TRUE) {
          x$results$id[which(
            x$results$possibilities$type == "binary" &
              as.Date(x$results$close_time) >= as.Date(Sys.time()) - days &
              unlist(
                lapply(x$results$community_prediction$history, function(z)
                  nrow(z) > 0)
              )
          )]
        }))
    }
  )
}
