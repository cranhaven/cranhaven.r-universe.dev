openai_error <- R6Class(
  "openai_error",
  private = list(
    message = NULL,
    type = NULL
  ),
  public = list(
    initialize = function(message, type = "general") {
      private$message <- message
      private$type <- type
    },
    get_message = function() {
      return(private$message)
    },
    get_type = function() {
      return(private$type)
    }
  )
)
