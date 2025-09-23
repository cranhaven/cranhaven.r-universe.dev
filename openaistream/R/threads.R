#' threads Class
threads <- R6Class(
  "threads",
  inherit = base_api,
  public = list(
    #' @description Create a thread.
    #' @param ... Additional parameters as required by the OpenAI API.For example:messages;name;metadata
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A thread object.
    create=function(...,verbosity=0){
      option <- list(...)
      result<-private$api_call("threads",body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a thread.
    #' @param thread_id character Required. The ID of the thread to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The thread object matching the specified ID.
    retrieve=function(thread_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a thread.
    #' @param thread_id The ID of the thread to modify. Only the metadata can be modified.
    #' @param ... Additional parameters as required by the OpenAI API.For example:metadata
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified thread object matching the specified ID.
    modify=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a thread.
    #' @param thread_id character Required The ID of the thread to delete.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return Deletion status.
    delete=function(thread_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
