#' messages Class
messages <- R6Class(
  "messages",
  inherit = base_api,
  public = list(
    #' @description Create a message.
    #' @param thread_id character Required. The ID of the thread to create a message for.
    #' @param role character Required. The role of the entity that is creating the message. Currently only user is supported.
    #' @param content character Required. The content of the message.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @param ...  Additional parameters as required by the OpenAI API. For example:file_ids,metadata
    #' @return A message object.
    create=function(thread_id,role,content,...,verbosity=0){
      option <- list(...)
      option$role <- role
      option$content <- content
      result<-private$api_call("threads", paste0("/", thread_id,"/messages"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieve a message.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The message object matching the specified ID.
    retrieve=function(thread_id,message_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a message.
    #' @param thread_id character Required. The ID of the thread the message belongs to.
    #' @param message_id character Required. The ID of the message to retrieve.
    #' @param ... Additional parameters as required by the OpenAI API. For example:metadata
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified message object.
    modify=function(thread_id,message_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of messages for a given thread.
    #' @param thread_id character Required. The ID of the thread the messages belong to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of message objects.
    list=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/messages"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Deletes a message.
    #' @param thread_id character Required. The ID of the thread to which this message belongs.
    #' @param message_id character Required. The ID of the message to delete.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return Deletion status
    delete=function(thread_id,message_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/messages/",message_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
