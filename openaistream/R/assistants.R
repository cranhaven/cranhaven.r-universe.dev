#' assistants Class test3
assistants <- R6Class(
  "assistants",
  inherit = base_api,
  public = list(
    #' @description Create an assistant with a model and instructions.
    #' @param model character Required. ID of the model to use. You can use the List models API to see all of your available models,
    #'             or see our Model overview for descriptions of them.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:name;description;instructions;tools;file_ids;metadata
    #' @return An assistant object.
    create=function(model,...,verbosity=0){
      option <- list(...)
      option$model <- model
      result <- private$api_call("assistants", body = option,method = "POST", headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves an assistant.
    #' @param assistant_id character Required. The ID of the assistant to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The assistant object matching the specified ID.
    retrieve=function(assistant_id,verbosity=0){
      result <- private$api_call("assistants",path=paste0("/",assistant_id),method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies an assistant.
    #' @param assistant_id character Required. The ID of the assistant to modify.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:model;name;description;instructions;tools;file_ids,metadata
    #' @return The assistant object matching the specified ID.
    modify=function(assistant_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("assistants",path=paste0("/",assistant_id), body = option,method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete an assistant.
    #' @param assistant_id character Required. The ID of the assistant to delete.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status
    delete=function(assistant_id,verbosity=0){
      result <- private$api_call("assistants", paste0("/", assistant_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of assistants
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:limit;order;after;before;
    #' @return A list of assistant objects.
    list=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("assistants",query = option,method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
